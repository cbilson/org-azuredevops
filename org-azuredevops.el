;;; org-azuredevops.el --- Org-mode config -*- lexical-binding: t -*-
;;; Commentary:

;; A package that provides Azure DevOps integration for `org-mode`.

;;; Code:
(require 'org)
(require 'rx)

;; -----------------------------------------------------------------------------
;; Customization
;; -----------------------------------------------------------------------------

(defcustom org-azuredevops-host
  "dev.azure.com"
  "The hostname to use for AzDevops links."
  :group 'org-link :type 'string)

(defcustom org-azuredevops-organization
  "msazure/One"
  "The org to use for AzDevops links."
  :group 'org-link :type 'string)

(defcustom org-azuredevops-default-repo
  "Azure-Compute"
  "The default repo to use for AzDevops repo related links."
  :group 'org-link :type 'string)

;;;
;;; Shared functions
;;;
(defun ado-export (type id link-fn description format)
  "Export an AzDevops work item.
Uses LINK-FN to get a hyperlink, for Work item ID, of type TYPE,
with DESCRIPTION from an Org file for FORMAT."
  (let* ((type (or type "Work Item"))
         (display-id (car (last (split-string id ":"))))
         (description (or description (concat type " #" display-id)))
         (href (funcall link-fn id)))
    (pcase format
      (`html (format "<a target=\"_blank\" href=\"%s\">%s</a>" href description))
      (`latex (format "\\href{%s}{%s}" href description))
      (`texinfo (format "@uref{%s,%s}" href description))
      (`ascii (format "%s (%s)" description href))
      (`md (format "[%s](%s)" description href))
      (_ href))))

;; -----------------------------------------------------------------------------
;; Azure-DevOps repo (source code) links.
;; -----------------------------------------------------------------------------

;; ado-repo-name matches either a bare repo name (e.g. "Some-Repo") or a
;; "org/project.../repo" style prefix (e.g. "azureconfig/Gold/Azure-Gold-Config"),
;; mirroring the org/project/repo support available on `pr:' links.
(rx-define ado-repo-name    (1+ (or letter digit ?- ?_ ?/)))
(rx-define ado-path         (1+ (or letter digit ?_ ?/ ?. ?-)))
(rx-define ado-line-num     (seq ?L (group (1+ digit))))
(rx-define ado-line-range   (seq ?L (group (1+ digit)) ?- (group (1+ digit))))

(rx-define ado-path-only             (seq bol (group ado-path) eol))
(rx-define ado-path-and-single-line  (seq bol (group ado-path) ?: ado-line-num eol))
(rx-define ado-path-and-line-range   (seq bol (group ado-path) ?: ado-line-range eol))
(rx-define ado-repo-and-path         (seq bol (group ado-repo-name) ?: (group ado-path) eol))
(rx-define ado-repo-with-single-line (seq bol (group ado-repo-name) ?: (group ado-path) ?: ado-line-num eol))
(rx-define ado-repo-with-line-range  (seq bol (group ado-repo-name) ?: (group ado-path) ?: ado-line-range eol))

(defun ado-split-repo-spec (repo-spec)
  "Split REPO-SPEC into an alist with `org' and `repo'.
REPO-SPEC can be a bare repo name like \"Some-Repo\", in which case
`org-azuredevops-organization' is used for org, or it can be a
\"org/project.../repo\" style path like
\"azureconfig/Gold/Azure-Gold-Config\", in which case everything but
the last component is the org, and the last component is the repo."
  (let ((components (split-string repo-spec "/")))
    (if (> (length components) 1)
        `((org . ,(mapconcat #'identity (butlast components) "/"))
          (repo . ,(car (last components))))
      `((org . ,org-azuredevops-organization)
        (repo . ,repo-spec)))))

(defun ado-parse-src-link (link)
  (cond
   ;; devops-src:Some-Repo:path/to/some/file.cs:L42-53
   ;; devops-src:azureconfig/Gold/Azure-Gold-Config:path/to/some/file.cs:L42-53
   ((string-match (rx ado-repo-with-line-range) link)
    (let-alist (ado-split-repo-spec (match-string 1 link))
      `((org . ,.org)
        (repo . ,.repo)
        (path . ,(match-string 2 link))
        (line-number . ,(match-string 3 link))
        (line-end . ,(match-string 4 link)))))

   ;; devops-src:path/to/some/file.cs:L42-53
   ((string-match (rx ado-path-and-line-range) link)
    `((org . ,org-azuredevops-organization)
      (repo . ,org-azuredevops-default-repo)
      (path . ,(match-string 1 link))
      (line-number . ,(match-string 2 link))
      (line-end . ,(match-string 3 link))))

   ;; devops-src:Some-Repo:path/to/some/file.cs:L42
   ;; devops-src:azureconfig/Gold/Azure-Gold-Config:path/to/some/file.cs:L42
   ((string-match (rx ado-repo-with-single-line) link)
    (let-alist (ado-split-repo-spec (match-string 1 link))
      `((org . ,.org)
        (repo . ,.repo)
        (path . ,(match-string 2 link))
        (line-number . ,(match-string 3 link)))))

   ;; devops-src:path/to/some/file.cs:L42
   ((string-match (rx ado-path-and-single-line) link)
    `((org . ,org-azuredevops-organization)
      (repo . ,org-azuredevops-default-repo)
      (path . ,(match-string 1 link))
      (line-number . ,(match-string 2 link))))

   ;; devops-src:Some-Repo:path/to/some/file.cs
   ;; devops-src:azureconfig/Gold/Azure-Gold-Config:path/to/some/file.cs
   ((string-match (rx ado-repo-and-path) link)
    (let-alist (ado-split-repo-spec (match-string 1 link))
      `((org . ,.org)
        (repo . ,.repo)
        (path . ,(match-string 2 link)))))

   ;; devops-src:path/to/some/file.cs
   ((string-match (rx ado-path-only) link)
    `((org . ,org-azuredevops-organization)
      (repo . ,org-azuredevops-default-repo)
      (path . ,(match-string 1 link))))))

(defun ado-src-escape-path (path)
  "Escape the `/' characters in PATH, with a leading `/' prepended.
Used to build a `path=' query parameter value for AzDevops source
links, e.g. \"src/some/file.cs\" becomes \"%2Fsrc%2Fsome%2Ffile.cs\"."
  (replace-regexp-in-string "/" "%2F" (concat "/" path)))

(defun ado-src-link-to-url (path)
  "Expand a Azure-Compute source code link PATH into a URL in AzDevops.
Links can be in the form of `ado-src:<path>' which links to a file
the default repo, Azure-Compute, `ado-src:<repo>:<path>', which
links to a file in repository <repo>, or
`ado-src:<org>/<project>/<repo>:<path>', which links to a file in
repository <repo> under the specified org/project."
  (let ((parsed (ado-parse-src-link path)))
    (let-alist parsed
      (cond
       (.line-number (concat "https://" org-azuredevops-host "/" .org "/_git/"
                             .repo "?path=" (ado-src-escape-path .path) "&line="
                             .line-number "&lineEnd=" (or .line-end .line-number)
                             "&lineStartColumn=0&lineEndColumn=1000"))
       (t (concat "https://" org-azuredevops-host "/" .org "/_git/"
                  .repo "?path=" (ado-src-escape-path .path)))))))

(defun ado-src-export (path description format)
  "Export an AzDevops src link PATH with DESCRIPTION to FORMAT."
  (let-alist (ado-parse-src-link path)
    (let* ((href (ado-src-link-to-url path))
           (description (or description (concat .repo ":/" .path))))
      (pcase format
        (`html (format "<a target=\"_blank\" href=\"%s\">%s</a>" href description))
        (`latex (format "\\href{%s}{%s}" href description))
        (`texinfo (format "@uref{%s,%s}" href description))
        (`ascii (format "%s (%s)" description href))
	(`md (format "[%s](%s)" description href))
        (_ href)))))

(defun ado-src-command (path)
  "Open an AzDevops src link PATH in the browser."
  (let ((url (ado-src-link-to-url path)))
    (message "Opening browser to: %s" url)
    (browse-url url)))

(org-link-set-parameters "devops-src" :follow #'ado-src-command :export #'ado-src-export)

;; -----------------------------------------------------------------------------
;; Work Item Links  <workitem-type|workitem>:[org/project:]<id>
;; -----------------------------------------------------------------------------

(defun ado-parse-workitem-link (path)
  "Parse a work item link PATH into an alist with `org' and `id'.
PATH can be a bare numeric ID like \"18469984\" or include an
org/project prefix like \"msazure/One:18469984\"."
  (let ((components (split-string path ":")))
    (if (> (length components) 1)
        `((org . ,(mapconcat #'identity (butlast components) ":"))
          (id . ,(car (last components))))
      `((org . ,org-azuredevops-organization)
        (id . ,path)))))

(defun ado-workitem-url (path &optional org host)
  "Expand a work item link PATH into a URL in AzDevops.
PATH can be a bare ID or \"org/project:id\".  Optional ORG and
HOST override the parsed/default values."
  (let* ((parsed (ado-parse-workitem-link path))
         (org (or org (cdr (assq 'org parsed))))
         (host (or host org-azuredevops-host))
         (id (cdr (assq 'id parsed))))
    (concat "https://" host "/" org "/_workitems/edit/" id)))

(defun ado-workitem-command (path &optional org host)
  "Open an AzDevops work-item link to the work item PATH in the browser."
  (browse-url (ado-workitem-url path org host)))

(defun ado-workitem-export (id description format)
  "Export an AzDevops Work Item link ID with DESCRIPTION to FORMAT."
  (ado-export "Work Item" id 'ado-workitem-url description format))

(org-link-set-parameters "workitem" :follow #'ado-workitem-command :export #'ado-workitem-export)

(defun ado-epic-export (id description format)
  "Export an AzDevops Epic link ID with DESCRIPTION to FORMAT."
  (ado-export "Epic" id 'ado-workitem-url description format))

(org-link-set-parameters "epic" :follow #'ado-workitem-command :export #'ado-epic-export)

(defun ado-feature-export (id description format)
  "Export an AzDevops Feature link ID with DESCRIPTION to FORMAT."
  (ado-export "Feature" id 'ado-workitem-url description format))

(org-link-set-parameters "feature" :follow #'ado-workitem-command :export #'ado-feature-export)

(defun ado-pbi-export (id description format)
  "Export an AzDevops PBI link ID with DESCRIPTION to FORMAT."
  (ado-export "PBI" id 'ado-workitem-url description format))

(org-link-set-parameters "pbi" :follow #'ado-workitem-command :export #'ado-pbi-export)

(defun ado-task-export (id description format)
  "Export an AzDevops Task link ID with DESCRIPTION to FORMAT."
  (ado-export "Task" id 'ado-workitem-url description format))

(org-link-set-parameters "task" :follow #'ado-workitem-command :export #'ado-task-export)

(defun ado-bug-export (id description format)
  "Export an AzDevops Bug link ID with DESCRIPTION to FORMAT."
  (ado-export "Bug" id 'ado-workitem-url description format))

(org-link-set-parameters "bug" :follow #'ado-workitem-command :export #'ado-bug-export)

;; -----------------------------------------------------------------------------
;; Artifact and other link types.
;; -----------------------------------------------------------------------------

;; Build links
(defun ado-build-link-to-url (path)
  "Expand a build link PATH into a URL in AzDevops."
  (concat "https://" org-azuredevops-host "/" org-azuredevops-organization "/_build/results?buildId=" path))

(defun ado-build-command (path)
  "Open an AzDevops build link to the work item PATH in the browser."
  (browse-url (ado-build-link-to-url path)))

(defun ado-build-export (id description format)
  "Export an AzDevops Build link ID with DESCRIPTION to FORMAT."
  (ado-export "Build" id 'ado-build-link-to-url description format))

(org-link-set-parameters "build" :follow #'ado-build-command :export #'ado-build-export)

;; Pull Request links
(defun ado-pr-link-to-url (path)
  "Expand a Azure-Compute PR link PATH into a URL in AzDevops.
Links can be in the form of:
- `pr:<number>` - PR in default org/repo
- `pr:<repo>/<number>` - PR in default org, specified repo
- `pr:<org>/<project>/<repo>/<number>` - PR in specified org/project/repo"
  (let* ((components (split-string path "/"))
         (num-components (length components))
         (org (cond
               ;; 3+ components: org/project.../repo/number
               ;; Everything except last 2 components is org/project
               ((>= num-components 3)
                (mapconcat 'identity (butlast components 2) "/"))
               ;; 1 component (repo/number) or 0 (just number): use default org
               (t org-azuredevops-organization)))
         (repo (cond
                ;; 3+ components: second-to-last is repo
                ((>= num-components 3)
                 (nth (- num-components 2) components))
                ;; 2 components: first is repo
                ((= num-components 2)
                 (car components))
                ;; 1 component (just number): use default repo
                (t org-azuredevops-default-repo)))
         (id (cond
              ;; 2+ components: last component is PR number
              ((>= num-components 2)
               (car (last components)))
              ;; 1 component: entire path is PR number
              (t path))))
    (concat "https://" org-azuredevops-host "/" org "/_git/" repo "/pullrequest/" id)))

(defun ado-pr-command (id)
  "Open an AzDevops PR link to the PR ID in the browser."
  (browse-url (ado-pr-link-to-url id)))

(defun ado-pr-export (id description format)
  "Export an AzDevops PR link ID with DESCRIPTION to FORMAT."
  (ado-export "Pull Request" id 'ado-pr-link-to-url description format))

(org-link-set-parameters "pr" :follow #'ado-pr-command :export #'ado-pr-export)

;; Release links
(defun ado-release-command (path)
  "Open an AzDevops release link to the work item PATH in the browser."
  (browse-url (ado-release-link-to-url path)))

(defun ado-release-link-to-url (path)
  "Expand a release link PATH into a URL in AzDevops."
  (concat "https://" org-azuredevops-host "/" org-azuredevops-organization
          "/_releaseProgress?_a=release-pipeline-progress&releaseId=" path))

(defun ado-release-export (id description format)
  "Export an AzDevops Release link ID with DESCRIPTION to FORMAT."
  (ado-export "Release" id 'ado-release-link-to-url description format))

(org-link-set-parameters "release" :follow #'ado-release-command :export #'ado-release-export)

;;; -----------------------------------------------------------------------------
;;; Public Functions
;;; -----------------------------------------------------------------------------
(defun org-azuredevops-pullrequest-title-to-link (title)
  "Given an AzureDevOps Pull Request TITLE (the 'copy' button to the
right of the title on the pull request view), create a pr: link,
preserving the title. Used in pull-request-capture-template.org" 
  (if (string-match "^Pull Request \\([[:digit:]]+\\): \\(.*\\)$" title)
      (concat "pr:" (match-string 1 title) ": " (match-string 2 title))
    title))

;;; -----------------------------------------------------------------------------

(provide 'org-azuredevops)
;;; org-azuredevops.el ends here

;; Local Variables:
;; read-symbol-shorthands: (("ado-" . "org-azuredevops--"))
;; End:
