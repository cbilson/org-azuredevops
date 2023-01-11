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

;; -----------------------------------------------------------------------------
;; Azure-DevOps repo (source code) links.
;; -----------------------------------------------------------------------------

(rx-define ado-repo-name    (1+ (or letter digit ?- ?_)))
(rx-define ado-path         (1+ (or letter digit ?_ ?/ ?. ?-)))
(rx-define ado-line-num     (seq ?L (group (1+ digit))))
(rx-define ado-line-range   (seq ?L (group (1+ digit)) ?- (group (1+ digit))))

(rx-define ado-path-only             (seq bol (group ado-path) eol))
(rx-define ado-path-and-single-line  (seq bol (group ado-path) ?: ado-line-num eol))
(rx-define ado-path-and-line-range   (seq bol (group ado-path) ?: ado-line-range eol))
(rx-define ado-repo-and-path         (seq bol (group ado-repo-name) ?: (group ado-path) eol))
(rx-define ado-repo-with-single-line (seq bol (group ado-repo-name) ?: (group ado-path) ?: ado-line-num eol))
(rx-define ado-repo-with-line-range  (seq bol (group ado-repo-name) ?: (group ado-path) ?: ado-line-range eol))

(defun ado-parse-src-link (link)
  (cond
   ;; devops-src:Some-Repo:path/to/some/file.cs:L42-53
   ((string-match (rx ado-repo-with-line-range) link)
    `((repo . ,(match-string 1 link))
      (path . ,(match-string 2 link))
      (line-number . ,(match-string 3 link))
      (line-end . ,(match-string 4 link))))

   ;; devops-src:path/to/some/file.cs:L42-53
   ((string-match (rx ado-path-and-line-range) link)
    `((repo . ,org-azuredevops-default-repo)
      (path . ,(match-string 1 link))
      (line-number . ,(match-string 2 link))
      (line-end . ,(match-string 3 link))))

   ;; devops-src:Some-Repo:path/to/some/file.cs:L42
   ((string-match (rx ado-repo-with-single-line) link)
    `((repo . ,(match-string 1 link))
      (path . ,(match-string 2 link))
      (line-number . ,(match-string 3 link))))

   ;; devops-src:path/to/some/file.cs:L42
   ((string-match (rx ado-path-and-single-line) link)
    `((repo . ,org-azuredevops-default-repo)
      (path . ,(match-string 1 link))
      (line-number . ,(match-string 2 link))))

   ;; devops-src:Some-Repo:path/to/some/file.cs
   ((string-match (rx ado-repo-and-path) link)
    `((repo . ,(match-string 1 link))
      (path . ,(match-string 2 link))))

   ;; devops-src:path/to/some/file.cs
   ((string-match (rx ado-path-only) link)
    `((repo . ,org-azuredevops-default-repo)
      (path . ,(match-string 1 link))))))

(defun ado-src-link-to-url (path)
  "Expand a Azure-Compute source code link PATH into a URL in AzDevops.
Links can be in the form of `ado-src:<path>` which links to
a file the default repo, Azure-Compute, or `ado-src:<repo>:<path>`,
which links to a file in repository <repo>."
  (let ((parsed (ado-parse-src-link path)))
    (let-alist parsed
      (cond
       (.line-number (concat "https://" org-azuredevops-host "/" org-azuredevops-organization "/_git/"
                             .repo "?path=/" .path "&line="
                             .line-number "&lineEnd=" (or .line-end .line-number)
                             "&lineStartColumn=0&lineEndColumn=1000"))
       (t (concat "https://" org-azuredevops-host "/" org-azuredevops-organization "/_git/"
                  .repo "?path=/" .path))))))

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
        (_ href)))))

(defun ado-src-command (path)
  "Open an AzDevops src link PATH in the browser."
  (let ((url (ado-src-link-to-url path)))
    (message "Opening browser to: %s" url)
    (browse-url url)))

(org-link-set-parameters "devops-src" :follow #'ado-src-command :export #'ado-src-export)

;; -----------------------------------------------------------------------------
;; Work Item Links  <workitem-type|workitem>:<id>
;; -----------------------------------------------------------------------------

(defun ado-workitem-url (path &optional org host)
  "Expand a work item link PATH into a URL in AzDevops."
  (let ((org (or org org-azuredevops-organization))
        (host (or org-azuredevops-host)))
    (concat "https://" host "/" org "/_workitems/edit/" path)))

(defun org-azdevops-workitem-command (path &optional org host)
  "Open an AzDevops work-item link to the work item PATH in the browser."
  (browse-url (ado-workitem-url path org host)))

(defun org-azdevops-workitem-export (id description format)
  "Export an AzDevops Work Item link ID with DESCRIPTION to FORMAT."
  (ado-export "Work Item" id 'ado-workitem-url description format))

(org-link-set-parameters "workitem" :follow #'org-azdevops-workitem-command :export #'org-azdevops-workitem-export)

(defun org-azdevops-epic-export (id description format)
  "Export an AzDevops Epic link ID with DESCRIPTION to FORMAT."
  (ado-export "Epic" id 'ado-workitem-url description format))

(org-link-set-parameters "epic" :follow #'org-azdevops-workitem-command :export #'org-azdevops-epic-export)

(defun org-azdevops-feature-export (id description format)
  "Export an AzDevops Feature link ID with DESCRIPTION to FORMAT."
  (ado-export "Feature" id 'ado-workitem-url description format))

(org-link-set-parameters "feature" :follow #'org-azdevops-workitem-command :export #'org-azdevops-feature-export)

(defun org-azdevops-pbi-export (id description format)
  "Export an AzDevops PBI link ID with DESCRIPTION to FORMAT."
  (ado-export "PBI" id 'ado-workitem-url description format))

(org-link-set-parameters "pbi" :follow #'org-azdevops-workitem-command :export #'org-azdevops-pbi-export)

(defun org-azdevops-task-export (id description format)
  "Export an AzDevops Task link ID with DESCRIPTION to FORMAT."
  (ado-export "Task" id 'ado-workitem-url description format))

(org-link-set-parameters "task" :follow #'org-azdevops-workitem-command :export #'org-azdevops-task-export)

(defun org-azdevops-bug-export (id description format)
  "Export an AzDevops Bug link ID with DESCRIPTION to FORMAT."
  (ado-export "Bug" id 'ado-workitem-url description format))

(org-link-set-parameters "bug" :follow #'org-azdevops-workitem-command :export #'org-azdevops-bug-export)

;; -----------------------------------------------------------------------------
;; Artifact and other link types.
;; -----------------------------------------------------------------------------

;; Build links
(defun org-azdevops-build-link-to-url (path)
  "Expand a build link PATH into a URL in AzDevops."
  (concat "https://" org-azuredevops-host "/" org-azuredevops-organization "/_build/results?buildId=" path))

(defun org-azdevops-build-command (path)
  "Open an AzDevops build link to the work item PATH in the browser."
  (browse-url (org-azdevops-build-link-to-url path)))

(defun org-azdevops-build-export (id description format)
  "Export an AzDevops Build link ID with DESCRIPTION to FORMAT."
  (ado-export "Build" id 'org-azdevops-build-link-to-url description format))

(org-link-set-parameters "build" :follow #'org-azdevops-build-command :export #'org-azdevops-build-export)

;; Pull Request links
(defun org-azdevops-pr-link-to-url (path)
  "Expand a Azure-Compute PR link PATH into a URL in AzDevops.
Links can be in the form of `pr:<number>` which links to
PR #<number> in the default repo, or `pr:<repo>/<number>`,
which links to PR #<number> in repository <repo>."
  (let* ((components
          (if (string-match (rx line-start
                                (group (one-or-more (in "A-Za-z0-9-_")))
                                "/"
                                (group (one-or-more digit))
                                line-end)
                            path)
              (list (match-string 1 path) (match-string 2 path))
            (list "Azure-Compute" path)))
         (repo (car components))
         (id (nth 1 components)))
    (concat "https://" org-azuredevops-host "/" org-azuredevops-organization "/_git/" repo "/pullrequest/" id)))

(defun org-azdevops-pr-command (id)
  "Open an AzDevops PR link to the PR ID in the browser."
  (browse-url (org-azdevops-pr-link-to-url id)))

(defun org-azdevops-pr-export (id description format)
  "Export an AzDevops PR link ID with DESCRIPTION to FORMAT."
  (ado-export "Pull Request" id 'org-azdevops-pr-link-to-url description format))

(org-link-set-parameters "pr" :follow #'org-azdevops-pr-command :export #'org-azdevops-pr-export)

(defun org-azdevops-pullrequest-title-to-link (title)
  "Given an AzureDevOps Pull Request TITLE (the 'copy' button to the right of the title on the
pull request view), create a pr: link, preserving the title."
  (if (string-match "^Pull Request \\([[:digit:]]+\\): \\(.*\\)$" title)
      (concat "pr:" (match-string 1 title) ": " (match-string 2 title))
    title))

;; Release links
(defun org-azdevops-release-command (path)
  "Open an AzDevops release link to the work item PATH in the browser."
  (browse-url (org-azdevops-release-link-to-url path)))

(defun org-azdevops-release-link-to-url (path)
  "Expand a release link PATH into a URL in AzDevops."
  (concat "https://" org-azuredevops-host "/" org-azuredevops-organization
          "/_releaseProgress?_a=release-pipeline-progress&releaseId=" path))

(defun org-azdevops-release-export (id description format)
  "Export an AzDevops Release link ID with DESCRIPTION to FORMAT."
  (ado-export "Release" id 'org-azdevops-release-link-to-url description format))

(org-link-set-parameters "release" :follow #'org-azdevops-release-command :export #'org-azdevops-release-export)

;; -----------------------------------------------------------------------------
;; Public Functions
;; -----------------------------------------------------------------------------

(defun org-azuredevops-export (type id link-fn description format)
  "Export an AzDevops work item.
Uses LINK-FN to get a hyperlink, for Work item ID, of type TYPE,
with DESCRIPTION from an Org file for FORMAT."
  (let* ((type (or type "Work Item"))
         (description (or description (concat type " #" id)))
         (href (funcall link-fn id)))
    (pcase format
      (`html (format "<a target=\"_blank\" href=\"%s\">%s</a>" href description))
      (`latex (format "\\href{%s}{%s}" href description))
      (`texinfo (format "@uref{%s,%s}" href description))
      (`ascii (format "%s (%s)" description href))
      (_ href))))

(provide 'org-azuredevops)
;;; org-azuredevops.el ends here

;; Local Variables:
;; read-symbol-shorthands: (("ado-" . "org-azuredevops--"))
;; End:
