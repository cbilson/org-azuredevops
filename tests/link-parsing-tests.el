;;; tests for Azure DevOps link parsing  -*- lexical-binding: t -*-

(require 'buttercup)
(require 'org-azuredevops)

;; convenience for running tests
(when nil
  (defun -run-tests ()
    (interactive)
    (let* ((home (file-name-as-directory (getenv "HOME")))
           (project (file-name-concat home ".emacs.d" "site-lisp" "org-azuredevops"))
           (eldev (file-name-concat home ".local" "bin" "eldev.bat --debug")))
      (compile (format "cd %s && %s test" project eldev))))

  (global-set-key [f8] #'-run-tests))

(defun -extract (expr str group)
  (when (string-match expr str)
    (match-string group str)))

(describe
 "grammar for devops-src: path only links"

 (describe
  "- path only links"
  (let ((expr (rx ado-path-only)))
    (it "- match a path"
        (expect "src/Foo/Bar.cs" :to-match expr))
    (it "- capture the path"
        (expect (-extract expr "src/Foo/Bar.cs" 1) :to-equal "src/Foo/Bar.cs"))))

 (describe
  "- repo:path links"
  (let ((expr (rx ado-repo-and-path)))
    (it "- match a repo:path"
        (expect "Some-Repo:some/path" :to-match expr))
    (it "- ignores simpler link types"
        (expect "some/path" :not :to-match expr))
    (it "- captures the repo"
        (expect (-extract expr "Some-Repo:some/path" 1) :to-equal "Some-Repo"))
    (it "- captures the path"
        (expect (-extract expr "Some-Repo:some/path" 2) :to-equal "some/path"))))

 (describe
  "- repo:path:Ln links"
  (let ((expr (rx ado-repo-with-single-line)))
    (it "- match a repo:path:Ln link"
        (expect "Some-Repo:some/path:L42" :to-match expr))
    (it "- ignores simpler link types"
        (expect "some/path" :not :to-match expr)
        (expect "Some-Repo:some/path" :not :to-match expr))
    (it "- captures the link comonents"
        (expect (-extract expr "Some-Repo:some/path:L42" 1) :to-equal "Some-Repo")
        (expect (-extract expr "Some-Repo:some/path:L42" 2) :to-equal "some/path")
        (expect (-extract expr "Some-Repo:some-path:L42" 3) :to-equal "42")))))

(describe
 "- repo:path:Ln-m links"
 (let ((expr (rx ado-repo-with-line-range)))
   (it "- match a repo:path:Ln link"
       (expect "Some-Repo:some/path:L42-53" :to-match expr))
   (it "- ignores simpler link types"
       (expect "some/path" :not :to-match expr)
       (expect "Some-Repo:some/path" :not :to-match expr)
       (expect "Some-Repo:some/path:L42" :not :to-match expr))
   (it "- captures the link comonents"
       (expect (-extract expr "Some-Repo:some/path:L42-53" 1) :to-equal "Some-Repo")
       (expect (-extract expr "Some-Repo:some/path:L42-53" 2) :to-equal "some/path")
       (expect (-extract expr "Some-Repo:some-path:L42-53" 3) :to-equal "42"))))

(describe
 "syntax tree"
 (it "- parse path-only link"
     (let-alist (ado-parse-src-link "some/path/foo.cs")
       (expect .path :to-equal "some/path/foo.cs")
       (expect .repo :to-equal org-azuredevops-default-repo)
       (expect .line-number :to-be nil)
       (expect .line-end :to-equal nil)))
 (it "- parse repo:path links"
     (let-alist (ado-parse-src-link "Some-Repo:some/path/foo.cs")
       (expect .path :to-equal "some/path/foo.cs")
       (expect .repo :to-equal "Some-Repo")
       (expect .line-number :to-be nil)
       (expect .line-end :to-equal nil)))
 (it "- parse repo:path:Ln links"
     (let-alist (ado-parse-src-link "Some-Repo:some/path/foo.cs:L42")
       (expect .path :to-equal "some/path/foo.cs")
       (expect .repo :to-equal "Some-Repo")
       (expect .line-number :to-equal "42")
       (expect .line-end :to-equal nil)))
 (it "- parse repo:path:Ln-m links"
     (let-alist (ado-parse-src-link "Some-Repo:some/path/foo.cs:L42-53")
       (expect .path :to-equal "some/path/foo.cs")
       (expect .repo :to-equal "Some-Repo")
       (expect .line-number :to-equal "42")
       (expect .line-end :to-equal "53"))))

(describe
 "generating urls"
 (let ((prefix "https://dev.azure.com/msazure/One/_git/"))
   (expect (ado-src-link-to-url "some/path/foo.cs")
           :to-equal (concat prefix "Azure-Compute?path=/some/path/foo.cs"))
   (expect (ado-src-link-to-url "Some-Repo:some/path/foo.cs")
           :to-equal (concat prefix "Some-Repo?path=/some/path/foo.cs"))
   (expect (ado-src-link-to-url "Some-Repo:some/path/foo.cs:L23")
           :to-equal
           (concat prefix "Some-Repo?path=/some/path/foo.cs&line=23&lineEnd=23&lineStartColumn=0&lineEndColumn=1000"))
   (expect (ado-src-link-to-url "Some-Repo:some/path/foo.cs:L23-42")
           :to-equal
           (concat prefix "Some-Repo?path=/some/path/foo.cs&line=23&lineEnd=42&lineStartColumn=0&lineEndColumn=1000"))))

(xdescribe
 "exporting HTML src links"
 (expect (ado-src-export "some/path/foo.cs" nil 'html)
         :to-match
         (concat "<a target=\"_blank\""
                 "href=\"https://dev.azure.com/msazure/One/_git/Azure-Compute?path=/some/path/foo.cs\">")))

(xdescribe
 "exporting HTML src links"
 (expect (ado-src-export "some/path/foo.cs" nil 'html)
         :to-equal
         (concat "<a target=\"_blank\" "
                 "href=\"https://dev.azure.com/msazure/One/_git/Azure-Compute?path=/some/path/foo.cs\">"
                 "Azure-Compute:/some/path/foo.cs</a>"))
 (expect (ado-src-export "Some-Repo:some/path/foo.cs" nil 'html)
         :to-equal
         (concat "<a target=\"_blank\" "
                 "href=\"https://dev.azure.com/msazure/One/_git/Some-Repo?path=/some/path/foo.cs\">"
                 "Azure-Compute:/some/path/foo.cs</a>"))
 (expect (ado-src-export "Some-Repo:some/path/foo.cs:L42" nil 'html)
         :to-equal
         (concat "<a target=\"_blank\" "
                 "href=\"https://dev.azure.com/msazure/One/_git/Some-Repo?path=/some/path/foo.cs"
                 "&line=42&lineEnd=42&lineStartColumn=0&lineEndColumn=1000\">"
                 "Azure-Compute:/some/path/foo.cs</a>"))
 (expect (ado-src-export "Some-Repo:some/path/foo.cs:L42-53" nil 'html)
         :to-equal
         (concat "<a target=\"_blank\" "
                 "href=\"https://dev.azure.com/msazure/One/_git/Some-Repo?path=/some/path/foo.cs"
                 "&line=42&lineEnd=53&lineStartColumn=0&lineEndColumn=1000\">"
                 "Azure-Compute:/some/path/foo.cs</a>")))

;; Local Variables:
;; read-symbol-shorthands: (("ado-" . "org-azuredevops--"))
;; End:
