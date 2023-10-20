;;; tests for Azure DevOps source code links  -*- lexical-binding: t -*-
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
 "link grammar"
 (describe
  "path only links"
  (let ((expr (rx ado-path-only)))
    (it "matches"
        (expect "src/Foo/Bar.cs" :to-match expr))
    (it "captures components"
        (expect (-extract expr "src/Foo/Bar.cs" 1) :to-equal "src/Foo/Bar.cs"))))

 (describe
  "repo:path links"
  (let ((expr (rx ado-repo-and-path)))
    (it "matches"
        (expect "Some-Repo:some/path" :to-match expr))
    (it "ignores simpler link types"
        (expect "some/path" :not :to-match expr))
    (it "captures components"
        (expect (-extract expr "Some-Repo:some/path" 1) :to-equal "Some-Repo")
        (expect (-extract expr "Some-Repo:some/path" 2) :to-equal "some/path"))))

 (describe
  "path:Ln links"
  (let ((expr (rx ado-path-and-single-line)))
    (it "matches"
        (expect "src/Foo/Bar.cs:L42" :to-match expr))
    (it "ignores simpler link types"
        (expect "some/path" :not :to-match expr))
    (it "captures components"
        (expect (-extract expr "src/Foo/Bar.cs:L42" 1) :to-equal "src/Foo/Bar.cs")
        (expect (-extract expr "src/Foo/Bar.cs:L42" 2) :to-equal "42"))))

 (describe
  "repo:path:Ln links"
  (let ((expr (rx ado-repo-with-single-line)))
    (it "matches"
        (expect "Some-Repo:some/path:L42" :to-match expr))
    (it "ignores simpler link types"
        (expect "some/path" :not :to-match expr)
        (expect "Some-Repo:some/path" :not :to-match expr))
    (it "captures components"
        (expect (-extract expr "Some-Repo:some/path:L42" 1) :to-equal "Some-Repo")
        (expect (-extract expr "Some-Repo:some/path:L42" 2) :to-equal "some/path")
        (expect (-extract expr "Some-Repo:some-path:L42" 3) :to-equal "42"))))

 (describe
  "path:Ln-m links"
  (let ((expr (rx ado-path-and-line-range)))
    (it "matches"
        (expect "some/path:L42-53" :to-match expr))
    (it "ignores simpler link types"
        (expect "some/path" :not :to-match expr)
        (expect "some/path:L42" :not :to-match expr)
        (expect "Some-Repo:some/path" :not :to-match expr))
    (it "captures components"
        (expect (-extract expr "some/path:L42-53" 1) :to-equal "some/path")
        (expect (-extract expr "some-path:L42-53" 2) :to-equal "42")
        (expect (-extract expr "some-path:L42-53" 3) :to-equal "53"))))

 (describe
  "repo:path:Ln-m links"
  (let ((expr (rx ado-repo-with-line-range)))
    (it "matches"
        (expect "Some-Repo:some/path:L42-53" :to-match expr))
    (it "ignores simpler link types"
        (expect "some/path" :not :to-match expr)
        (expect "some/path:L42" :not :to-match expr)
        (expect "some/path:L42-53" :not :to-match expr)
        (expect "Some-Repo:some/path" :not :to-match expr)
        (expect "Some-Repo:some/path:L42" :not :to-match expr))
    (it "captures components"
        (expect (-extract expr "Some-Repo:some/path:L42-53" 1) :to-equal "Some-Repo")
        (expect (-extract expr "Some-Repo:some/path:L42-53" 2) :to-equal "some/path")
        (expect (-extract expr "Some-Repo:some-path:L42-53" 3) :to-equal "42")
        (expect (-extract expr "Some-Repo:some-path:L42-53" 4) :to-equal "53")))))

(describe
 "syntax tree"
 (it "parse path-only link"
     (let-alist (ado-parse-src-link "some/path/foo.cs")
       (expect .repo :to-equal org-azuredevops-default-repo)
       (expect .path :to-equal "some/path/foo.cs")
       (expect .line-number :to-be nil)
       (expect .line-end :to-equal nil)))
 (it "parse repo:path links"
     (let-alist (ado-parse-src-link "Some-Repo:some/path/foo.cs")
       (expect .repo :to-equal "Some-Repo")
       (expect .path :to-equal "some/path/foo.cs")
       (expect .line-number :to-be nil)
       (expect .line-end :to-equal nil)))
 (it "parse path:Ln links"
     (let-alist (ado-parse-src-link "some/path/foo.cs:L42")
       (expect .repo :to-equal org-azuredevops-default-repo)
       (expect .path :to-equal "some/path/foo.cs")
       (expect .line-number :to-equal "42")
       (expect .line-end :to-equal nil)))
 (it "parse repo:path:Ln links"
     (let-alist (ado-parse-src-link "Some-Repo:some/path/foo.cs:L42")
       (expect .repo :to-equal "Some-Repo")
       (expect .path :to-equal "some/path/foo.cs")
       (expect .line-number :to-equal "42")
       (expect .line-end :to-equal nil)))
 (it "parse path:Ln-m links"
     (let-alist (ado-parse-src-link "some/path/foo.cs:L42-53")
       (expect .repo :to-equal org-azuredevops-default-repo)
       (expect .path :to-equal "some/path/foo.cs")
       (expect .line-number :to-equal "42")
       (expect .line-end :to-equal "53")))
 (it "parse repo:path:Ln-m links"
     (let-alist (ado-parse-src-link "Some-Repo:some/path/foo.cs:L42-53")
       (expect .repo :to-equal "Some-Repo")
       (expect .path :to-equal "some/path/foo.cs")
       (expect .line-number :to-equal "42")
       (expect .line-end :to-equal "53"))))

(describe
 "generating urls"
 (let ((prefix "https://dev.azure.com/msazure/One/_git/"))
   (it "generates path-only links"
       (expect (ado-src-link-to-url "some/path/foo.cs")
               :to-equal
               (concat prefix "Azure-Compute?path=/some/path/foo.cs")))
   (it "generates repo:path links"
       (expect (ado-src-link-to-url "Some-Repo:some/path/foo.cs")
               :to-equal
               (concat prefix "Some-Repo?path=/some/path/foo.cs")))
   (it "generates path:Ln links"
       (expect (ado-src-link-to-url "some/path/foo.cs:L23")
               :to-equal
               (concat prefix "Azure-Compute?path=/some/path/foo.cs&line=23&lineEnd=23&lineStartColumn=0&lineEndColumn=1000")))
   (it "generates repo:path:Ln links"
       (expect (ado-src-link-to-url "Some-Repo:some/path/foo.cs:L23")
               :to-equal
               (concat prefix "Some-Repo?path=/some/path/foo.cs&line=23&lineEnd=23&lineStartColumn=0&lineEndColumn=1000")))
   (it "generates path:Ln-m links"
       (expect (ado-src-link-to-url "some/path/foo.cs:L23-42")
               :to-equal
               (concat prefix "Azure-Compute?path=/some/path/foo.cs&line=23&lineEnd=42&lineStartColumn=0&lineEndColumn=1000")))
   (it "generates repo:path:Ln-m links"
       (expect (ado-src-link-to-url "Some-Repo:some/path/foo.cs:L23-42")
               :to-equal
               (concat prefix "Some-Repo?path=/some/path/foo.cs&line=23&lineEnd=42&lineStartColumn=0&lineEndColumn=1000")))))

(describe
 "exporting HTML src links"
 (it "for path only links"
     (expect (ado-src-export "some/path/foo.cs" nil 'html)
             :to-equal
             (concat "<a target=\"_blank\" "
                     "href=\"https://dev.azure.com/msazure/One/_git/Azure-Compute?path=/some/path/foo.cs\">"
                     "Azure-Compute:/some/path/foo.cs</a>")))
 (it "for repo:path links"
     (expect (ado-src-export "Some-Repo:some/path/foo.cs" nil 'html)
             :to-equal
             (concat "<a target=\"_blank\" "
                     "href=\"https://dev.azure.com/msazure/One/_git/Some-Repo?path=/some/path/foo.cs\">"
                     "Some-Repo:/some/path/foo.cs</a>")))
 (it "for repo:path:Ln links"
     (expect (ado-src-export "Some-Repo:some/path/foo.cs:L42" nil 'html)
             :to-equal
             (concat "<a target=\"_blank\" "
                     "href=\"https://dev.azure.com/msazure/One/_git/Some-Repo?path=/some/path/foo.cs"
                     "&line=42&lineEnd=42&lineStartColumn=0&lineEndColumn=1000\">"
                     "Some-Repo:/some/path/foo.cs</a>")))
 (it "for repo:path:Ln-m links"
     (expect (ado-src-export "Some-Repo:some/path/foo.cs:L42-53" nil 'html)
             :to-equal
             (concat "<a target=\"_blank\" "
                     "href=\"https://dev.azure.com/msazure/One/_git/Some-Repo?path=/some/path/foo.cs"
                     "&line=42&lineEnd=53&lineStartColumn=0&lineEndColumn=1000\">"
                     "Some-Repo:/some/path/foo.cs</a>"))))

(describe
 "exporting latex src links"
 (it "for path only links"
     (expect (ado-src-export "some/path/foo.cs" nil 'latex)
             :to-equal
	     "\\href{https://dev.azure.com/msazure/One/_git/Azure-Compute?path=/some/path/foo.cs}{Azure-Compute:/some/path/foo.cs}"))
 (it "for repo:path links"
     (expect (ado-src-export "Some-Repo:some/path/foo.cs" nil 'latex)
             :to-equal
	     "\\href{https://dev.azure.com/msazure/One/_git/Some-Repo?path=/some/path/foo.cs}{Some-Repo:/some/path/foo.cs}"))
 (it "for repo:path:Ln links"
     (expect (ado-src-export "Some-Repo:some/path/foo.cs:L42" nil 'latex)
             :to-equal
	     "\\href{https://dev.azure.com/msazure/One/_git/Some-Repo?path=/some/path/foo.cs&line=42&lineEnd=42&lineStartColumn=0&lineEndColumn=1000}{Some-Repo:/some/path/foo.cs}"))
 (it "for repo:path:Ln-m links"
     (expect (ado-src-export "Some-Repo:some/path/foo.cs:L42-53" nil 'latex)
             :to-equal
	     "\\href{https://dev.azure.com/msazure/One/_git/Some-Repo?path=/some/path/foo.cs&line=42&lineEnd=53&lineStartColumn=0&lineEndColumn=1000}{Some-Repo:/some/path/foo.cs}")))

(describe
 "exporting texinfo src links"
 (it "for path only links"
     (expect (ado-src-export "some/path/foo.cs" nil 'texinfo)
             :to-equal
	     "@uref{https://dev.azure.com/msazure/One/_git/Azure-Compute?path=/some/path/foo.cs,Azure-Compute:/some/path/foo.cs}"))
 (it "for repo:path links"
     (expect (ado-src-export "Some-Repo:some/path/foo.cs" nil 'texinfo)
             :to-equal
	     "@uref{https://dev.azure.com/msazure/One/_git/Some-Repo?path=/some/path/foo.cs,Some-Repo:/some/path/foo.cs}"))
 (it "for repo:path:Ln links"
     (expect (ado-src-export "Some-Repo:some/path/foo.cs:L42" nil 'texinfo)
             :to-equal
	     "@uref{https://dev.azure.com/msazure/One/_git/Some-Repo?path=/some/path/foo.cs&line=42&lineEnd=42&lineStartColumn=0&lineEndColumn=1000,Some-Repo:/some/path/foo.cs}"))
 (it "for repo:path:Ln-m links"
     (expect (ado-src-export "Some-Repo:some/path/foo.cs:L42-53" nil 'texinfo)
             :to-equal
	     "@uref{https://dev.azure.com/msazure/One/_git/Some-Repo?path=/some/path/foo.cs&line=42&lineEnd=53&lineStartColumn=0&lineEndColumn=1000,Some-Repo:/some/path/foo.cs}")))

(describe
 "exporting texinfo src links"
 (it "for path only links"
     (expect (ado-src-export "some/path/foo.cs" nil 'texinfo)
             :to-equal
	     "@uref{https://dev.azure.com/msazure/One/_git/Azure-Compute?path=/some/path/foo.cs,Azure-Compute:/some/path/foo.cs}"))
 (it "for repo:path links"
     (expect (ado-src-export "Some-Repo:some/path/foo.cs" nil 'texinfo)
             :to-equal
	     "@uref{https://dev.azure.com/msazure/One/_git/Some-Repo?path=/some/path/foo.cs,Some-Repo:/some/path/foo.cs}"))
 (it "for repo:path:Ln links"
     (expect (ado-src-export "Some-Repo:some/path/foo.cs:L42" nil 'texinfo)
             :to-equal
	     "@uref{https://dev.azure.com/msazure/One/_git/Some-Repo?path=/some/path/foo.cs&line=42&lineEnd=42&lineStartColumn=0&lineEndColumn=1000,Some-Repo:/some/path/foo.cs}"))
 (it "for repo:path:Ln-m links"
     (expect (ado-src-export "Some-Repo:some/path/foo.cs:L42-53" nil 'texinfo)
             :to-equal
	     "@uref{https://dev.azure.com/msazure/One/_git/Some-Repo?path=/some/path/foo.cs&line=42&lineEnd=53&lineStartColumn=0&lineEndColumn=1000,Some-Repo:/some/path/foo.cs}")))

(describe
 "exporting ascii src links"
 (it "for path only links"
     (expect (ado-src-export "some/path/foo.cs" nil 'ascii)
             :to-equal
	     "Azure-Compute:/some/path/foo.cs (https://dev.azure.com/msazure/One/_git/Azure-Compute?path=/some/path/foo.cs)"))
 (it "for repo:path links"
     (expect (ado-src-export "Some-Repo:some/path/foo.cs" nil 'ascii)
             :to-equal
	     "Some-Repo:/some/path/foo.cs (https://dev.azure.com/msazure/One/_git/Some-Repo?path=/some/path/foo.cs)"))
 (it "for repo:path:Ln links"
     (expect (ado-src-export "Some-Repo:some/path/foo.cs:L42" nil 'ascii)
             :to-equal
	     "Some-Repo:/some/path/foo.cs (https://dev.azure.com/msazure/One/_git/Some-Repo?path=/some/path/foo.cs&line=42&lineEnd=42&lineStartColumn=0&lineEndColumn=1000)"))
 (it "for repo:path:Ln-m links"
     (expect (ado-src-export "Some-Repo:some/path/foo.cs:L42-53" nil 'ascii)
             :to-equal
	     "Some-Repo:/some/path/foo.cs (https://dev.azure.com/msazure/One/_git/Some-Repo?path=/some/path/foo.cs&line=42&lineEnd=53&lineStartColumn=0&lineEndColumn=1000)")))

(describe
 "exporting Markdown src links"
 (it "for path only links"
     (expect (ado-src-export "some/path/foo.cs" nil 'md)
             :to-equal
	     "[Azure-Compute:/some/path/foo.cs](https://dev.azure.com/msazure/One/_git/Azure-Compute?path=/some/path/foo.cs)"))
 (it "for repo:path links"
     (expect (ado-src-export "Some-Repo:some/path/foo.cs" nil 'md)
             :to-equal
	     "[Some-Repo:/some/path/foo.cs](https://dev.azure.com/msazure/One/_git/Some-Repo?path=/some/path/foo.cs)"))
 (it "for repo:path:Ln links"
     (expect (ado-src-export "Some-Repo:some/path/foo.cs:L42" nil 'md)
             :to-equal
	     "[Some-Repo:/some/path/foo.cs](https://dev.azure.com/msazure/One/_git/Some-Repo?path=/some/path/foo.cs&line=42&lineEnd=42&lineStartColumn=0&lineEndColumn=1000)"))
 (it "for repo:path:Ln-m links"
     (expect (ado-src-export "Some-Repo:some/path/foo.cs:L42-53" nil 'md)
             :to-equal
	     "[Some-Repo:/some/path/foo.cs](https://dev.azure.com/msazure/One/_git/Some-Repo?path=/some/path/foo.cs&line=42&lineEnd=53&lineStartColumn=0&lineEndColumn=1000)")))

(describe
 "opening links in a browser"
 (before-each (spy-on 'browse-url))
 (it "opens the browser for src links"
     (expect (ado-src-command "some/path/foo.cs") :not :to-throw)
     (expect 'browse-url :to-have-been-called-with
             "https://dev.azure.com/msazure/One/_git/Azure-Compute?path=/some/path/foo.cs")))

;; Local Variables:
;; read-symbol-shorthands: (("ado-" . "org-azuredevops--"))
;; End:
