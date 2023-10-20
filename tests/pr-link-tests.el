;;; tests for Azure DevOps pull request code links  -*- lexical-binding: t -*-
(require 'buttercup)
(require 'org-azuredevops)

(defun fake-link-function (id)
  (format "https://some-link-to/some-id/%s" id))

(describe
 "exporting ADO links"
 (it "converts work item links to HTML"
     (expect (ado-export "SomeWorkItemType" "SomeID" 'fake-link-function "some description" 'html)
             :to-equal
	     "<a target=\"_blank\" href=\"https://some-link-to/some-id/SomeID\">some description</a>"))
 (it "converts work item links to LaTeX"
     (expect (ado-export "SomeWorkItemType" "SomeID" 'fake-link-function "some description" 'latex)
             :to-equal
	     "\\href{https://some-link-to/some-id/SomeID}{some description}"))
 (it "converts work item links to Texinfo"
     (expect (ado-export "SomeWorkItemType" "SomeID" 'fake-link-function "some description" 'texinfo)
             :to-equal
	     "@uref{https://some-link-to/some-id/SomeID,some description}"))
 (it "converts work item links to ASCII"
     (expect (ado-export "SomeWorkItemType" "SomeID" 'fake-link-function "some description" 'ascii)
             :to-equal
	     "some description (https://some-link-to/some-id/SomeID)"))
 (it "converts work item links to Markdown"
     (expect (ado-export "SomeWorkItemType" "SomeID" 'fake-link-function "some description" 'md)
             :to-equal
	     "[some description](https://some-link-to/some-id/SomeID)")))

(describe
 "Converting Azure DevOps PR titles to links"
 (it "converts a good title to a link"
     (expect (org-azuredevops-pullrequest-title-to-link "Pull Request 123456789: Some really cool change")
             :to-equal
             "pr:123456789: Some really cool change")
     (expect (org-azuredevops-pullrequest-title-to-link "Pull Request 7497240: Lifecycle - Assign resource to Decom owner when they go to PreparingDecom (could be after ReturnResource API call)")
             :to-equal
             "pr:7497240: Lifecycle - Assign resource to Decom owner when they go to PreparingDecom (could be after ReturnResource API call)"))
 (it "leaves bad titles as is"
     (expect (org-azuredevops-pullrequest-title-to-link "Blah 123456789: Some really cool change")
             :to-equal "Blah 123456789: Some really cool change")
     (expect (org-azuredevops-pullrequest-title-to-link "Pull Request: Some really cool change")
             :to-equal "Pull Request: Some really cool change")))

;; Local Variables:
;; read-symbol-shorthands: (("ado-" . "org-azuredevops--"))
;; End:
