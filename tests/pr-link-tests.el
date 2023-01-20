;;; tests for Azure DevOps pull request code links  -*- lexical-binding: t -*-
(require 'buttercup)
(require 'org-azuredevops)

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
