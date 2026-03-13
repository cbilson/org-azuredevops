;;; tests for Azure DevOps work item links  -*- lexical-binding: t -*-
(require 'buttercup)
(require 'org-azuredevops)

(describe
 "parsing work item links"
 (it "parses a bare numeric ID"
     (let ((result (ado-parse-workitem-link "18469984")))
       (expect (cdr (assq 'id result)) :to-equal "18469984")
       (expect (cdr (assq 'org result)) :to-equal "msazure/One")))

 (it "parses org/project:id format"
     (let ((result (ado-parse-workitem-link "msazure/One:18469984")))
       (expect (cdr (assq 'id result)) :to-equal "18469984")
       (expect (cdr (assq 'org result)) :to-equal "msazure/One")))

 (it "parses a different org/project"
     (let ((result (ado-parse-workitem-link "microsoft/Two:12345")))
       (expect (cdr (assq 'id result)) :to-equal "12345")
       (expect (cdr (assq 'org result)) :to-equal "microsoft/Two"))))

(describe
 "work item link URL generation"
 (it "generates URL from bare ID using defaults"
     (expect (ado-workitem-url "18469984")
             :to-equal
             "https://dev.azure.com/msazure/One/_workitems/edit/18469984"))

 (it "generates URL with explicit org/project"
     (expect (ado-workitem-url "msazure/One:18469984")
             :to-equal
             "https://dev.azure.com/msazure/One/_workitems/edit/18469984"))

 (it "generates URL for a different org/project"
     (expect (ado-workitem-url "microsoft/Two:12345")
             :to-equal
             "https://dev.azure.com/microsoft/Two/_workitems/edit/12345"))

 (it "respects explicit org parameter override"
     (expect (ado-workitem-url "12345" "otherorg/Proj")
             :to-equal
             "https://dev.azure.com/otherorg/Proj/_workitems/edit/12345")))

(describe
 "work item export with org/project prefix"
 (it "uses just the numeric ID in auto-generated description"
     (expect (ado-export "Feature" "msazure/One:18469984" 'ado-workitem-url nil 'html)
             :to-equal
             "<a target=\"_blank\" href=\"https://dev.azure.com/msazure/One/_workitems/edit/18469984\">Feature #18469984</a>"))

 (it "uses just the numeric ID for bare IDs too"
     (expect (ado-export "Feature" "18469984" 'ado-workitem-url nil 'html)
             :to-equal
             "<a target=\"_blank\" href=\"https://dev.azure.com/msazure/One/_workitems/edit/18469984\">Feature #18469984</a>"))

 (it "preserves explicit description when provided"
     (expect (ado-export "Feature" "microsoft/Two:12345" 'ado-workitem-url "My Feature" 'html)
             :to-equal
             "<a target=\"_blank\" href=\"https://dev.azure.com/microsoft/Two/_workitems/edit/12345\">My Feature</a>")))

;; Local Variables:
;; read-symbol-shorthands: (("ado-" . "org-azuredevops--"))
;; End:
