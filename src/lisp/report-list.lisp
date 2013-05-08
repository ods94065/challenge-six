;;;; report-list.lisp
;;;; Implements the /report/list URL.

(in-package :challenge-six)

(define-demo-handler (report-list :uri "/report/list") ()
  (let ((reports (query-zip "SELECT * FROM Reports ORDER BY RptSequence")))
    (html-to-string 
      (<simple-page "Report List"
	(<:h3 "Reports")
	(<:ul
	 (dolist (r reports)
	   (with-assoc (ReportID ReportTitle) r
	     (<:li (<:a :href (format nil "run?reportID=~a" ReportID) (<:ah ReportTitle))) (<:br)))
	 ;; Hard-coded items
	 (<:li (<:href "run?reportID=3&rpt_prompt=no&rpt_show=yes" "(Test Report without prompts)")))
	(<:hr)))))
