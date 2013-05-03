(in-package :challenge-six)

#.(locally-enable-sql-reader-syntax)

(define-demo-handler (report-list :uri "/report/list") ()
  (let ((reports (select-zip [*] :from [Reports] :order-by [RptSequence])))
    (html-to-string 
      (<simple-page "Report List"
	(<:h3 "Reports")
	(<:ul
	 (dolist (r reports)
	   (with-assoc r (ReportID ReportTitle)
	     (<:li (<:a :href (format nil "run?reportID=~a" ReportID) (<:ah ReportTitle))) (<:br)))
	 ; Hard-coded items
	 (<:li (<:href "run?reportID=3&rpt_prompt=no&rpt_show=yes" "(Test Report without prompts)")))
	(<:hr)))))

#.(restore-sql-reader-syntax-state)
