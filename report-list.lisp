(in-package :challenge-six)
#.(locally-enable-sql-reader-syntax)

(define-demo-handler (report-list :uri "/report/list") ()
  (let ((reports (select-zip [*] :from [Reports] :order-by [RptSequence])))
    (html-to-string 
     (:html (:head (:title "Report List"))
       (:body (:h3 "Reports")
	      (:ul
	       (dolist (r reports)
		 (with-assoc r (ReportID ReportTitle)
		   (html (:li (:a :href (:attribute (:format "run?reportID=~a" ReportID)) ReportTitle)) (:br))))
	       ; Hard-coded items
	       (:li (:a :href "run?reportID=3&rpt_prompt=no&rpt_show=yes"
			"(Test Report without prompts)")))
		 (:hr))))))

#.(restore-sql-reader-syntax-state)
