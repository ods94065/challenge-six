(in-package :challenge-six)

(defun fmt-type-to-char (fmt-type)
  (let ((s (string-upcase (trim-to-string fmt-type))))
    (when (> (length s) 0) (char s 0))))

#.(locally-enable-sql-reader-syntax)

(defun select-report-fields (user-id rpt-id)
  "Returns report criteria fields for given report and user."
  (query-zip (format nil (<++ "SELECT * FROM "
			      "(Reports INNER JOIN ReportCriteria ON Reports.ReportID = ReportCriteria.ReportRef) "
			      "INNER JOIN UserFields ON ItemID = UserFields.RptItemID "
			      "WHERE ReportID = ~a AND UserID = ~a") 
		     rpt-id user-id)))

(defun validate-criterion (c)
  (with-assoc c (FldTitle FmtType FldValue Required)
    (let* ((fld-val (trim-to-string FldValue))
	   (fld-val-len (length fld-val))
	   (fmt-type (fmt-type-to-char FmtType))
	   errors)
      (when (and (eql fmt-type #\N) (> fld-val-len 0) (not (is-number fld-val)))
	(push (format nil "Invalid Number: ~a" fld-val) errors))
      (when (and (eql fmt-type #\D) (> fld-val-len 0) (not (is-date fld-val)))
	(push (format nil "Invalid Date: ~a" fld-val) errors))
      (when (and (= Required 1) (= fld-val-len 0))
	(push (format nil "Field is Required: '~a'" FldTitle) errors))
      (nreverse errors))))

(defun validate-criteria (user-id rpt-id)
  (let ((report-fields (select-report-fields user-id rpt-id)))
    (values report-fields (mapcan #'validate-criterion report-fields))))

(defun save-crit (user-id rpt-field)
  (with-assoc rpt-field (ItemID)
    (let* ((pname (<++ "fld_" (write-to-string ItemID)))
	   (new-fld-val (parameter pname)))
    (update-records [UserFields] :attributes (list [FldValue]) :values (list new-fld-val)
		    :where [and [= [UserID] user-id] [= [RptItemID] ItemID]]))))

(defun save-crit-values (user-id rpt-id)
  "Save criteria responses to table from HTTP."
  (with-transaction ()
    (dolist (rpt-field (select-report-fields user-id rpt-id))
      (save-crit user-id rpt-field))))

(defun gen-where-field-clause (rpt-field)
  (with-assoc rpt-field (FldName FldValue FmtType Comparer)
    (let* ((fld-val (trim-to-string FldValue))
	   (fmt-type (fmt-type-to-char FmtType))
	   (comp (trim-to-string Comparer))
	   (use-val
	    (if (is-blank fld-val) ""
		(if (not fmt-type)
		    fld-val
		    (case fmt-type
		      (#\T (if (is-blank comp) 
			       (wrap "'" (wrap "%" fld-val))
			       (wrap "'" fld-val)))
		      (#\D (format nil "datetime(~a)" fld-val))
		      (#\N fld-val)
		      (#\Y (let ((yes-no-val (string-downcase fld-val)))
			     (cond ((string= yes-no-val "yes") "1")
				   ((string= yes-no-val "no") "0")
				   (t ""))))
		      (#\L (let ((list-val (string-downcase fld-val)))
			     (cond ((string= list-val "(any)") "")
				   (t (wrap "'" fld-val)))))))))
	   (use-comparer
	    (wrap " " (cond ((and (string= fmt-type "T") (string= comp "") "LIKE"))
			    ((string= comp "") "=")
			    (t comp)))))
      (when (not (is-blank use-val))
	(list (<++ FldName use-comparer use-val))))))

(defun gen-where-clause (where-base rpt-fields)
  (let ((use-where-base (trim-to-string where-base))
	(where-field-clauses (mapcan #'gen-where-field-clause rpt-fields)))
    (when (not (is-blank use-where-base))
      (push use-where-base where-field-clauses))
    (format nil " WHERE ~{~a~^ AND ~}" where-field-clauses)))

(defun gen-report-query (user-id rpt-id max-rows row-offset)
  (let ((rpt-fields (select-report-fields user-id rpt-id)))
    (when (and rpt-fields (consp rpt-fields))
      (let ((rpt (car rpt-fields)))
	(with-assoc rpt (SelectClause FromClause WhereClause GroupByClause OrderByClause)
	  (<++ (format nil "SELECT ~a FROM ~a" SelectClause FromClause)
	       (gen-where-clause WhereClause rpt-fields)
	       (when GroupByClause (format nil " GROUP BY ~a" GroupByClause))
	       (when OrderByClause (format nil " ORDER BY ~a" OrderByClause))
	       (format nil " LIMIT ~a OFFSET ~a" max-rows row-offset)))))))
	
(defun gen-report (q)
  (if (null q) (values nil nil) (query q)))  

(defun init-user-report (user-id rpt-id)
  "Make sure there are per-user value field records for a given report."
  (query (format nil (<++ "INSERT INTO UserFields SELECT ~a AS UserID, ItemID AS RptItemID, DefaultVal AS FldValue " 
			  "FROM ReportCriteria WHERE ReportRef = ~a AND ItemID NOT IN "
			  "(SELECT RptItemID FROM UserFields WHERE UserFields.UserID = ~a )")
		 user-id rpt-id user-id)))

#.(restore-sql-reader-syntax-state)

(defun parse-bool-criteria (val)
  (let ((dval (string-downcase val)))
    (cond ((member dval '("" "(either)") :test #'string=) "(either)")
	  ((member dval '("1" "true" "yes" "on") :test #'string=) "Yes")
	  (t "No"))))

(defun title-and-ellipsis (title minwidth)
  (if (< (length title) minwidth)
      (subseq (<++ title "..........................................") 0 minwidth)
      title))

(defun criteria-field (c)
  (with-assoc c (Width ItemID FldValue FldTitle KeepWithPrior FmtType TheList)
    (let ((use-width (if (and Width (> Width 0)) Width 20)) ; default width if blank or zero
	  (fld-ref (format nil "fld_~d" ItemID))
	  (use-val (trim-to-string FldValue))
	  (use-type (fmt-type-to-char FmtType))
	  (use-list (cons "(any)" (mapcar #'trim (split-sequence #\, TheList)))))
      (<title-padder FldTitle (= KeepWithPrior 0) 20)
      (cond
	((member use-type '(#\T #\N #\D)) (<:text :name fld-ref :value use-val :width use-width :maxlength use-width))
	((eql use-type #\Y) (<picklist1 fld-ref (parse-bool-criteria use-val) '("(either)" "Yes" "No")))
	((eql use-type #\L) (<picklist1 fld-ref use-val use-list))))))

(defun criteria-prompts (data)
  (with-hash-values data (report-fields crit-errors prompt-crit rpt-id)
    (when crit-errors
      (<:p (<color "red" (<:b "** PLEASE NOTE THE FOLLOWING ERRORS **"))
	   (<:ul
	    (dolist (err-msg crit-errors) (<:li err-msg)))))

    ; Use report and report field description tables to build prompts
    (if (null report-fields)
	(<:p "SORRY, report items are not set up yet.")
	(progn
	  (with-assoc (car report-fields) (ReportTitle)
	    (<:h3 (<:ah ReportTitle) " Report Criteria"))
	  (<:form :method "post" :action "run"
	    (<:p (dolist (report-field report-fields)
		   (criteria-field report-field)))
	    (<hidden :name "rpt_saveValues" :value "yes")
	    (<hidden :name "rpt_show" :value "yes")
	    (<hidden :name "rpt_prompt" :value (if prompt-crit "yes" "no"))
	    (<hidden :name "reportID" :value rpt-id)
	    (<:submit :name "btnSubmit" :value "View Report")
	    " " (<:&nbsp) " "
	    (<:href "clear" "Clear Criteria"))))))

(defun column-names (columns)
  (<:tr :style "background-color: #f0f0f0"
    (dolist (col columns) (<:th (<:ah col)))))

(defun display-report-chunk (query-chunk column-names)
  (let ((max-cell 100))
    (column-names column-names) ; show column names every now and then
    (dolist (row query-chunk)
      (<:tr 
	(dolist (col row)
	  (let ((scol (to-string col)))
	    (<:td 
	     (cond ((null col) (<color "#b0b0b0" "Null"))
		   ((is-blank scol) (<:&nbsp))
		   ((> (length scol) max-cell) (<:ah (<++ (subseq scol 0 max-cell) "...")))
		   (t (<:ah scol))))))))))

(defun display-report (data)
  (with-hash-values data (query results names report-title rpt-has-errs max-rows)
    (let ((grouped-query-results (group results 20))
	  (hit-max-rows (= (length results) max-rows)))
      (<:p (<:b "TEST: ") (<:ah query))
      (cond (rpt-has-errs (<:p " ** REPORT ERROR. Contact admin. ** "))
	    ((null results) (<:p "SORRY, no matching records were found. Please try again."))
	    (t (progn 
		 (<:h3 (<:ah report-title))
		 (<:table :border "1" :cellpadding "2" :cellspacing "0"
		   (dolist (chunk grouped-query-results)
		     (display-report-chunk chunk names)))
		 (when hit-max-rows
		   (progn
		     (<:p "NOTE: row " (<:b "quota") " has been reached. Perhaps you can adjust your query "
			  "to return a more specific result set.")
		     (<:br)))))))))

(defun report-page (data)
  (<simple-page "Report"
    (when (gethash :show-report data) (display-report data))
    (when (gethash :prompt-crit data) (criteria-prompts data))
    ; Bottom navigation
    (<center (<:hr) (<:href "list" "Reports List") " | " (<:href "/logout" "Logout"))))

(defmacro define-unimplemented-handler ((&rest uri-stuff))
  `(define-easy-handler ,uri-stuff ()
     (html-to-string 
       (<simple-page "Unimplemented Feature" 
         (<center (<:hr) "Feature not implemented yet" (<:hr))))))

(define-unimplemented-handler (report-clear :uri "/report/clear"))

(define-demo-handler (report :uri "/report/run")
    ((prompt-crit :real-name "rpt_prompt" :parameter-type #'yes-no-undef :init-form :yes)
     (save-crit :real-name "rpt_saveValues" :parameter-type #'yes-no-undef :init-form :no)
     (show-report :real-name "rpt_show" :parameter-type #'yes-no-undef :init-form :no)
     (rpt-id :real-name "reportID" :parameter-type #'xss-protected-string))

  (yes-no-undef-to-bool prompt-crit save-crit show-report)

  (when (is-blank rpt-id)
    (return-from report
      (html-to-string (<abort (<:b "** REPORT NOT FOUND **") " Try the " (<:href "list")))))
  
  (when save-crit (save-crit-values *user-id* rpt-id))
  
  (let ((data (make-hash-table))
	(max-rows 100)
	(row-offset 0))
    (hash-insert data show-report prompt-crit rpt-id max-rows)

    (when (or prompt-crit show-report)
      (multiple-value-bind (report-fields crit-errors)
	  (validate-criteria *user-id* rpt-id)
	(hash-insert data report-fields crit-errors)
	(when (consp report-fields)
	  (with-assoc (car report-fields) (ReportTitle)
	    (let ((report-title (trim-to-string ReportTitle)))
	      (hash-insert data report-title))))))

    (when (and show-report (null (gethash :errors data)))
      (let ((query (gen-report-query *user-id* rpt-id max-rows row-offset))
	    (rpt-has-errs))
	(or (hash-insert data query) (setf rpt-has-errs t))
	(multiple-value-to-hash-table data (results names)
	  (handler-case
	      (gen-report query)
	    (sql-database-error (e) 
	      (log-message* :error "Report Error ~a / ~a" (sql-error-error-id e) (sql-error-database-message e))
	      (setf rpt-has-errs t)
	      (values nil nil))))
        (hash-insert data rpt-has-errs)))
 
    (html-to-string (report-page data))))
