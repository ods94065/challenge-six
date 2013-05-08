;;;; report.lisp
;;;; Implements the URL /report/run.

(in-package :challenge-six)

;; Declare module-level vars
(defvar *prompt-crit*)
(defvar *show-report*)
(defvar *save-crit*)
(defvar *err-msgs*)
(defvar *report-id*)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (deftag-macro <padded-title (&attribute title is-break minwidth)
    "Format field title for consistent appearance. A future version will
     use HTML tables."
    `(progn
      (when ,is-break (<:br))
      (<:tt (<:ah
        (if ,is-break
	    (if (< (length ,title) ,minwidth)
	        (subseq (<++ ,title "..........................................") 0 ,minwidth)
	        ,title)
	    (wrap " " ,title)))))))

(defun format-type-to-char (format-type)
  (let ((s (string-upcase (trim-to-string format-type))))
    (when (> (length s) 0) (char s 0))))

(defun the-list-to-list (str)
  (cons "(any)" (mapcar #'trim (split-sequence #\, str))))

(defun criteria-report-title (criteria)
  (when (consp criteria)
    (cdr (assoc :ReportTitle (car criteria)))))

(defun load-criteria ()
  "Returns report criteria fields for given report and user."
  (query-zip
   (format nil (<++ "SELECT * FROM "
		    "(Reports INNER JOIN ReportCriteria ON Reports.ReportID = ReportCriteria.ReportRef) "
		    "INNER JOIN UserFields ON ItemID = UserFields.RptItemID "
		    "WHERE ReportID = ~a AND UserID = ~a ORDER BY Sequence") 
	   *report-id* *user-id*)))

(defun validate-criterion (crit)
  (with-assoc (FldTitle FmtType FldValue Required) crit
    (let ((empty-field (or (null FldValue) (= (length FldValue) 0)))
	  (fmt (format-type-to-char FmtType)))
      (if empty-field
	  (when (= Required 1)
	    (push (format nil "Field is Required: '~a'" FldTitle) *err-msgs*))
	  (progn
	    (when (and (eql fmt #\N) (not (is-number FldValue)))
	      (push (format nil "Invalid Number: ~a" FldValue) *err-msgs*))
	    (when (and (eql fmt #\D) (not (is-date FldValue)))
	      (push (format nil "Invalid Date: ~a" FldValue) *err-msgs*)))))))

(defun validate-criteria ()
  (dolist (crit (load-criteria))
    (validate-criterion crit)))

(defun save-crit (crit)
  (with-assoc (ItemID) crit
    (let* ((pname (<++ "fld_" (write-to-string ItemID)))
	   (field-value (trim-to-string (parameter pname))))
      (execute-command
       (format nil (<++ "UPDATE UserFields SET FldValue = '~a' "
			"WHERE UserID = ~a AND RptItemID = ~a")
	       field-value *user-id* ItemID)))))

(defun save-crit-values ()
  "Save criteria responses to table from HTTP."
  (with-transaction ()
    (dolist (crit (load-criteria))
      (save-crit crit))))

(defun init-user-report ()
  "Make sure there are per-user value field records for given report."
  (query 
   (format nil 
	   (<++ "INSERT INTO UserFields SELECT ~a AS UserID, ItemID AS RptItemID, DefaultVal AS FldValue " 
		"FROM ReportCriteria WHERE ReportRef = ~a AND ItemID NOT IN "
		"(SELECT RptItemID FROM UserFields WHERE UserFields.UserID = ~a )")
	   *user-id* *report-id* *user-id*)))

(defun parse-bool-criteria (val)
  (let ((dval (string-downcase val)))
    (cond ((member dval '("" "(either)") :test #'string=) "(either)")
	  ((member dval '("1" "true" "yes" "on") :test #'string=) "Yes")
	  (t "No"))))

(defun criteria-prompts ()
  "Display report criteria prompts."
  (when *err-msgs* ; Display any validation error messages
    (<:p (<color "red" (<:b "** PLEASE NOTE THE FOLLOWING ERRORS **"))
	 (<:ul (dolist (err *err-msgs*) (<:li (<:ah err))))))
  (init-user-report)
  (let ((criteria (load-criteria)))
    (when (null criteria)
	(<:p "SORRY, report items are not set up yet.")
	(return-from criteria-prompts))
    
    ;; Use report and report field description tables to build prompts
    (let ((report-title (criteria-report-title criteria)))
      (<:h3 (<:ah report-title) " Report Criteria")
      (<:form :method "post" :action "run"
	(dolist (crit criteria)
	  (with-assoc (Width ItemID FldTitle FldValue FmtType TheList KeepWithPrior) crit
	    (let ((use-width (if (and Width (> Width 0)) Width 20)) ; default width if blank or zero
		  (fmt (format-type-to-char FmtType))
		  (field-ref (format nil "fld_~d" ItemID)))
	      (<padded-title :title FldTitle :is-break (= KeepWithPrior 0) :minwidth 20)
	      (cond
		((member fmt '(#\T #\N #\D))
		 (<:text :name field-ref :value FldValue :width use-width :maxlength use-width))
		((eql fmt #\Y)
		 (<picklist1 :name field-ref :current (parse-bool-criteria FldValue) :items '("(either)" "Yes" "No")))
		((eql fmt #\L)
		 (<picklist1 :name field-ref :current FldValue :items (the-list-to-list TheList)))))))
	(<:p
	 (<hidden :name "rpt_saveValues" :value "yes")
	 (<hidden :name "rpt_show" :value "yes")
	 (<hidden :name "rpt_prompt" :value (if *prompt-crit* "yes" "no"))
	 (<hidden :name "reportID" :value *report-id*)
	 (<:submit :name "btnSubmit" :value "View Report")
	 " " (<:&nbsp) " "
	 (<:href "clear" "Clear Criteria"))))))

(defun column-names (columns)
  (<:tr :style "background-color: #f0f0f0"
    (dolist (col columns) (<:th (<:ah col)))))

(defun display-report-chunk (result-chunk names)
  "Display column names and a set of result rows."
  (let ((max-cell 100))
    (column-names names)
    (dolist (row result-chunk)
      (<:tr 
	(dolist (col row)
	  (let ((scol (to-string col)))
	    (<:td 
	     (cond ((null col) (<color "#b0b0b0" "Null"))
		   ((is-blank scol) (<:&nbsp))
		   ((> (length scol) max-cell)
		    (<:ah (<++ (subseq scol 0 max-cell) "...")))
		   (t (<:ah scol))))))))))

(defun display-report (report)
  "Display report as an HTML table."
  (with-assoc (title query names results hit-max-rows) report
    (<:p (<:b "TEST: ") (<:ah query))
    (if (null results)
	(<:p "SORRY, no matching records were found. Please try again.")
	(progn
	  (<:h3 (<:ah title))
	  (<:table :border "1" :cellpadding "2" :cellspacing "0"
	      (dolist (chunk (group results 20))
		(display-report-chunk chunk names)))
	  (log-message* :info "hit-max-rows: ~s" hit-max-rows)
	  (when hit-max-rows
	    (<:p "NOTE: row " (<:b "quota") " has been reached. "
		 "Perhaps you can adjust your query to return a more specific result set."))))))

(defun where-val-for-type (fmt field-value comparer)
  (case fmt
    (#\T (if (is-blank comparer)
	     (wrap "'" (wrap "%" field-value)) ; for LIKE
	     (wrap "'" field-value))) ; quote wrap
    (#\D (format nil "datetime(~a)" field-value))
    (#\N field-value) ; leave as-is
    (#\Y 
     (let ((yes-no-val (string-downcase field-value)))
       (cond ((string= yes-no-val "yes") "1")
	     ((string= yes-no-val "no") "0")
	     (t ""))))
    (#\L 
     (if (string= (string-downcase field-value) "(any)")
	 ""
	 (wrap "'" field-value)))))

(defun gen-where-field-clause (crit)
  (with-assoc (FldName FldValue FmtType Comparer) crit
    (let* ((fmt (format-type-to-char FmtType))
	   (use-value
	    (cond ((is-blank FldValue) "")
		  ((is-blank FmtType) FldValue)
		  (t (where-val-for-type fmt FldValue Comparer))))
	   (use-comparer
	    (cond ((and (eql fmt #\T) (is-blank Comparer) " LIKE "))
		  ((is-blank Comparer) " = ")
		  (t (wrap " " Comparer)))))
      (when (not (is-blank use-value))
	(list (<++ FldName use-comparer use-value))))))

(defun gen-where-clause (where-clause criteria)
  (let ((where-clauses (mapcan #'gen-where-field-clause criteria)))
    (when (not (is-blank where-clause))
      (push where-clause where-clauses))
    (format nil " WHERE ~{~a~^ AND ~}" where-clauses)))

(defun gen-report-sql (criteria max-rows)
  (let ((crit (car criteria)))
    (with-assoc (SelectClause FromClause WhereClause GroupByClause OrderByClause) crit
      (<++ (format nil "SELECT ~a FROM ~a" SelectClause FromClause)
	   (gen-where-clause WhereClause criteria)
	   (when (not (is-blank GroupByClause)) (format nil " GROUP BY ~a" GroupByClause))
	   (when (not (is-blank OrderByClause)) (format nil " ORDER BY ~a" OrderByClause))
	   (format nil " LIMIT ~a" max-rows)))))

(defun load-report (criteria max-rows)
  (let ((sql (gen-report-sql criteria max-rows))
	(title (criteria-report-title criteria)))
    (if (not sql)
	`((:title . ,title)
	  (:query . ,sql)
	  (:names) (:results) (:hit-max-rows))
	(multiple-value-bind (results names) (query sql)
	  (let ((hit-max-rows (= (length results) max-rows))) 
	    `((:title . ,title)
	      (:query . ,sql)
	      (:names . ,names)
	      (:results . ,results)
	      (:hit-max-rows . ,hit-max-rows)))))))

(defun gen-report ()
  "Generate report based on built-up SQL statement."
  (let ((criteria (load-criteria))
	(max-rows 100))
    (if (null criteria)
	(<:p " ** REPORT ERROR. Contact admin. ** ")
	(display-report (load-report criteria max-rows)))))

(define-demo-handler (report :uri "/report/run")
    ((prompt-crit :real-name "rpt_prompt" :parameter-type #'yes-no-bool :init-form t)
     (save-crit :real-name "rpt_saveValues" :parameter-type #'yes-no-bool :init-form nil)
     (show-report :real-name "rpt_show" :parameter-type #'yes-no-bool :init-form nil)
     (report-id :real-name "reportID" :parameter-type #'xss-protected-string))

  ;; Initialize
  (setf *prompt-crit* prompt-crit
	*save-crit* save-crit
	*show-report* show-report
	*report-id* report-id
	*err-msgs* nil)

  (when (is-blank *report-id*)
    (return-from report
      (html-to-string (<abort (<:b "** REPORT NOT FOUND **") " Try the " (<:href "list")))))

  (html-to-string
    (<simple-page "Report"
      (when *save-crit*
	(save-crit-values))

      (when *show-report*
	(validate-criteria)
	(when (null *err-msgs*) (gen-report)))

      (when *prompt-crit*
	(criteria-prompts))

      ;; Bottom navigation
      (<center (<:hr) (<:href "list" "Reports List") " | " (<:href "/logout" "Logout")))))
