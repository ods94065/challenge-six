(in-package :challenge-six)

(require :chronicity)

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
    (values report-fields (mappend #'validate-criterion report-fields))))

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
	(where-field-clauses (mappend #'gen-where-field-clause rpt-fields)))
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
  (query (<++ (format nil "INSERT INTO UserFields SELECT ~a AS UserID, ItemID AS RptItemID, DefaultVal AS FldValue " user-id)
	      (format nil "FROM ReportCriteria WHERE ReportRef = ~a AND ItemID NOT IN " rpt-id)
	      (format nil "(SELECT RptItemID FROM UserFields WHERE UserFields.UserID = ~a )" user-id))))

#.(restore-sql-reader-syntax-state)

(defun parse-bool-criteria (val)
  (let ((temp (string-downcase val)))
    (cond ((member temp '("" "(either)") :test #'string=) "(either)")
	  ((member temp '("1" "true" "yes" "on") :test #'string=) "Yes")
	  (t "No"))))

(define-html-macro :abort (&rest args) `(:html (:body (:p ,@args))))

(define-html-macro :if (test then else) `(if ,test (html ,then) (html ,else)))

(define-html-macro :when (test then) `(when ,test (html ,then)))

(define-html-macro :cond (&rest clauses)
  `(cond ,@(mapcar #'(lambda (clause) `(,(car clause) (html ,(cadr clause))))
		   clauses)))

(define-html-macro :picklist1 (field-name current the-list)
  "field-name=the HTML field name, current=default item (if any), the-list=comma-separated list"
  `(:select :name ,field-name
    (:when ,current (:option (:print ,current)))
    (dolist (i (split-sequence #\, ,the-list))
      (when (not (equalp (trim i) (trim ,current)))
	(html (:option i))))))

(define-html-macro :input-box (h-type field-name field-val &optional (width 0) (max-length 0))
  "Generate an HTML output box. Zero widths exclude tag."
  (with-gensyms (trimmed-field-val has-width has-maxlen)
    `(let ((,trimmed-field-val (trim-to-string ,field-val))
	   (,has-width (> ,width 0))
	   (,has-maxlen (> ,max-length 0)))
       (cond ((and ,has-width ,has-maxlen)
	      (html (:input :type ,h-type :name ,field-name :value (:print ,trimmed-field-val) :width ,width :maxlength ,max-length)))
	     (,has-width
	      (html (:input :type ,h-type :name ,field-name :value (:print ,trimmed-field-val) :width ,width)))
	     (,has-maxlen
	      (html (:input :type ,h-type :name ,field-name :value (:print ,trimmed-field-val) :maxlength ,max-length)))
	     (t
	      (html (:input :type ,h-type :name ,field-name :value (:print ,trimmed-field-val))))))))

(define-html-macro :title-padder (title is-break min-width)
  "Format field title for consistent appearance. A future version will use HTML tables."
  `(:if (not ,is-break)
     (:tt (:print (wrap " " ,title)))
     (:progn (:br)
       (:tt (:if (< (length ,title) ,min-width)
		 (:print (subseq (<++ ,title "..........................................") 0 ,min-width))
		 ,title)))))

(define-html-macro :criteria-field (c)
  `(with-assoc ,c (Width ItemID FldValue FldTitle KeepWithPrior FmtType TheList)
     (let ((use-width (if (and Width (> Width 0)) Width 20)) ; default width if blank or zero
	   (fld-ref (format nil "fld_~d" ItemID))
	   (use-val (trim-to-string FldValue))
	   (use-type (fmt-type-to-char FmtType)))
       (html
	(:title-padder FldTitle (= KeepWithPrior 0) 20)
	(:cond
	  ((member use-type '(#\T #\N #\D)) (:input-box "text" fld-ref use-val use-width use-width))
	  ((eql use-type #\Y) (:picklist1 fld-ref (parse-bool-criteria use-val) "(either),Yes,No"))
	  ((eql use-type #\L) (:picklist1 fld-ref use-val (<++ "(any)," TheList))))))))

(define-html-macro :criteria-prompts (report-fields errors)
  `(:progn
     (log-message :info ">>> criteria-promts: ~S ~S" ,report-fields ,errors)
     (:when ,errors
       (:p (:font :color "red" (:b "** PLEASE NOTE THE FOLLOWING ERRORS **"))
	   (:ul
	    (dolist (err-msg ,errors) (html (:li err-msg))))))
     ; Use report and report field description tables to build prompts
     (:if (null ,report-fields)
       (:p "SORRY, report items are not set up yet.")
       (:progn
	 (with-assoc (car ,report-fields) (ReportTitle)
	   (html (:h3 ReportTitle " Report Criteria")))
	 (:form :method "post" :action "run"
	   (:p (dolist (report-field ,report-fields)
		 (html (:criteria-field report-field))))
	   (:input-box "hidden" "rpt_saveValues" "yes")
	   (:input-box "hidden" "rpt_show" "yes")
	   (with-hash-values data (prompt-crit rpt-id)
	     (let ((sprompt-crit (if prompt-crit "yes" "no")))
	       (html (:input-box "hidden" "rpt_prompt" sprompt-crit)
		     (:input-box "hidden" "reportID" rpt-id))))
	   (:input-box "submit" "btnSubmit" "View Report")
	   (:noescape " &nbsp; ")
	   (:a :href "clear" "Clear Criteria"))))))

(define-html-macro :column-names (column-names)
  `(:tr :bgcolor "#f0f0f0"
    (dolist (col ,column-names)
      (html (:th col)))))

(define-html-macro :display-report-chunk (query-chunk column-names)
  (let ((max-cell 100))
    `(:progn
       (:column-names ,column-names) ; show column names every now and then
       (dolist (row ,query-chunk)
	 (html (:tr
		(dolist (col row)
		  (let ((scol (mkstr col)))
		    (html (:td 
			   (:cond ((null col) (:font :color "#b0b0b0" "Null"))
				  ((is-blank scol) (:noescapes "&nbsp;"))
				  ((> (length scol) ,max-cell) (:print (<++ (subseq scol 0 ,max-cell) "...")))
				  (t scol))))))))))))

(define-html-macro :display-report (query query-results column-names title has-errs max-rows)
  (with-gensyms (chunk)
    `(let ((grouped-query-results (group ,query-results 20))
	   (hit-max-rows (= (length ,query-results) ,max-rows)))
       (html
	(:p (:b "TEST: ") ,query)
	(:cond (,has-errs (:p " ** REPORT ERROR. Contact admin. ** "))
	       ((null ,query-results) (:p "SORRY, no matching records were found. Please try again."))
	       (t (:progn 
		    (:h3 ,title)
		    (:table :border "1" :cellpadding "2" :cellspacing "0"
		      (dolist (,chunk grouped-query-results) (html (:display-report-chunk ,chunk ,column-names)))
		        (:when hit-max-rows
			  (:progn
			    (:p "NOTE: row " (:b "quota") " has been reached. Perhaps you can adjust your query "
			        "to return a more specific result set.")
			    (:br)))))))))))

(define-html-macro :report-page (data)
  `(:html
    (:head (:title "Report"))
    (:body :bgcolor "white"
      (when (gethash (reread 'show-report) ,data)
	(with-hash-values ,data (report-title query results names rpt-has-errs)
	  (html (:display-report query results names report-title rpt-has-errs 100))))
      (when (gethash (reread 'prompt-crit) ,data)
	(with-hash-values ,data (report-fields crit-errors)
	  (html (:criteria-prompts report-fields crit-errors))))
      ; Bottom navigation
      (:div :align "center" (:hr) (:a :href "list" "Reports List") " | " (:a :href "/logout" "Logout")))))

(define-html-macro :unimplemented ()
  `(:html (:head (:title "Unimplemented Feature")) (:body (:center (:hr) "Feature not implemented yet" (:hr)))))

(defmacro define-unimplemented-handler ((&rest uri-stuff))
  `(define-easy-handler ,uri-stuff () (html-to-string (:unimplemented))))

(define-unimplemented-handler (report-clear :uri "/report/clear"))

(define-demo-handler (report :uri "/report/run")
    ((prompt-crit :real-name "rpt_prompt" :parameter-type #'yes-no-undef :init-form :yes)
     (save-crit :real-name "rpt_saveValues" :parameter-type #'yes-no-undef :init-form :no)
     (show-report :real-name "rpt_show" :parameter-type #'yes-no-undef :init-form :no)
     (rpt-id :real-name "reportID" :parameter-type #'xss-protected-string))

  (yes-no-undef-to-bool prompt-crit save-crit show-report)

  (when (is-blank rpt-id)
    (return-from report (html-to-string (:abort (:b "** REPORT NOT FOUND **") " Try the " (:a :href "list" "list")))))
  
  (when save-crit (save-crit-values *user-id* rpt-id))
  
  (let ((data (make-hash-table)))
    (hash-insert show-report data)
    (hash-insert prompt-crit data)
    (hash-insert rpt-id data)

    (when (or prompt-crit show-report)
      (multiple-value-bind (report-fields crit-errors)
	  (validate-criteria *user-id* rpt-id)
	(hash-insert report-fields data)
	(hash-insert crit-errors data)
	(when (consp report-fields)
	  (with-assoc (car report-fields) (ReportTitle)
	    (let ((report-title (trim-to-string ReportTitle)))
	      (hash-insert report-title data))))))

    (when (and show-report (null (gethash 'errors data)))
      (let ((query (gen-report-query *user-id* rpt-id 100 0))
	    (rpt-has-errs))
	(or (hash-insert query data)
	    (setf rpt-has-errs t))
	(multiple-value-to-hash-table data (results names)
	  (handler-case
	      (gen-report query)
	    (sql-database-error (e) 
	      (log-message :error "Report Error ~a / ~a" (sql-error-error-id e) (sql-error-database-message e))
	      (setf rpt-has-errs t)
	      (values nil nil))))
        (hash-insert rpt-has-errs data)))
 
    (html-to-string (:report-page data))))
