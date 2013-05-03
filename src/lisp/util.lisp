(in-package :challenge-six)

(require :ironclad)

(defvar *demo-db-path*)

(defun to-string (&rest args)
  "Prints all the arguments to a string."
  (with-output-to-string (s) (dolist (a args) (princ a s))))

(defun compose (&rest fs)
  "Returns a function that is the functional composition of the given functions."
  (if (not fs)
      #'identity
      (let ((f1 (car (last fs)))
	    (fs (butlast fs)))
	#'(lambda (&rest args)
	    (reduce #'funcall fs :from-end t :initial-value (apply f1 args))))))

(defun key-symb (&rest args)
  "Converts each of the given string arguments to symbols in the keyword package."
  (values (intern (apply (compose #'string-upcase #'to-string) args) :keyword)))

(defmacro <++ (&rest args) 
  "An alias for string concatenation."
  `(concatenate 'string ,@args))

(defun group (src n)
  "Takes a list and returns a list of groups of size n.

  The groups are successive sublists of the input list. The last group may have less than n elements."
  (if (zerop n)
      (error "cannot make subgroups of length 0")
      (labels ((group-rec (src acc)
		 (let ((rest (nthcdr n src)))
		   (if (consp rest)
		       (group-rec rest (cons (subseq src 0 n) acc))
		       (nreverse (cons src acc))))))
	(when src (group-rec src nil)))))

(defmacro with-gensyms ((&rest gensyms) &body body)
  "Binds the given variables to gensyms. These get substituted in macros to create guaranteed-unique variable names."
  `(let ,(mapcar #'(lambda (g) `(,g (gensym))) gensyms)
     ,@body))

(defmacro hash-insert (table &rest vars)
  "Insert the value of each given variable into a hashtable, using the variable name itself as a key."
  `(progn
     ,@(mapcar #'(lambda (var)
		  `(setf (gethash (key-symb ',var) ,table) ,var))
	      vars)))

(defmacro multiple-value-to-hash-table (table (&rest keys) form)
  "Name the return values from the given form and insert them into a hastable, using the names as keys."
  `(multiple-value-bind ,keys ,form
     ,@(mapcar #'(lambda (k) 
		   `(setf (gethash (key-symb ',k) ,table) ,k))
	       keys)))

(defmacro with-hash-values (table (&rest keys) &body body)
  "Bind each of the given variable names to values from the hashtable, using the name of each as the key. Then evaluate the body."
  `(let ,(mapcar #'(lambda (k) 
		     `(,k (gethash (key-symb ',k) ,table)))
		 keys)
     ,@body))

(defmacro with-assoc (alist (&rest keys) &body body)
  "Bind each of the given variable names to values from the assoc list, using the name of each as the key. Then evaluate the body."
  `(let ,(mapcar #'(lambda (k) 
		     `(,k (cdr (assoc (key-symb ',k) ,alist))))
		 keys)
     ,@body))

(defun yes-no-undef (s)
  "Converts a string to one of :yes, :no, or nil."
  (when s (if (equalp s "yes") :yes :no)))

(defmacro yes-no-undef-to-bool (&rest args)
  "Convert the value of each variable from :yes, :no, or nil to a boolean, and set each variable to its converted value. In this case, nil counts as false."
  `(progn
     ,@(mapcar #'(lambda (arg) `(setf ,arg (eq ,arg :yes))) args)))

(defun xss-protected-string (s)
  "Strips out characters that could be used for SQL injection."
  (let ((dirty-chars '(#\\ #\" #\/ #\* #\' #\= #\- #\# #\; #\< #\> #\+ #\%)))
    (remove-if #'(lambda (x) (member x dirty-chars)) s)))

(defun is-number (s)
  "Returns true if the given string can be parsed as an integer."
  (handler-case
      (progn (parse-integer s) t)
    (parse-error () nil)))

(defun is-date (s)
  "Returns true if the given string can be parsed as a date-time string."
  (when (chronicity:parse s) t))

(defun trim-to-string (s)
  "Convert to string and trim."
  (trim (<++ s)))

(defun is-blank (s)
  "Returns true if s is nil or the empty string."
  (or (null s) (string= s "")))

(defun wrap (delim s)
  "Wraps the given string with the delimiter string on both front and back."
  (<++ delim s delim))

(defmacro with-demo-db (&body body)
  "Establish a connection to the demo DB and evaluate the body. Automatically disconnect from the DB when we are done."
  (with-gensyms (demo-db)
    `(let ((,demo-db (connect (list *demo-db-path*) :if-exists :old :database-type :sqlite3 :make-default t)))
       (unwind-protect
	    (progn ,@body)
	 (disconnect :database ,demo-db)))))

(defvar *user-id*)

(defmacro define-demo-handler ((&rest handler-info) (&rest param-info) &body body)
  "Defines a demo handler, which extends Hunchentoot's easy handler by adding connectivity to the demo DB and user sessions."
  `(define-easy-handler ,handler-info ,param-info
     (progv '(*user-id*) (list (session-value 'user-id))
       (when (null *user-id*) (redirect "/login"))
       (with-demo-db ,@body))))

(defun zip-select-results (results columns)
  "Given a list of result tuples and a list of tuple names, return a list of tuples of pairs.

  Example: (zip-select-results '((1 2) (3 4)) '(a b)) => '(((:a . 1) (:b . 2)) (:a . 3) (:b . 4))"
  (let ((columns-as-symbols (mapcar #'key-symb columns)))
    (mapcar #'(lambda (tuple)
		(mapcar #'cons columns-as-symbols tuple))
	    results)))

(defmacro query-zip (&rest args)
  "Executes a query with the given arguments, and collects results and column headers together into pairs using zip-select-results."
  (with-gensyms (vals col-headers)
    `(multiple-value-bind (,vals ,col-headers) (query ,@args)
       (zip-select-results ,vals ,col-headers))))

(defmacro select-zip (&rest args)
  (with-gensyms (vals col-headers)
    `(multiple-value-bind (,vals ,col-headers) (clsql:select ,@args)
       (zip-select-results ,vals ,col-headers))))

(defun insert-rows (table attributes rows)
  "Insert the given set of rows into the given table. attributes gives the column names and ordering of the row data."
  (mapcar #'(lambda (vals) (insert-records :into table :attributes attributes :values vals))
	     rows))

(defvar *whitespace-bag* '(#\Space #\Tab #\Newline))
(defun trim (s)
  "Trims whitespace characters off of the front and end of strings. Can be configured by modifying *whitespace-bag*."
  (string-trim *whitespace-bag* s))

(defmacro html-to-string (&body body)
  "Convert the HTML expression to a string."
  `(with-yaclml-output-to-string ,@body))

(defun hash-password (pwd)
  "Hash a password."
  (ironclad:byte-array-to-hex-string (ironclad:digest-sequence :sha256 (ironclad:ascii-string-to-byte-array pwd))))

(defmacro test-html-macro (m &rest args)
  `(html-to-string (,m ,@args)))
