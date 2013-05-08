;;;; util.lisp
;;;; Various utility functions.

(in-package :challenge-six)

(require :ironclad)

(defvar *demo-db-path*)

(defun to-string (&rest args)
  "Prints all the arguments to a string."
  (with-output-to-string (s) (dolist (a args) (princ a s))))

(defun key-symb (&rest args)
  "Concatenates the given arguments as strings and converts to a
   symbol in the keyword package."
  (values (intern (string-upcase (apply #'to-string args)) :keyword)))

(defmacro <++ (&rest args) 
  "An alias for string concatenation."
  `(concatenate 'string ,@args))

(defun group (src n)
  "Takes a list and returns a list of groups of size n.

  The groups are successive sublists of the input list. The last group
  may have less than n elements."
  (if (zerop n)
      (error "cannot make subgroups of length 0")
      (labels ((group-rec (src acc)
		 (let ((rest (nthcdr n src)))
		   (if (consp rest)
		       (group-rec rest (cons (subseq src 0 n) acc))
		       (nreverse (cons src acc))))))
	(when src (group-rec src nil)))))

(defmacro with-gensyms ((&rest gensyms) &body body)
  "Binds the given variables to gensyms. These get substituted in
   macros to create guaranteed-unique variable names."
  `(let ,(mapcar #'(lambda (g) `(,g (gensym))) gensyms)
     ,@body))

(defmacro assoc-insert (alist &rest vars)
  "Insert the value of each given variable into an assoc list, using the
   variable name itself as a key."
  `(progn
     ,@(mapcar #'(lambda (var)
		   `(setf ,alist (acons (key-symb ',var) ,var ,alist)))
	       vars)))

(defmacro multiple-value-to-assoc (alist (&rest keys) &body body)
  "Name the return values from the given body and insert them into an
   assoc list, using the names as keys."
  `(multiple-value-bind ,keys ,body
     ,@(mapcar #'(lambda (k)
		   `(acons (key-symb ',k) ,k ,alist))
	       keys)))

(defmacro with-assoc ((&rest keys) alist &body body)
  "Bind each of the given variable names to values from the assoc
   list, using the name of each as the key. Then evaluate the body."
  `(let ,(mapcar #'(lambda (k) 
		     `(,k (cdr (assoc (key-symb ',k) ,alist))))
		 keys)
     ,@body))

(defun yes-no-bool (s)
  "Converts yes/no to a boolean value. Anything but 'yes' is false."
  (and s (equalp s "yes")))

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
  "Establish a connection to the demo DB and evaluate the
   body. Automatically disconnect from the DB when we are done."
  (with-gensyms (demo-db)
    `(let ((,demo-db (connect (list *demo-db-path*)
			      :if-exists :old
			      :database-type :sqlite3
			      :make-default t)))
       (unwind-protect
	    (progn ,@body)
	 (disconnect :database ,demo-db)))))

(defun zip-select-results (results columns)
  "Given a list of result tuples and a list of tuple names, return a 
   list of tuples of pairs.

   Example: (zip-select-results '((1 2) (3 4)) '(a b)) =>
   '(((:a . 1) (:b . 2)) (:a . 3) (:b . 4))"
  (let ((keys (mapcar #'key-symb columns)))
    (mapcar #'(lambda (vals) (mapcar #'cons keys vals))
	    results)))

(defmacro query-zip (&rest args)
  "Executes a query with the given arguments, and collects results and
   column headers together into pairs using zip-select-results."
  (with-gensyms (vals col-headers)
    `(multiple-value-bind (,vals ,col-headers) (query ,@args)
       (zip-select-results ,vals ,col-headers))))

(defvar *whitespace-bag* '(#\Space #\Tab #\Newline))

(defun trim (s)
  "Trims whitespace characters off of the front and end of
   strings. Can be configured by modifying *whitespace-bag*."
  (string-trim *whitespace-bag* s))

(defmacro html-to-string (&body body)
  "Convert the HTML expression to a string."
  `(with-yaclml-output-to-string ,@body))

(defun hash-password (pwd)
  "Hash a password string using SHA-256."
  (ironclad:byte-array-to-hex-string
   (ironclad:digest-sequence :sha256
    (ironclad:ascii-string-to-byte-array pwd))))
