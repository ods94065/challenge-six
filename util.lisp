(in-package :challenge-six)

(require :ironclad)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (in-html-style :xhtml))

(defvar *demo-db-path*)

(defun mkstr (&rest args)
  (with-output-to-string (s) (dolist (a args) (princ a s))))

(defun reread (&rest args)
  (values (read-from-string (apply #'mkstr args))))

(defun append1 (lst obj)
  (append lst (list obj)))

(defun mappend (f &rest lists)
  (apply #'append (apply #'mapcar f lists)))

(defun compose (&rest fs)
  (if (not fs)
      #'identity
      (let ((f1 (car (last fs)))
	    (fs (butlast fs)))
	#'(lambda (&rest args)
	    (reduce #'funcall fs :from-end t :initial-value (apply f1 args))))))

(defun key-symb (&rest args)
  (values (intern (apply (compose #'string-upcase #'mkstr) args) :keyword)))

(defmacro <++ (&rest args) `(concatenate 'string ,@args))

(defun group (src n)
  (if (zerop n)
      (error "cannot make subgroups of length 0")
      (labels ((group-rec (src acc)
		 (let ((rest (nthcdr n src)))
		   (if (consp rest)
		       (group-rec rest (cons (subseq src 0 n) acc))
		       (nreverse (cons src acc))))))
	(when src (group-rec src nil)))))

(defmacro hash-insert (var table)
  `(setf (gethash (key-symb ',var) ,table) ,var))

(defmacro multiple-value-to-hash-table (table (&rest keys) form)
  `(multiple-value-bind ,keys ,form
     ,@(mapcar #'(lambda (k) 
		   `(setf (gethash (key-symb ',k) ,table) ,k))
	       keys)))

(defmacro with-gensyms ((&rest gensyms) &body body)
  `(let ,(mapcar #'(lambda (g) `(,g (gensym))) gensyms)
     ,@body))

(defmacro with-hash-values (table (&rest keys) &body body)
  `(let ,(mapcar #'(lambda (k) 
		     `(,k (gethash (key-symb ',k) ,table)))
		 keys)
     ,@body))

(defmacro with-assoc (alist (&rest keys) &body body)
  `(let ,(mapcar #'(lambda (k) 
		     `(,k (cdr (assoc (key-symb ',k) ,alist))))
		 keys)
     ,@body))

(defun yes-no-undef (s) (when s (if (equalp s "yes") :yes :no)))

(defmacro yes-no-undef-to-bool (&rest args)
  `(progn
     ,@(mapcar #'(lambda (arg) `(setf ,arg (eq ,arg :yes))) args)))

(defun xss-protected-string (s)
  "Strips out characters that could be used for SQL injection."
  (let ((dirty-chars '(#\\ #\" #\/ #\* #\' #\= #\- #\# #\; #\< #\> #\+ #\%)))
    (remove-if #'(lambda (x) (member x dirty-chars)) s)))

(defun is-number (s)
  (handler-case
      (progn (parse-integer s) t)
    (parse-error () nil)))

(defun is-date (s)
  (when (chronicity:parse s) t))

(defun trim-to-string (s) (trim (<++ s)))

(defun is-blank (s) (or (null s) (string= s "")))

(defun wrap (delim s) (<++ delim s delim))

(defmacro with-demo-db (&body body)
  (with-gensyms (demo-db)
    `(let ((,demo-db (connect (list *demo-db-path*) :if-exists :old :database-type :sqlite3 :make-default t)))
       (unwind-protect
	    (progn ,@body)
	 (disconnect :database ,demo-db)))))

(defvar *user-id*)

(defmacro define-demo-handler ((&rest handler-info) (&rest param-info) &body body)
  `(define-easy-handler ,handler-info ,param-info
     (progv '(*user-id*) (list (session-value 'user-id))
       (when (null *user-id*) (redirect "/login"))
       (with-demo-db ,@body))))

(defun challenge-six-intern (s)
  (intern s :challenge-six))

(defun zip-select-results (results columns)
  (let ((columns-as-symbols (mapcar #'key-symb columns)))
    (mapcar #'(lambda (tuple)
		(mapcar #'cons columns-as-symbols tuple))
	    results)))

(defmacro query-zip (&rest args)
  (with-gensyms (vals col-headers)
    `(multiple-value-bind (,vals ,col-headers) (query ,@args)
       (zip-select-results ,vals ,col-headers))))

(defmacro select-zip (&rest args)
  (with-gensyms (vals col-headers)
    `(multiple-value-bind (,vals ,col-headers) (select ,@args)
       (zip-select-results ,vals ,col-headers))))

(defun insert-rows (table attributes rows)
  (mapcar #'(lambda (vals) (insert-records :into table :attributes attributes :values vals))
	     rows))

(defvar *whitespace-bag* '(#\Space #\Tab #\Newline))
(defun trim (s) (string-trim *whitespace-bag* s))

(defmacro html-to-string (&body body)
  (let ((var (gensym)))
    `(with-output-to-string (,var)
       (with-html-output (,var)
	 (progn (html ,@body) ,var)))))

(defun hash-password (pwd)
  (ironclad:byte-array-to-hex-string (ironclad:digest-sequence :sha256 (ironclad:ascii-string-to-byte-array pwd))))

(defmacro test-html-macro (m &rest args)
  `(html-to-string (,m ,@args)))

(defmacro trace-html-macro (&rest macro-names)
  (let ((trace-macro-names (mapcar #'(lambda (sym) (cons sym (reread "TRACE-" sym))) macro-names)))
    `(progn
       ,@(mapcar #'(lambda (m)
		     `(setf (symbol-function ',(cdr m)) (get ',(car m) 'com.gigamonkeys.html::html-macro)))
		 trace-macro-names)
       (trace ,@(mapcar #'cdr trace-macro-names)))))
  
