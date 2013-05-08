;;;; login.lisp
;;;; Implementation of /login and /logout URLs.

(in-package :challenge-six)

(defun load-user-by-name (user-name)
  "Loads the given user record from the database."
  (query (format nil "SELECT UserID, Password FROM Users WHERE UserName = '~a'" user-name)))

(defun validate-login (user-name in-pwd)
  "Given a user name and password, check them against the database.
   Returns the user ID if the login is valid, nil otherwise."
  (with-demo-db
    (let ((in-hash-pwd (hash-password in-pwd))
	  (results (load-user-by-name user-name)))
      (when results
	(destructuring-bind (user-id db-hash-pwd) (car results)
	  (when (string= db-hash-pwd in-hash-pwd)
	    user-id))))))

(defun do-logout ()
  "Removes the current user session, logging the user out."
  (let ((s (start-session)))
    (remove-session s)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (deftag-macro <login-page (user-name &optional error)
    "A login page with user name prefilled and an optional error displayed."
    `(<simple-page "Login"
       (<:h3 "Login")
       (when ,error (<:p (<:em (<:ah ,error))))
       (<:form :method "post" :action "/login"
	       (<:p "User name: " (<:text :name "name" :value (or ,user-name "") :maxlength 40) (<:br)
		    "Password: " (<password :name "pwd" :value "" :maxlength 40) (<:br)
		    (<:submit :name "submit" :value "Login"))))))

(define-easy-handler (login :uri "/login") 
    ((user-name :real-name "name" :parameter-type 'xss-protected-string)
     (pwd :real-name "pwd"))
  (when (or (is-blank user-name) (is-blank pwd))
    (return-from login (html-to-string (<login-page user-name))))
  (let ((user-id (validate-login user-name pwd)))
    (if (null user-id)
	(html-to-string (<login-page user-name "Either the user name or password were not in our records."))
	(progn
	  (start-session)
	  (setf (session-value 'user-id) user-id)
	  (redirect "/report/list")))))

(define-easy-handler (logout :uri "/logout") ()
  (do-logout)
  (html-to-string (<simple-page "Logged Out"
                    (<:h3 "Logged out") (<:p "Thank you!") (<:hr)
		    (<:p (<:href "/login" "Log in") " again."))))
