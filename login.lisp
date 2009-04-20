(in-package :challenge-six)

#.(locally-enable-sql-reader-syntax)

(defun load-user-by-name (user-name)
  (select [UserID] [Password] :from [Users] :where [= [UserName] user-name]))

(defun validate-login (user-name in-pwd)
  (with-demo-db
    (let ((in-hash-pwd (hash-password in-pwd))
	  (results (load-user-by-name user-name)))
      (when results
	(destructuring-bind (user-id db-hash-pwd) (car results)
	  (when (string= db-hash-pwd in-hash-pwd)
	    user-id))))))

#.(restore-sql-reader-syntax-state)

(defun do-logout ()
  (let ((s (start-session)))
    (remove-session s)))

(define-html-macro :login-page (user-name &optional error)
  `(:html
    (:head (:title "Login"))
    (:body 
     (:h3 "Login")
     (when ,error (html (:p (:em ,error))))
     (:form :method "post" :action "/login"
	    (:p "User name: " (:input :type "text" :name "name" :value (:print (or ,user-name "")) :maxlength 40) (:br)
		"Password: " (:input :type "password" :name "pwd" :value "" :maxlength 40) (:br)
		(:input :type "submit" :name "submit" :value "Login"))))))

(define-easy-handler (login :uri "/login") ((user-name :real-name "name" :parameter-type 'xss-protected-string)
					    (pwd :real-name "pwd"))
  (when (or (is-blank user-name) (is-blank pwd))
    (return-from login (html-to-string (:login-page user-name))))
  (let ((user-id (validate-login user-name pwd)))
    (if (null user-id)
	(html-to-string (:login-page user-name "Either the user name or password were not in our records."))
	(progn
	  (start-session)
	  (setf (session-value 'user-id) user-id)
	  (redirect "/report/list")))))

(define-easy-handler (logout :uri "/logout") ()
  (do-logout)
  (html-to-string (:html (:head (:title "Logged Out"))
		    (:body (:h3 "Logged out") (:p "Thank you!") (:hr)
			   (:p (:a :href "/login" "Log in") " again.")))))
