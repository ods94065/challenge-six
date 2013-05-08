;;;; controller.lisp
;;;; Convenience macros for defining Hunchentoot handlers.

(in-package :challenge-six)

(defmacro define-unimplemented-handler ((&rest uri-stuff))
  "Defines an URI that simply shows an 'unimplemented' error page."
  `(define-easy-handler ,uri-stuff ()
     (html-to-string 
       (<simple-page "Unimplemented Feature" 
         (<center (<:hr) "Feature not implemented yet" (<:hr))))))

(defvar *user-id*)

(defmacro define-demo-handler ((&rest handler-info) (&rest param-info) &body body)
  "Defines a demo handler, which extends Hunchentoot's easy handler by adding connectivity to the demo DB and user sessions."
  `(define-easy-handler ,handler-info ,param-info
     (progv '(*user-id*) (list (session-value 'user-id))
       (when (null *user-id*) (redirect "/login"))
       (with-demo-db ,@body))))
