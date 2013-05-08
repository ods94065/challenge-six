;;;; tags.lisp
;;;; Defines HTML convenience macros.

(in-package :challenge-six)

(deftag-macro <simple-page (title &body body)
  "Displays a simple page with the given title and body."
  `(<:html (<:head (<:title (<:ah ,title)))
	   (<:body :style "background-color: white" ,@body)))

(deftag-macro <abort (&body body)
  "Displays an abort page."
  `(<simple-page "Abort" ,@body))

(deftag-macro <color (c &body body)
  "Displays the given body with the given text color.
   c is a color expressed as a string in a CSS-compatible format."
  `(<:span :style (format nil "color: ~a" ,c) ,@body))

(deftag-macro <center (&body body)
  "Displays the given body with centered text alignment."
  `(<:div :style "text-align: center" ,@body))

(deftag-macro <picklist1 (&attribute name current items)
  "Creates a drop-down list. with the given name attribute and items.

   items is a list of strings.

   current is the default item (if any), and is shown first. It should
   be a string or nil. If it is present, it should be equal to one of
   the elements of items."
  (with-gensyms (i)
    `(<:select :name ,name
      (when ,current (<:option (<:ah ,current)))
      (dolist (,i ,items)
	(when (not (equalp ,i ,current))
	  (<:option (<:ah ,i)))))))

(deftag-macro <hidden (&allow-other-attributes others)
  "Displays a hidden form input element with the given attributes."
  `(<:input :type "hidden" ,@others))

(deftag-macro <password (&allow-other-attributes others)
  "Displays a password form input element with the given attributes."
  `(<:input :type "password" ,@others))
