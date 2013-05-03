(in-package :challenge-six)

(deftag-macro <simple-page (title &body body) `(<:html (<:head (<:title (<:ah ,title))) (<:body :style "background-color: white" ,@body)))

(deftag-macro <abort (&body body) `(<simple-page "Abort" ,@body))

(deftag-macro <color (c &body body) `(<:span :style (format nil "color: ~a" ,c) ,@body))

(deftag-macro <center (&body body) `(<:div :style "text-align: center" ,@body))

(deftag-macro <picklist1 (name current items)
  "fldName=the HTML field name, current=default item(if any), items=list of items" 
  (with-gensyms (i)
    `(<:select :name ,name
      (when ,current (<:option (<:ah ,current)))
      (dolist (,i ,items)
	(when (not (equalp ,i ,current))
	  (<:option (<:ah ,i)))))))

(deftag-macro <hidden (&allow-other-attributes others)
  `(<:input :type "hidden" ,@others))

(deftag-macro <password (&allow-other-attributes others)
  `(<:input :type "password" ,@others))

(deftag-macro <title-padder (title break minwidth)
  "Format field title for consistent appearance. A future version will use HTML tables."
  `(if (not ,break)
       (<:tt (<:ah (wrap " " ,title)))
       (progn (<:br) (<:tt (<:ah (title-and-ellipsis ,title ,minwidth))))))
