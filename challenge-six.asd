(defpackage :challenge-six-system (:use :asdf :cl))
(in-package :challenge-six-system)
(defsystem challenge-six
    :name "Challenge 6"
    :author "Owen Smith"
    :version "0.1"
    :maintainer "Owen Smith"
    :license "BSD"
    :description "Challenge 6 solution."
    :long-description "Solution to http://c2.com/cgi/wiki?ChallengeSixVersusFpDiscussion in Lisp."
    :components
    ((:file "packages")
     (:file "login" :depends-on ("packages" "util"))
     (:file "report" :depends-on ("packages" "util"))
     (:file "report-list" :depends-on ("packages" "util"))
     (:file "start-site" :depends-on ("packages" "util"))
     (:file "util" :depends-on ("packages")))
    :depends-on (:html :hunchentoot :split-sequence :clsql :clsql-sqlite3 :chronicity :ironclad))
