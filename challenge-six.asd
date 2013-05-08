(defpackage :challenge-six-system (:use :asdf :cl))
(in-package :challenge-six-system)
(defsystem challenge-six
    :name "Challenge 6"
    :author "Owen Smith"
    :version "0.1"
    :maintainer "Owen Smith"
    :license "BSD"
    :description "Challenge 6 solution."
    :long-description "Solution to http://c2.com/cgi/wiki?ChallengeSixVersusFpDiscussion in Common Lisp."
    :components
    ((:module "base" :pathname "src/lisp"
	      :components
	      ((:file "packages")
	       (:file "controller" :depends-on ("packages" "util" "tags"))
	       (:file "login" :depends-on ("packages" "util" "tags"))
	       (:file "report" :depends-on ("packages" "util" "tags" "controller"))
	       (:file "report-clear" :depends-on ("packages" "util" "tags" "controller"))
	       (:file "report-list" :depends-on ("packages" "util" "tags"))
	       (:file "start-site" :depends-on ("packages" "util"))
	       (:file "tags" :depends-on ("packages" "util"))
	       (:file "util" :depends-on ("packages")))))
    :depends-on
    (:chronicity :clsql :clsql-sqlite3 :hunchentoot :ironclad :split-sequence :yaclml))
