;;;; packages.lisp
;;;; Defines the CHALLENGE-SIX Lisp package.

(in-package :cl-user)
(defpackage :challenge-six
  (:use :cl :clsql :hunchentoot :split-sequence :yaclml)
  (:export :start-app :init-app :start-server :stop-server :restart-server))
