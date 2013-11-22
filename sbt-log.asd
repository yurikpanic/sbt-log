;;; -*-  Lisp -*-
(defpackage :sbt-log-system
  (:use :cl :asdf))

(in-package :sbt-log-system)

(defsystem sbt-log
  :description "Simple sbcl backtrace logger"
  :author "Yuri Vishnevsky <vishnevsky@gmail.com>"
  :depends-on (sb-introspect)
  :components ((:file "sbt-log")))


