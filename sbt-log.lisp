(defpackage :sbt-log
  (:nicknames :sbt)
  (:use :cl)
  (:export :with-bt-log))

(in-package :sbt-log)

(defun sbt-debug (condition dh)
  (declare (ignore dh))
  (format t "Ooops: ~A~%" condition)
  (invoke-debugger condition))

(defmacro with-bt-log (&body body)
  `(let ((*debugger-hook* #'sbt-debug))
     ,@body))
