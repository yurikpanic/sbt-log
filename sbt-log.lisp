(defpackage :sbt-log
  (:nicknames :sbt)
  (:use :cl)
  (:export :with-bt-log
           :*bt-log-stream*
           :print-bt))

(in-package :sbt-log)

(defvar *bt-log-stream* t)

(defun print-bt ()
  (loop for frame = (sb-di:top-frame) then (sb-di:frame-down frame)
     until (null frame)
     do (let ((fun (sb-di:frame-debug-fun frame)))
          (format *bt-log-stream* "~D: ~A" (sb-di:frame-number frame) (sb-di:debug-fun-name fun))
          (format *bt-log-stream* "~%")))))

(defun sbt-debug (condition dh)
  (declare (ignore dh))
  (format *bt-log-stream* "Ooops: ~A~%" condition)
  (invoke-debugger condition))

(defmacro with-bt-log (&body body)
  `(let ((*debugger-hook* #'sbt-debug))
     ,@body))
