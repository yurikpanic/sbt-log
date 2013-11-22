(defpackage :sbt-log
  (:nicknames :sbt)
  (:use :cl)
  (:export :with-bt-log
           :*bt-log-stream*
           :print-bt))

(in-package :sbt-log)

(defvar *bt-log-stream* t)

(defun print-file-pos (src)
  (ignore-errors
    (let ((offs (sb-introspect:definition-source-character-offset src)))
      (let ((line 1))
        (with-open-file (f (sb-introspect:definition-source-pathname src) :element-type 'character)
          (dotimes (i (1+ offs))
            (and (eq (read-char f) #\Newline) (incf line))))
        (format *bt-log-stream* " :~D" line)))))

(defun print-source-location (fun-name)
  (ignore-errors
    (let* ((src (sb-introspect:find-definition-source (symbol-function fun-name)))
           (path (sb-introspect:definition-source-pathname src)))
      (format *bt-log-stream* "~%      ~S" path)
      (print-file-pos src))))

(defun print-bt ()
  (loop for frame = (sb-di:top-frame) then (sb-di:frame-down frame)
     until (null frame)
     do (let* ((fun (sb-di:frame-debug-fun frame))
               (fun-name (sb-di:debug-fun-name fun)))
          (format *bt-log-stream* "~D: ~S" (sb-di:frame-number frame) fun-name)
          (print-source-location fun-name)
          (format *bt-log-stream* "~%"))))

(defun sbt-debug (condition dh)
  (declare (ignore dh))
  (format *bt-log-stream* "Ooops: ~A~%" condition)
  (print-bt)
  (invoke-debugger condition))

(defmacro with-bt-log (&body body)
  `(let ((*debugger-hook* #'sbt-debug))
     ,@body))
