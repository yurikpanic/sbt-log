(defpackage :sbt-log
  (:nicknames :sbt)
  (:use :cl)
  (:export :with-bt-log
           :*bt-log-stream*
           :*invoke-debugger*
           :*print-vars*
           :*print-timestamp*
           :print-bt))

(in-package :sbt-log)

(defvar *invoke-debugger* nil)

(defvar *print-vars* nil)

(defvar *print-timestamp* t)

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

(defun print-local-vars (frame)
  (let ((fun (sb-di:frame-debug-fun frame)))
    (dolist (var (sb-di:debug-fun-lambda-list fun))
      (format *bt-log-stream* "~%          ")
      (when (typep var 'list)
        (format *bt-log-stream* "~S " (car var))
        (setf var (second var)))
      (format *bt-log-stream* "~A = ~S" (sb-di:debug-var-symbol-name var)
              (handler-case
                  (sb-di:debug-var-valid-value var frame)
                (sb-di:invalid-value () :unknown))))))

(defun print-bt ()
  (loop for frame = (sb-di:top-frame) then (sb-di:frame-down frame)
     until (null frame)
     do (let* ((fun (sb-di:frame-debug-fun frame))
               (fun-name (sb-di:debug-fun-name fun)))
          (format *bt-log-stream* "~D: ~S" (sb-di:frame-number frame) fun-name)
          (when *print-vars* (print-local-vars frame))
          (print-source-location fun-name)
          (format *bt-log-stream* "~%"))))

(defun print-timestartmp ()
  (let ((time (multiple-value-list (get-decoded-time))))
    (format *bt-log-stream* "~4,'0D-~2,'0d-~2,'0d ~2,'0d:~2,'0d:~2,'0d"
            (sixth time) (fifth time) (fourth time) (third time) (second time) (first time))))

(defun sbt-debug (condition dh)
  (declare (ignore dh))
  (when *print-timestamp*
    (print-timestartmp)
    (princ " " *bt-log-stream*))
  (format *bt-log-stream* "Ooops: ~A~%" condition)
  (print-bt)
  (if *invoke-debugger*
      (funcall *invoke-debugger* condition)
      (invoke-debugger condition)))

(defmacro with-bt-log ((&key (out-stream t) invoke-debugger print-vars (unhandled-only t)) &body body)
  `(let ((*bt-log-stream* ,out-stream)
         (*invoke-debugger* ,invoke-debugger)
         (*print-vars* ,print-vars))
     ,(if unhandled-only
          `(let ((*debugger-hook* #'sbt-debug))
             ,@body)
          `(handler-bind ((error #'(lambda (c)
                                     (declare (ignore c))
                                     (print-bt))))
             ,@body))))

