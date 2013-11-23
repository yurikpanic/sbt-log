sbt-log
=======

A simple library for sbcl to log backtraces on error.
After backtrace was logged the control is passed to upper level debugger if other behavior was not specified.

Usage
-----

To log backtraces on error wrap code with this macro:

        (with-bt-log ((&key (out-stream t) invoke-debugger print-vars (unhandled-only t)) &body body))

* out-stream - a stream to print backtraces to.
* invoke-debugger - a function to be called after backtrace is logged. If this parameter is nil - standard invoke-debugger will be called.
* print-vars - enable printing of local vars.
* unhandled-only - print backtrace only for unhandled errors (those which triggered debugger). If nil - print backtrace for all errors, but let error propagate further.

### Example

        (sbt:with-bt-log (:print-vars t)
          (+ 4 (/ 5 0)))

### Example

Sometimes we need to exit on error instead of entering debugger:

        (sbt:with-bt-log (:invoke-debugger #'(lambda (condition) 
                                               (declare (ignore condition))
                                               (sb-sys:os-exit 42))) 
          (+ 4 (/ 5 0)))

### Example

If we need just log errror, but leave it to be handled at upper levels.

        (handler-case
             (with-bt-log (:unhandled-only nil)
               (/ 5 0))   
          (division-by-zero (e) 42))


Other exported symbols
----------------------

*  A stream to print backtraces to (defualt - t)

       \*bt-log-stream\*

*  A function to be called instead of standard invoke-debugger (default - nil)

       \*invoke-debugger\*

*  A boolean controling whether to print local vars (default - nil)

       \*print-vars\*

*  A boolean whether to print timestamp before backtrace (default - t)

       \*print-timestamp\*


*  A function to print current backtrace

       (print-bt)
