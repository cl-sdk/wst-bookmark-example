(in-package #:wst.example.bookmark-manager)

(defconstant +sigint+ 2)
(defconstant +sigquit+ 3)
(defconstant +sigterm+ 15)
(defparameter *server-port* 3000)
(defparameter *server-running-p* nil)
(defparameter *restart-requested-p* nil)

#+sbcl
(defparameter *server-control-lock*
  (sb-thread:make-mutex :name "bookmark-manager-server-control"))

(defmacro with-server-control-lock (&body body)
  #+sbcl
  `(sb-thread:with-mutex (*server-control-lock*)
     ,@body)
  #-sbcl
  `(progn ,@body))

(defun woo-signal-symbol (name)
  (or (find-symbol name :woo.signal)
      (error "Woo internal symbol ~a not found in package WOO.SIGNAL" name)))

(defun make-graceful-shutdown-signals ()
  (let ((graceful-callback-symbol (woo-signal-symbol "SIGQUIT-CB")))
    (list (cons +sigint+ graceful-callback-symbol)
          (cons +sigquit+ graceful-callback-symbol)
          (cons +sigterm+ graceful-callback-symbol))))

(defun request-graceful-stop ()
  #+sbcl
  (sb-posix:kill (sb-posix:getpid) +sigquit+)
  #-sbcl
  (error "Restarting a running server is only supported on SBCL."))

(defun start-server (&key (port 3000))
  (with-server-control-lock
    (when *server-running-p*
      (error "The example app is already running. Use (restart-server) to restart it."))
    (setf *server-port* port
          *restart-requested-p* nil))
  (wst.session.sqlite-store:initialize-sqlite-store *sessions*)
  (build-app-routes)
  (format t "~&Starting bookmark manager on http://localhost:~a~%" *server-port*)
  (format t "~&Press Ctrl+C to stop gracefully.~%")
  (let ((signals-symbol (woo-signal-symbol "*SIGNALS*")))
    (unwind-protect
         (progn
           (with-server-control-lock
             (setf *server-running-p* t))
           (progv (list signals-symbol) (list (make-graceful-shutdown-signals))
             (woo:run #'app :port *server-port* :debug t :worker-num 1)))
      (with-server-control-lock
        (setf *server-running-p* nil))))
  (format t "~&Restarting example app...~%"))

(defun restart-server (&key (port *server-port*))
  (multiple-value-bind (running-p target-port)
      (with-server-control-lock
        (setf *server-port* port)
        (if *server-running-p*
            (progn
              (setf *restart-requested-p* t)
              (values t *server-port*))
            (values nil *server-port*)))
    (if running-p
        (request-graceful-stop)
        (start-server :port target-port))))

(start-server)
