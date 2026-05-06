(in-package #:wst.example.bookmark-manager)

(defvar *users* '())

(defparameter *database-lock*
  (sb-thread:make-mutex :name "bookmark-manager-database"))

(defparameter *database-path*
  (print (merge-pathnames #P"bookmarks.sqlite3" (or *load-truename* ""))))

(defparameter *database-connection*
  (sqlite:connect *database-path*))
(defparameter *session-max-age-seconds* 3600)

(defun serialize-session-data (data)
  (let ((*print-readably* t)
        (*print-circle* t))
    (write-to-string data)))

(defun deserialize-session-data (payload)
  (let ((*read-eval* nil))
    (read-from-string payload)))

(defparameter *sessions*
  (make-instance 'wst.session.sqlite-store:sqlite-store
                 :connection *database-connection*
                 :database-path *database-path*
                 :database-lock *database-lock*
                 :data-deserializer #'deserialize-session-data
                 :data-serializer #'serialize-session-data))

(defmacro with-database-lock (&body body)
  #+sbcl
  `(sb-thread:with-mutex (*database-lock*)
     ,@body)
  #-sbcl
  `(progn ,@body))

(defun current-timestamp ()
  (get-universal-time))

(defun sweep-expired-sessions ()
  (let ((now (current-timestamp)))
    (let ((expired-session-ids nil))
      (maphash (lambda (session-id entry)
                 (when (<= (getf entry :expires-at 0) now)
                   (push session-id expired-session-ids)))
               *sessions*)
      (dolist (session-id expired-session-ids)
        (remhash session-id *sessions*)))))

(defun generate-session-id ()
  #+sbcl
  (if (probe-file "/dev/urandom")
      (handler-case
          (let ((bytes (make-array 32 :element-type '(unsigned-byte 8))))
            (with-open-file (in "/dev/urandom"
                                :direction :input
                                :element-type '(unsigned-byte 8))
              (let ((read-count (read-sequence bytes in))
                    (expected (length bytes)))
                (unless (= read-count expected)
                  (error "Insufficient random bytes read from /dev/urandom (read ~a, expected ~a)."
                         read-count expected))))
            (with-output-to-string (out)
              (loop for byte across bytes
                    do (format out "~2,'0x" byte))))
        (error (err)
          (error "Secure session-id generation failed: ~a" err)))
      (error "Secure entropy source unavailable; pass :session-id explicitly when creating sessions."))
  #-sbcl
  (error "Automatic session-id generation requires SBCL with a secure entropy source; pass :session-id explicitly."))

(defun create-session (sessions)
  (wst.session:create-session sessions
                              (list :authed nil)
                              :session-id (generate-session-id)))

(defun destroy-session (sessions session-id)
  (wst.session:terminate-session sessions session-id))

(defun session-user (sessions session-id)
  (let ((entry (wst.session:recover-session sessions session-id)))
    (cond
      ((null entry) nil)
      ((<= (getf entry :expires-at 0) (current-timestamp))
       (remhash session-id *sessions*)
       nil)
      (t (getf entry :user)))))

(defun bookmarks-for-user (user)
  (log:info "getting bookmarks for user" user)
  (with-database-lock
    (sqlite:execute-to-list *database-connection*
                            "SELECT id, title, url
                             FROM bookmarks
                             WHERE user = ?
                             ORDER BY id DESC"
                            user)))

(defun bookmark-exists-for-user-p (user bookmark-id)
  (with-database-lock
    (not (null (sqlite:execute-single *database-connection*
                                      "SELECT id FROM bookmarks
                                       WHERE id = ? AND user = ?
                                       LIMIT 1"
                                      bookmark-id user)))))

(defun add-bookmark (user title url)
  (with-database-lock
    (sqlite:execute-non-query *database-connection*
                              "INSERT INTO bookmarks(user, title, url)
                               VALUES (?, ?, ?)"
                              user title url)))

(defun delete-bookmark (user bookmark-id)
  (with-database-lock
    (sqlite:execute-non-query *database-connection*
                              "DELETE FROM bookmarks WHERE id = ? AND user = ?"
                              bookmark-id user)))
