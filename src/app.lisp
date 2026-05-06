(in-package #:wst.example.bookmark-manager)

(defparameter *parse-content-middleware*
  (wst.request-content.routing:parse-request-content))

(defparameter *trust-forwarded-https-headers-p* nil)

(defmethod wst.request-content:parse-content
    ((type (eql :|application/json|)) content &optional (encoding :utf-8))
  (declare (ignore type))
  (com.inuoe.jzon:parse (wst.request-content:content-as-string content encoding)))

(defun body-value (request key)
  (let ((body (wst.routing:request-content request)))
    (cond
      ((hash-table-p body) (or (gethash key body)
                              (gethash (intern (string-upcase key) :keyword) body)))
      ((listp body) (or (cdr (assoc key body :test #'string=))
                       (cdr (assoc (intern (string-upcase key) :keyword) body))))
      (t nil))))

(defun find-session-id-cookie (cookies)
  (loop for cookie in (cl-cookie:cookie-jar-cookies cookies)
        when (string= (wst.cookies:cookie-name cookie) "wst_session_id")
          do (return (wst.cookies:cookie-value cookie))))

(defun request-header (request name)
  (log:info "getting request header" name)
  (let ((headers (wst.routing:request-headers request)))
    (or (gethash name headers)
       (gethash (string-downcase name) headers)
       (gethash (string-capitalize name) headers))))

(defun request-https-p (request)
  (and *trust-forwarded-https-headers-p*
       (or (string-equal (or (request-header request "X-Forwarded-Proto") "") "https")
           (string-equal (or (request-header request "X-Forwarded-Ssl") "") "on"))))

(defun session-cookie-value (request session-id)
  (format nil "wst_session_id=~a; Path=/; HttpOnly; SameSite=Lax; Max-Age=~a~:[~;; Secure~]"
          session-id
          *session-max-age-seconds*
          (request-https-p request)))

(defun ensure-session (request response)
  "Ensure that the request has a related session object.

 It can be:

 - A new user if no session id was found
 - A dangling session, if a session if was found but no related session object
 - A valid session id and session"
  (labels ((%create-session (request response)
             (log:info "creating session")
             (let ((session (wst.session:create-session *sessions*
                                                        (list :authed nil)
                                                        :session-id (generate-session-id))))
               (setf (getf (wst.routing:request-data request) :session) session)
               (cons :halt
                     (wst.routing:redirect-see-other-response t response "/api/v1/session/login"))))
           (%recreate-session (request response)
             (log:info "recreating session")
             (let ((session (create-session *sessions*)))
               (setf (getf (wst.routing:request-data request) :session) session)
               (break)
               (cons :halt
                     (wst.routing:redirect-see-other-response t response "/api/v1/session/login"))))
           (%current-session (request response session)
             (log:info "current session" session)
             (if (<= (getf session :expires-at 0) (current-timestamp))
                 (progn
                   ;; detroy session
                   (cons :halt
                         (wst.routing:redirect-see-other-response t response "/api/v1/session/login")))
                 (progn
                   (setf (getf (wst.routing:request-data request) :session) session)
                   (cons :continue response)))))
    (log:info "ensuring session")
    (wst.routing:with-request-data (cookies)
        request
      (let* ((session-id (find-session-id-cookie cookies))
             (session (and session-id (wst.session:recover-session *sessions* session-id))))
        (log:info "found session for" session-id session)
        (cond
          ((null session-id) (%create-session request response))
          ((and session-id (null session)) (%recreate-session request response))
          (t (%current-session request response session)))))))

bnhhhhhhaszzzzzbv      bn        (defun session-to-response-cookie (request response)
  (wst.routing:with-request-data (session)
      request
    (wst.routing:with-response-data (cookies)
        response
      (cl-cookie:merge-cookies
       cookies
       (list (cl-cookie:make-cookie
         :name "wst_session_id"
         :value (getf session :id)
         :httponly-p t
         :secure-p t
         :path "/"
         :max-age 0
         :same-site "Lax")))
      response)))

(defun setup-request (request response)
  "Initialize all necessary items like cookies, parse content..."
  (log:info "setup request")
  (setf (getf (wst.routing:request-data request) :cookies)
        (let ((cookies (wst.cookies:parse-cookies (wst.routing:request-headers request))))
          (cl-cookie:make-cookie-jar :cookies cookies)))
  (setf (getf (wst.routing:response-data response) :cookies)
        (cl-cookie:make-cookie-jar))
  (cons :continue response))

(defun finish-response (request response)
  "Convert headers."
  (log:info "finish response")
  (session-to-response-cookie request response)
  response)

(defun index-handler (request response)
  (declare (ignore request))
  (wst.routing:ok-response
   t response
   :content
   "wst + woo bookmark manager example
Demo authentication uses only a username (no password).
POST /api/v1/session/login with user=alice
POST /api/v1/bookmarks with title/url
GET /api/v1/bookmarks to list your bookmarks"))

(defun health-handler (request response)
  (declare (ignore request))
  (wst.routing:ok-response t response :content "ok"))

(defvar *users*
  '(("alice" (:id 1 :username "alice"))
    ("bob" (:id 2 :username "bob"))
    ("jane" (:id 3 :username "jane"))))

(defun login-handler (request response)
  (log:info "log-in handler")
  (wst.routing:with-request-data (session)
      request
    (log:info session)
    (let ((user-name (trim-whitespace (or (body-value request "user") ""))))
      (if (not (valid-username-p user-name))
          (wst.routing:bad-request-response t response)
          (let* ((user (cadr (assoc user-name *users* :test #'equal)))
                 (session-data (setf (getf session :data)
                                     (append (getf session :data)
                                             `(:user_id ,(getf user :id))))))
            (wst.session:update-session *sessions* session-data)
            (wst.routing:ok-response
             t
             response
             :headers (list :set-cookie
                            (session-cookie-value request
                                                  (getf session :id)))
             :content (format nil "logged in as ~a" user)))))))

(defun logout-handler (request response)
  (let ((session-id (find-session-id-cookie request)))
    (when session-id
      (destroy-session *sessions* session-id))
    (wst.routing:redirect-see-other-response
     t
     response
     :location (wst.routing:redirect-see-other-response t response :location "/api/v1/session/login"))))

(defun list-bookmarks-handler (request response)
  (wst.routing:with-request-data (session)
      request
    (cond
      ((null session)
       (wst.routing:unauthorized-response t response))
      (t
       (let ((bookmarks (bookmarks-for-user session)))
         (wst.routing:ok-response
          t response
          :content (if bookmarks
                       (with-output-to-string (out)
                         (dolist (row bookmarks)
                           (format out "~a. ~a -> ~a~%"
                                   (sqlite-row-column row 0)
                                   (sqlite-row-column row 1)
                                   (sqlite-row-column row 2))))
                       "no bookmarks yet")))))))

(defun create-bookmark-handler (request response)
  (let ((user (current-user request)))
    (if (null user)
        (wst.routing:unauthorized-response t response)
        (let* ((raw-url (body-value request "url"))
               (url (normalize-url raw-url))
               (title (trim-whitespace (or (body-value request "title") "")))
               (final-title (if (zerop (length title)) url title)))
          (if (or (null url)
                  (not (valid-http-url-p url)))
              (wst.routing:write-response response
                                          :status 400
                                          :content "url field is required and must be http/https")
              (progn
                (add-bookmark user final-title url)
                (wst.routing:write-response
                 response
                 :status 201
                 :content (format nil "saved bookmark ~a -> ~a" final-title url))))))))

(defun request-bookmark-id (request)
  (wst.routing:with-request-data (params) request
    (let ((raw (cdr (assoc "bookmark-id" params :test #'string-equal))))
      (parse-positive-integer raw))))

(defun delete-bookmark-handler (request response)
  (let ((user (current-user request)))
    (if (null user)
        (wst.routing:unauthorized-response t response)
        (let ((bookmark-id (request-bookmark-id request)))
          (if (or (null bookmark-id)
                  (not (bookmark-exists-for-user-p user bookmark-id)))
              (wst.routing:not-found-response t response :content "bookmark not found")
              (progn
                (delete-bookmark user bookmark-id)
                (wst.routing:ok-response t response :content (format nil "deleted bookmark ~a" bookmark-id))))))))

(defun not-found-handler (request response)
  (declare (ignore request))
  (wst.routing:not-found-response t response :content "route not found"))

(defun build-app-routes ()
  (wst.routing:condition-handler #'wst.routing:development-condition-handler)

  (wst.routing.dsl:build-webserver
   `(wst.routing.dsl:wrap
     :before (,*parse-content-middleware* setup-request ensure-session)
     :after (finish-response)
     :route (wst.routing.dsl:group
             (wst.routing.dsl:route :GET index "/" index-handler)
             (wst.routing.dsl:route :GET health "/health" health-handler)
             (wst.routing.dsl:resource
              "/api/v1"
              (wst.routing.dsl:group
               (wst.routing.dsl:route :POST login "/session/login" login-handler)
               (wst.routing.dsl:route :POST logout "/session/logout" logout-handler)
               (wst.routing.dsl:route :POST create-bookmark "/bookmarks" create-bookmark-handler))
              (wst.routing.dsl:route :GET list-bookmarks "/bookmarks" list-bookmarks-handler)
              (wst.routing.dsl:route :DELETE delete-bookmark "/bookmarks/:bookmark-id" delete-bookmark-handler))
             (wst.routing.dsl:any-route :GET not-found-handler)))))

(defun app (env)
  (let* ((request (wst.routing.woo:request-from-woo-env env))
         (response (wst.routing:dispatch-route request)))
    (wst.routing.woo:response-to-woo-response response)))
