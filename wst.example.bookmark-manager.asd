(asdf:defsystem #:wst.example.bookmark-manager
  :description "Runnable wst bookmark manager example application."
  :author "Bruno Dias"
  :license "Unlicense"
  :version "0.0.1"
  :depends-on (#:log4cl
               #:wst.routing
               #:wst.routing.dsl
               #:wst.routing.woo
               #:wst.request-content
               #:wst.request-content.routing
               #:wst.cookies
               #:wst.session.sqlite-store
               #:sqlite
               #:woo)
  :serial t
  :pathname "src"
  :components ((:file "package")
               (:file "utils")
               (:file "domain")
               (:file "app")
               (:file "main")))
