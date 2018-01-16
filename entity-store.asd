(asdf:defsystem :entity-store
  :serial t
  :depends-on (:alexandria)
  :components ((:file "package")
               (:file "entity-store")))

