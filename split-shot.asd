
(asdf:defsystem #:split-shot
  :author "Zach Kost-Smith"
  :license "GPLv3 or later"
  :version "0.1"
  :serial t
  :components ((:file "package")
               (:file "utils")
               (:file "split-shot"))
  :depends-on (:iterate :trivial-gamekit))
