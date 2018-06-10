(asdf:defsystem testgame
  :name "testgame"
  :version "0.1"
  :maintainer "JRH"
  :author "JRH"
  :license "MIT"
  :description "Test of trivial-gamekit"
  :serial t
  :depends-on (:alexandria :trivial-gamekit)
  
  :components ((:file "testgame")))
