(asdf:defsystem pokemonstrous
  :version "0.1"
  :author "Scott Helvick"
  :license "MIT"
  :depends-on (:bknr.datastore)
  :components ((:file "package")
	       (:file "main")))

(asdf:defsystem pokemonstrous.build
  :depends-on (:bknr.datastore
	       :drakma
	       :jonathan
	       :pokemonstrous)
  :components ((:file "build")))
