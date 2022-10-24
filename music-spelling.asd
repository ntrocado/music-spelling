(asdf:defsystem #:music-spelling
  :description "Automatic pitch and rhythm spelling."
  :author "Nuno Trocado"
  :license  "GNU Lesser Public License 3.0"
  :version "0.0.1"
  :serial t
  :depends-on (#:alexandria)
  :components ((:file "package")
	       (:file "pitch-spelling")
	       (:file "rhythm-spelling")))
