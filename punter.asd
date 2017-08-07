(in-package :asdf)

(defsystem :punter
  :class :package-inferred-system
  :defsystem-depends-on (:asdf-package-system)
  :depends-on (:alexandria
	       :cl-json
	       :usocket
	       :flexi-streams
	       :s-base64
	       :cl-conspack
	       :hu.dwim.serializer
	       :punter/core))
