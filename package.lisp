(in-package :cl-user)

(defpackage pokemonstrous
  (:use :cl)
  (:export :random-battle
	   :*datastore*
	   :build-db
	   :*api-endpoint*
	   :*game-version*)
  (:nicknames :pm))
