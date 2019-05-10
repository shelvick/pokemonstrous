(in-package :pokemonstrous)



;; Set this to wherever you've set up your local PokeAPI endpoint.
(defvar *api-endpoint* "http://localhost:8000/api/v2/")

;; Game version for choosing which moves can be learned by which Pokemon.
;; Currently we only support one version at a time, so if you change this,
;; you have to rebuild all the Pokemon.
(defvar *game-version* "ultra-sun-ultra-moon")



;;; Helper functions

(defun getval (key alist)
  (cdr (assoc key alist :test #'equalp)))

(defun get-values (alist keys)
  (let ((keys (append '("id" "name") keys)))
    (values-list
     (mapcar #'(lambda (k) (getval k alist)) keys))))

(defun get-api-info (resource id-or-name)
  (jonathan:parse
   (cl-user::octets-to-string
    (drakma:http-request (format nil "~A~A/~A?limit=10000" *api-endpoint* resource id-or-name)))
   :as :alist))

(defun get-info (resource &optional (id-or-name ""))
  (let ((info (get-api-info resource id-or-name)))
    (if (equal id-or-name "")
	info
	(alexandria:switch (resource :test #'equalp)
	  ("type" (get-values info '("damage_relations")))
	  ("stat" (get-values info nil))
	  ("nature" (get-values info '("increased_stat" "decreased_stat")))
	  ("gender" (get-values info nil))
	  ("pokemon" (get-values info '("types" "weight" "stats" "moves")))
	  ("ability" (get-values info '("pokemon" "effect_entries")))
	  ("move" (get-values info '("type" "damage_class" "power" "accuracy" "pp" "priority"
				     "target" "effect_entries" "stat_changes" "meta" "effect_chance")))
	  ("move-ailment" (get-values info nil))
	  ("move-category" (get-values info '("descriptions")))
	  ("move-damage-class" (get-values info nil))
	  ("move-target" (get-values info '("descriptions")))
	  ("item-attribute" (get-values info '("descriptions")))
	  ("item-category" (get-values info nil))
	  ("item-fling-effect" (get-values info '("effect_entries")))
	  ("item" (get-values info '("category" "attributes" "fling_power" "fling_effect" "effect_entries")))
	  ("pokemon-species" (get-values info '("evolves_from_species")))
	  (t info)))))



;;; Database build functions

(defun build-type (id-or-name)
  (let ((r (if (integerp id-or-name)
	       (find-type-id id-or-name)
	       (find-type-name id-or-name))))
    (if r
	r
	(multiple-value-bind (id name relations)
	    (get-info "type" id-or-name)
	  (flet ((get-types (category)
		   (mapcar #'(lambda (k) (getval "name" k))
			   (getval category relations))))
	    (make-instance 'poketype
			   :oid id
			   :name name
			   :double-from (get-types "double_damage_from")
			   :half-from (get-types "half_damage_from")
			   :none-from (get-types "no_damage_from")
			   :double-to (get-types "double_damage_to")
			   :half-to (get-types "half_damage_to")
			   :none-to (get-types "no_damage_to")
			   :pokemon nil
			   :moves nil))))))

(defun build-types ()
  (format t "Building type relations...~%")
  (dotimes (i 18)
    (build-type (1+ i)))
  (build-type "unknown")
  (build-type "shadow")
  (all-types))


(defun build-stat (id-or-name)
  (let ((r (if (integerp id-or-name)
	       (find-stat-id id-or-name)
	       (find-stat-name id-or-name))))
    (if r
	r
	(multiple-value-bind (id name)
	    (get-info "stat" id-or-name)
	  (make-instance 'stat
			 :oid id
			 :name name
			 :move-increase nil
			 :move-decrease nil
			 :nature-increase nil
			 :nature-decrease nil)))))

(defun build-stats ()
  (format t "Building stats...~%")
  (dotimes (i 8)
    (build-stat (1+ i)))
  (all-stats))


(defun build-nature (id-or-name)
  (let ((r (if (integerp id-or-name)
	       (find-nature-id id-or-name)
	       (find-nature-name id-or-name))))
    (if r
	r
	(multiple-value-bind (id name inc dec)
	    (get-info "nature" id-or-name)
	  (flet ((get-stat (stat)
		   (find-stat-name (getval "name" stat))))
	    (make-instance 'nature
			   :oid id
			   :name name
			   :increased (get-stat inc)
			   :decreased (get-stat dec)))))))

(defun build-natures ()
  (format t "Building natures...~%")
  (dotimes (i 25)
    (build-nature (1+ i)))
  (all-natures))


(defun build-gender (id-or-name)
  (let ((r (if (integerp id-or-name)
	       (find-gender-id id-or-name)
	       (find-gender-name id-or-name))))
    (if r
	r
	(multiple-value-bind (id name)
	    (get-info "gender" id-or-name)
	  (make-instance 'gender
			 :oid id
			 :name name
			 :pokemon nil)))))

(defun build-genders ()
  (format t "Building genders...~%")
  (dotimes (i 3)
    (build-gender (1+ i)))
  (all-genders))


(defun build-pokemon (id-or-name)
  (let ((r (if (integerp id-or-name)
	       (find-pokemon-id id-or-name)
	       (find-pokemon-name id-or-name))))
    (if r
	r
	(multiple-value-bind (id name types weight stats moves)
	    (get-info "pokemon" id-or-name)
	  (let ((statlist (mapcar #'(lambda (s) (getval "base_stat" s)) stats)))
	    (flet ((get-stat (stat)
		     (nth (position stat stats :test #'(lambda (k v)
							 (equalp k (getval "name" (getval "stat" v)))))
			  statlist))
		   (get-types (types)
		     (mapcar #'(lambda (tname)
				 (find-type-name tname))
			     (mapcar #'(lambda (x)
					 (getval "name" (getval "type" x)))
				     types)))
		   (get-moves (moves)
		     (mapcar #'(lambda (a) (find-move-name (getval "name" a)))
			     (loop
				for i from 0
				for elt in (mapcar #'(lambda (b)
						       (find *game-version*
							     (getval "version_group_details" b)
							     :test #'(lambda (k v)
								       (equalp k (getval "name" (getval "version_group" v))))))
						   moves)
				when elt collect (nth i (mapcar #'(lambda (c)
								    (getval "move" c))
								moves))))))
	      (make-instance 'pokemon
			     :oid id
			     :name name
			     :types (get-types types)
			     :hp (get-stat "hp")
			     :atk (get-stat "attack")
			     :def (get-stat "defense")
			     :spatk (get-stat "special-attack")
			     :spdef (get-stat "special-defense")
			     :spd (get-stat "speed")
			     :weight weight
			     :moves (let ((preev (getval "name"
							 (nth-value 2 (get-info "pokemon-species" id-or-name)))))
				      (when (and preev (not (or (find-pokemon-name preev)
								(find-pokemon-id (nth-value 0 (get-info "pokemon-species" preev))))))
					(build-pokemon preev))
				      (if preev
					  (remove-duplicates (append (get-moves moves)
								     (pokemon-moves (or (find-pokemon-name preev)
											    (find-pokemon-id (nth-value 0 (get-info "pokemon-species" preev)))))))
					  (get-moves moves)))
			     :abilities nil)))))))

(defun build-pokemons ()
  (format t "Building Pokemon...~%")
  (dotimes (i 807)
    (build-pokemon (1+ i)))
  (dotimes (i 157)
    (build-pokemon (+ i 10001)))
  (all-pokemon))


(defun build-ability (id-or-name)
  (let ((r (if (integerp id-or-name)
	       (find-ability-id id-or-name)
	       (find-ability-name id-or-name))))
    (if r
	r
	(multiple-value-bind (id name pokemon effect)
	    (get-info "ability" id-or-name)
	  (make-instance 'ability
			 :oid id
			 :name name
			 :pokemon (mapcar #'(lambda (x)
					      (find-pokemon-name x))
					  (mapcar #'(lambda (p)
						      (getval "name" (getval "pokemon" p)))
						  pokemon))
			 :effect (getval "effect" (car effect))
			 :fn nil)))))

(defun build-abilities ()
  (format t "Building abilities...~%")
  (dotimes (i 233)
    (build-ability (1+ i)))
  (dotimes (i 60)
    (build-ability (+ i 10001)))
  (all-abilities))


(defun build-status (id-or-name)
  (let ((r (if (integerp id-or-name)
	       (find-status-id id-or-name)
	       (find-status-name id-or-name))))
    (if r
	r
	(multiple-value-bind (id name)
	    (get-info "move-ailment" id-or-name)
	  (make-instance 'status
			 :oid id
			 :name name
			 :moves nil)))))

(defun build-statuses ()
  (format t "Building statuses...~%")
  (dolist (a (mapcar #'(lambda (r)
			 (getval "name" r))
		     (getval "results" (get-info "move-ailment"))))
    (build-status a))
  (all-statuses))


(defun build-category (id-or-name)
  (let ((r (if (integerp id-or-name)
	       (find-category-id id-or-name)
	       (find-category-name id-or-name))))
    (if r
	r
	(multiple-value-bind (id name description)
	    (get-info "move-category" id-or-name)
	  (make-instance 'category
			 :oid id
			 :name name
			 :description (getval "description" (car description))
			 :moves nil)))))

(defun build-categories ()
  (format t "Building move categories...~%")
  (dotimes (i 14)
    (build-category i))
  (all-categories))


(defun build-damage-class (id-or-name)
  (let ((r (if (integerp id-or-name)
	       (find-damage-class-id id-or-name)
	       (find-damage-class-name id-or-name))))
    (if r
	r
	(multiple-value-bind (id name)
	    (get-info "move-damage-class" id-or-name)
	  (make-instance 'damage-class
			 :oid id
			 :name name
			 :moves nil)))))

(defun build-damage-classes ()
  (format t "Building damage classes...~%")
  (dotimes (i 3)
    (build-damage-class (1+ i)))
  (all-damage-classes))


(defun build-target (id-or-name)
  (let ((r (if (integerp id-or-name)
	       (find-target-id id-or-name)
	       (find-target-name id-or-name))))
    (if r
	r
	(multiple-value-bind (id name description)
	    (get-info "move-target" id-or-name)
	  (make-instance 'target
			 :oid id
			 :name name
			 :description (getval "description" (cadr description))
			 :moves nil)))))

(defun build-targets ()
  (format t "Building move targets...~%")
  (dotimes (i 14)
    (build-target (1+ i)))
  (all-targets))


(defun build-move (id-or-name)
  (let ((r (if (integerp id-or-name)
	       (find-move-id id-or-name)
	       (find-move-name id-or-name))))
    (if r
	r
	(multiple-value-bind (id name type damage-class power accuracy pp priority
				 target effect stat-changes meta effect-chance)
	    (get-info "move" id-or-name)
	  (make-instance 'move
			 :oid id
			 :name name
			 :type (find-type-name (getval "name" type))
			 :damage-class (find-damage-class-name (getval "name" damage-class))
			 :power power
			 :accuracy accuracy
			 :pp pp
			 :priority priority
			 :target (find-target-name (getval "name" target))
			 :effect (getval "effect" (car effect))
			 :stat-changes (mapcar #'(lambda (s)
						   (cons (getval "name" (getval "stat" s))
							 (getval "change" s)))
					       stat-changes)
			 :max-turns (getval "max_turns" meta)
			 :min-turns (getval "min_turns" meta)
			 :healing (getval "healing" meta)
			 :flinch-chance (getval "flinch_chance" meta)
			 :status-chance (getval "ailment_chance" meta)
			 :category (find-category-name (getval "name" (getval "category" meta)))
			 :stat-chance (getval "stat_chance" meta)
			 :drain (getval "drain" meta)
			 :max-hits (getval "max_hits" meta)
			 :min-hits (getval "min_hits" meta)
			 :status (find-status-name (getval "name" (getval "ailment" meta)))
			 :crit-rate (getval "crit_rate" meta)
			 :effect-chance effect-chance
			 :pokemon nil
			 :fn nil)))))

(defun build-moves ()
  (format t "Building moves...~%")
  (dotimes (i 728)
    (build-move (1+ i)))
  (dotimes (i 18)
    (build-move (+ i 10001)))
  (all-moves))


(defun build-attribute (id-or-name)
  (let ((r (if (integerp id-or-name)
	       (find-attribute-id id-or-name)
	       (find-attribute-name id-or-name))))
    (if r
	r
	(multiple-value-bind (id name description)
	    (get-info "item-attribute" id-or-name)
	  (make-instance 'attribute
			 :oid id
			 :name name
			 :description (getval "description" (car description))
			 :items nil)))))

(defun build-attributes ()
  (format t "Building item attributes...~%")
  (dotimes (i 8)
    (build-attribute (1+ i)))
  (all-attributes))


(defun build-icategory (id-or-name)
  (let ((r (if (integerp id-or-name)
	       (find-icategory-id id-or-name)
	       (find-icategory-name id-or-name))))
    (if r
	r
	(multiple-value-bind (id name)
	    (get-info "item-category" id-or-name)
	  (make-instance 'icategory
			 :oid id
			 :name name
			 :items nil)))))

(defun build-icategories ()
  (format t "Building item categories...~%")
  (dotimes (i 46)
    (build-icategory (1+ i)))
  (all-icategories))


(defun build-fling-effect (id-or-name)
  (let ((r (if (integerp id-or-name)
	       (find-fling-effect-id id-or-name)
	       (find-fling-effect-name id-or-name))))
    (if r
	r
	(multiple-value-bind (id name effect)
	    (get-info "item-fling-effect" id-or-name)
	  (make-instance 'fling-effect
			 :oid id
			 :name name
			 :effect (getval "effect" (car effect))
			 :items nil
			 :fn nil)))))

(defun build-fling-effects ()
  (format t "Building fling effects...~%")
  (dotimes (i 7)
    (build-fling-effect (1+ i)))
  (all-fling-effects))


(defun build-item (id-or-name)
  (let ((r (if (integerp id-or-name)
	       (find-item-id id-or-name)
	       (find-item-name id-or-name))))
    (if r
	r
	(multiple-value-bind (id name category attributes fling-power fling-effect effect)
	    (get-info "item" id-or-name)
	  (make-instance 'item
			 :oid id
			 :name name
			 :category (find-icategory-name (getval "name" category))
			 :attributes (mapcar #'(lambda (a)
						 (find-attribute-name
						  (getval "name" a)))
					     attributes)
			 :fling-power (when fling-power fling-power)
			 :fling-effect (when fling-effect (find-fling-effect-name (getval "name" fling-effect)))
			 :effect (getval "effect" (car effect)))))))

(defun build-items ()
  (format t "Building items...~%")
  (dolist (i (mapcar #'(lambda (r)
			 (getval "name" r))
		     (getval "results" (get-info "item"))))
    (build-item i))
  (all-items))



(defun build-db ()
  (build-types)
  (build-stats)
  (build-natures)
  (build-genders)
  (build-statuses)
  (build-categories)
  (build-damage-classes)
  (build-targets)
  (build-moves)
  (build-pokemons)
  (build-abilities)
  (build-attributes)
  (build-icategories)
  (build-fling-effects)
  (build-items)
  t)
