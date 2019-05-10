(in-package :pokemonstrous)



;; This can be whatever you want, but you should copy the
;; .pmdb directory there from git first.
(defvar *datastore* "~/.pmdb/")



;;; Persistent object classes

(bknr.datastore:define-persistent-class poketype ()
  ((oid :read
	:index-type bknr.indices:unique-index
	:index-reader find-type-id)
   (name :read
	 :index-type bknr.indices:string-unique-index
	 :index-reader find-type-name
	 :index-values all-types)
   (double-from :update)
   (half-from :update)
   (none-from :update)
   (double-to :update)
   (half-to :update)
   (none-to :update)
   (pokemon :update)
   (moves :update)))

(defmethod initialize-instance :after ((type poketype) &key double-from half-from none-from double-to half-to none-to &allow-other-keys)
  (flet ((adjust-types (slot types)
	   (mapcar #'(lambda (tn)
		       (let ((r (find-type-name tn)))
			 (when r
			   (push r (slot-value type slot))
			   (push type
				 (slot-value r
					     (case slot
					       (double-from 'double-to)
					       (half-from 'half-to)
					       (none-from 'none-to)
					       (double-to 'double-from)
					       (half-to 'half-from)
					       (none-to 'none-from)))))))
		   types)))
    (loop
       for slot in '(double-from half-from none-from double-to half-to none-to)
       for lst in (list double-from half-from none-from double-to half-to none-to)
       do (setf (slot-value type slot) nil)
       do (adjust-types slot lst))))


(bknr.datastore:define-persistent-class stat ()
  ((oid :read
	:index-type bknr.indices:unique-index
	:index-reader find-stat-id)
   (name :read
	 :index-type bknr.indices:string-unique-index
	 :index-reader find-stat-name
	 :index-values all-stats)
   (move-increase :update)
   (move-decrease :update)
   (nature-increase :update)
   (nature-decrease :update)))


(bknr.datastore:define-persistent-class nature ()
  ((oid :read
	     :index-type bknr.indices:unique-index
	     :index-reader find-nature-id)
   (name :read
	 :index-type bknr.indices:string-unique-index
	 :index-reader find-nature-name
	 :index-values all-natures)
   (increased :read)
   (decreased :read)))

(defmethod initialize-instance :after ((nature nature) &key &allow-other-keys)
  (let ((i (nature-increased nature))
	(d (nature-decreased nature)))
    (when i (push nature (stat-nature-increase (nature-increased nature))))
    (when d (push nature (stat-nature-decrease (nature-decreased nature))))))


(bknr.datastore:define-persistent-class gender ()
  ((oid :read
	:index-type bknr.indices:unique-index
	:index-reader find-gender-id)
   (name :read
	 :index-type bknr.indices:string-unique-index
	 :index-reader find-gender-name
	 :index-values all-genders)
   (pokemon :update)))

(bknr.datastore:define-persistent-class status ()
  ((oid :read
	:index-type bknr.indices:unique-index
	:index-reader find-status-id)
   (name :read
	 :index-type bknr.indices:string-unique-index
	 :index-reader find-status-name
	 :index-values all-statuses)
   (moves :update)))

(bknr.datastore:define-persistent-class category ()
  ((oid :read
	:index-type bknr.indices:unique-index
	:index-reader find-category-id)
   (name :read
	 :index-type bknr.indices:string-unique-index
	 :index-reader find-category-name
	 :index-values all-categories)
   (description :read)
   (moves :update)))

(bknr.datastore:define-persistent-class damage-class ()
  ((oid :read
	:index-type bknr.indices:unique-index
	:index-reader find-damage-class-id)
   (name :read
	 :index-type bknr.indices:string-unique-index
	 :index-reader find-damage-class-name
	 :index-values all-damage-classes)
   (moves :update)))

(bknr.datastore:define-persistent-class target ()
  ((oid :read
	:index-type bknr.indices:unique-index
	:index-reader find-target-id)
   (name :read
	 :index-type bknr.indices:string-unique-index
	 :index-reader find-target-name
	 :index-values all-targets)
   (description :read)
   (moves :update)))

(bknr.datastore:define-persistent-class move ()
  ((oid :read
	:index-type bknr.indices:unique-index
	:index-reader find-move-id)
   (name :read
	 :index-type bknr.indices:string-unique-index
	 :index-reader find-move-name
	 :index-values all-moves)
   (type :read
	 :index-type bknr.indices:hash-index
	 :index-reader find-move-type)
   (damage-class :read
		 :index-type bknr.indices:hash-index
		 :index-reader find-move-damage-class)
   (power :read)
   (accuracy :read)
   (pp :read)
   (priority :read
	     :index-type bknr.indices:hash-index
	     :index-reader find-move-priority)
   (target :read
	   :index-type bknr.indices:hash-index
	   :index-reader find-move-target)
   (stat-changes :read)
   (max-turns :read)
   (min-turns :read)
   (healing :read)
   (flinch-chance :read)
   (status-chance :read)
   (category :read
	     :index-type bknr.indices:hash-index
	     :index-reader find-move-category)
   (stat-chance :read)
   (drain :read)
   (max-hits :read)
   (min-hits :read)
   (status :read)
   (crit-rate :read)
   (effect-chance :read)
   (effect :read)
   (pokemon :update)
   (fn :update)))

(defmethod initialize-instance :after ((move move) &key &allow-other-keys)
  (push move (poketype-moves (move-type move)))
  (push move (damage-class-moves (move-damage-class move)))
  (push move (target-moves (move-target move)))
  (mapcar #'(lambda (s)
	      (cond
		((> 0 (cdr s)) (push move (stat-move-increase (find-stat-name (car s)))))
		((< 0 (cdr s)) (push move (stat-move-decrease (find-stat-name (car s)))))))
	  (move-stat-changes move))
  (when (move-category move) (push move (category-moves (move-category move))))
  (when (move-status move) (push move (status-moves (move-status move)))))


(bknr.datastore:define-persistent-class pokemon ()
  ((oid :read
	:index-type bknr.indices:unique-index
	:index-reader find-pokemon-id)
   (name :read
	 :index-type bknr.indices:string-unique-index
	 :index-reader find-pokemon-name
	 :index-values all-pokemon)
   (types :read
	  :index-type bknr.indices:hash-index
	  :index-reader find-pokemon-types
	  :index-initargs (:test #'equalp))
   (hp :read)
   (atk :read)
   (def :read)
   (spatk :read)
   (spdef :read)
   (spd :read)
   (weight :read)
   (moves :update)
   (abilities :update)))

(defmethod initialize-instance :after ((pokemon pokemon) &key &allow-other-keys)
  (mapcar #'(lambda (m) (push pokemon (move-pokemon m))) (pokemon-moves pokemon)))


(bknr.datastore:define-persistent-class ability ()
  ((oid :read
	:index-type bknr.indices:unique-index
	:index-reader find-ability-id)
   (name :read
	 :index-type bknr.indices:string-unique-index
	 :index-reader find-ability-name
	 :index-values all-abilities)
   (pokemon :read)
   (effect :read)
   (fn :update)))

(defmethod initialize-instance :after ((ability ability) &key &allow-other-keys)
  (mapcar #'(lambda (p) (push ability (pokemon-abilities p)))
	  (ability-pokemon ability)))


(bknr.datastore:define-persistent-class attribute ()
  ((oid :read
	:index-type bknr.indices:unique-index
	:index-reader find-attribute-id)
   (name :read
	 :index-type bknr.indices:string-unique-index
	 :index-reader find-attribute-name
	 :index-values all-attributes)
   (description :read)
   (items :update)))

(bknr.datastore:define-persistent-class icategory ()
  ((oid :read
	:index-type bknr.indices:unique-index
	:index-reader find-icategory-id)
   (name :read
	 :index-type bknr.indices:string-unique-index
	 :index-reader find-icategory-name
	 :index-values all-icategories)
   (items :update)))

(bknr.datastore:define-persistent-class fling-effect ()
  ((oid :read
	:index-type bknr.indices:unique-index
	:index-reader find-fling-effect-id)
   (name :read
	 :index-type bknr.indices:string-unique-index
	 :index-reader find-fling-effect-name
	 :index-values all-fling-effects)
   (effect :read)
   (items :update)
   (fn :update)))

(bknr.datastore:define-persistent-class item ()
  ((oid :read
	:index-type bknr.indices:unique-index
	:index-reader find-item-id)
   (name :read
	 :index-type bknr.indices:string-unique-index
	 :index-reader find-item-name
	 :index-values all-items)
   (category :read
	      :index-type bknr.indices:hash-index
	      :index-reader find-item-category)
   (attributes :read)
   (fling-power :read)
   (fling-effect :read
		 :index-type bknr.indices:hash-index
		 :index-reader find-item-fling-effect)
   (effect :read)))

(defmethod initialize-instance :after ((item item) &key &allow-other-keys)
  (push item (icategory-items (item-category item)))
  (when (item-fling-effect item)
    (push item (fling-effect-items (item-fling-effect item))))
  (mapcar #'(lambda (a) (push item (attribute-items a))) (item-attributes item)))



;;; Transient classes / Helpers

(defclass bstat ()
  ((stat :initarg :stat :reader stat)
   (iv :initarg :iv :initform 31 :accessor iv)
   (ev :initarg :ev :initform 0 :accessor ev)
   (value :initarg :value :accessor value)
   (changes :initform 0 :accessor changes)))

(defclass battler ()
  ((pokemon :initarg :pokemon :accessor pokemon)
   (nature :initarg :nature :initform (find-nature-name "hardy") :reader nature)
   (types :initarg :types :accessor types)
   (ability :initarg :ability :initform nil :accessor ability)
   (item :initarg :item :initform nil :accessor item)
   (moves :initarg :moves :initform nil :accessor moves)
   (level :initarg :level :initform 50 :reader level)
   (current-hp :accessor current-hp)
   (hp :initarg :hp :initform (make-instance 'bstat :stat (find-stat-name "hp")) :accessor hp)
   (atk :initarg :atk :initform (make-instance 'bstat :stat (find-stat-name "attack")) :accessor atk)
   (def :initarg :def :initform (make-instance 'bstat :stat (find-stat-name "defense")) :accessor def)
   (spatk :initarg :spatk :initform (make-instance 'bstat :stat (find-stat-name "special-attack")) :accessor spatk)
   (spdef :initarg :spdef :initform (make-instance 'bstat :stat (find-stat-name "special-defense")) :accessor spdef)
   (spd :initarg :spd :initform (make-instance 'bstat :stat (find-stat-name "speed")) :accessor spd)
   (acc :initform (make-instance 'bstat :stat (find-stat-name "accuracy") :value 1) :accessor acc)
   (eva :initform (make-instance 'bstat :stat (find-stat-name "evasion") :value 1) :accessor eva)
   (weight :accessor weight)
   (gender :initarg :gender :initform (nth (random 3) (all-genders)) :reader gender)
   (status :initform nil :accessor status)
   (place :initform nil :accessor place)))

(defclass team ()
  ((pokemon :initarg :pokemon :reader pokemon)
   (selected :accessor selected)
   (active :accessor active)
   (effects :initform nil :accessor effects)))

(defmethod initialize-instance :after ((team team) &key &allow-other-keys)
  ;; Assumes a singles battle. The scaffolding supports
  ;; other formats, but we're keeping it simple for now.
  (setf (selected team) (pokemon team)
	(active team) `(,(car (pokemon team)))
	(place (car (pokemon team))) 0))

(defclass battle ()
  ((teams :initarg :teams :reader teams)
   (effects :initform nil :accessor effects)
   (turn :initform 0 :accessor turn)))

(defun calc-hp (battler)
  (+ 10
     (level battler)
     (floor (/ (* (level battler)
		  (+ (* 2 (pokemon-hp (pokemon battler)))
		     (iv (hp battler))
		     (floor (/ (ev (hp battler))
			       4))))
	       100))))

(defun calc-stat (battler stat)
  (floor
   (* (+ 5 (floor (/ (* (level battler)
			(+ (* 2 (slot-value (pokemon battler) stat))
			   (iv (slot-value battler stat))
			   (floor (/ (ev (slot-value battler stat))
				     4))))
		     100)))
      (cond
	((eq (stat (slot-value battler stat))
	     (nature-increased (nature battler)))
	 1.1)
	((eq (stat (slot-value battler stat))
	     (nature-decreased (nature battler)))
	 0.9)
	(t 1.0)))))

(defun set-stats (battler)
  (setf (value (hp battler)) (calc-hp battler)
	(current-hp battler) (value (hp battler)))
  (dolist (s '(atk def spatk spdef spd))
    (setf (value (slot-value battler s)) (calc-stat battler s))))

(defun reset-stats (battler)
  (dolist (s '(atk def spatk spdef spd))
    (setf (changes (slot-value battler s)) 0)))

(defmethod initialize-instance :after ((battler battler) &key
							   (hp-iv 31) (hp-ev 0)
							   (atk-iv 31) (atk-ev 0)
							   (def-iv 31) (def-ev 0)
							   (spatk-iv 31) (spatk-ev 0)
							   (spdef-iv 31) (spdef-ev 0)
							   (spd-iv 31) (spd-ev 0)
							   &allow-other-keys)
  (setf (types battler) (pokemon-types (pokemon battler))
	(weight battler) (pokemon-weight (pokemon battler))
	(iv (hp battler)) hp-iv
	(ev (hp battler)) hp-ev
	(iv (atk battler)) atk-iv
	(ev (atk battler)) atk-ev
	(iv (def battler)) def-iv
	(ev (def battler)) def-ev
	(iv (spatk battler)) spatk-iv
	(ev (spatk battler)) spatk-ev
	(iv (spdef battler)) spdef-iv
	(ev (spdef battler)) spdef-ev
	(iv (spd battler)) spd-iv
	(ev (spd battler)) spd-ev)
  (set-stats battler))



;;; Battle stuff (most of this code will be replaced)

(defun basic-damage (move attacker defender)
  (let* ((immune nil)
	 (damage
	  (floor (* (+ 2 (/ (* (if (eq (move-damage-class move) (find-damage-class-name "physical"))
				   (/ (value (atk attacker)) (value (def defender)))
				   (/ (value (spatk attacker)) (value (spdef defender))))
			       (move-power move)
			       (+ 2 (* (level attacker) (/ 2 5))))
			    50))
		    (if (= 23 (random 24))
			(progn (format t "Critical hit! ") (/ 3 2)
			       1)
			(/ (+ (random 15) 85) 100))
		    (if (member (move-type move) (types attacker))
			(/ 3 2)
			1)
		    (let ((mult
			   (reduce #'(lambda (a b)
				       (* a
					  (cond
					    ((member b (poketype-double-to (move-type move)))
					     2)
					    ((member b (poketype-half-to (move-type move)))
					     (/ 1 2))
					    ((member b (poketype-none-to (move-type move)))
					     (setf immune t)
					     0)
					    (t 1))))
				   (append '(1) (types defender)))))
		      (cond
			((= mult 0) (format t "Immune! "))
			((< mult 1) (format t "Not very effective... "))
			((> mult 1) (format t "Super effective! ")))
		      mult)))))
    (if (and (<= damage 0) (not immune)) 1 damage)))

(defun print-battler (battler)
  (format t "~%~%=== ~A ===~%" (pokemon-name (pokemon battler)))
  (format t "~%Type: ~{~A~^ / ~}" (mapcar #'poketype-name (types battler)))
  (format t "~%Nature: ~A" (nature-name (nature battler)))
  (format t "~%Moves: ~{~A~^, ~}~%" (mapcar #'move-name (moves battler)))
  (format t "~%HP: ~A" (value (hp battler)))
  (format t "~%Attack: ~A" (value (atk battler)))
  (format t "~%Defense: ~A" (value (def battler)))
  (format t "~%Sp. Attack: ~A" (value (spatk battler)))
  (format t "~%Sp. Defense: ~A" (value (spdef battler)))
  (format t "~%Speed: ~A~%" (value (spd battler))))

(defun random-battle ()
  (flet ((mkteam ()
	   (let ((p (find-pokemon-id (1+ (random 151)))))
	     (make-instance 'team
			    :pokemon `(,(make-instance 'battler
						       :pokemon p
						       :nature (find-nature-id (1+ (random 25)))
						       :moves (let ((moves (remove-if-not #'(lambda (m)
											      (and (integerp (move-power m)) (> (move-power m) 0)))
											  (pokemon-moves p))))
								(cond
								  ((= (length moves) 0) `(,(find-move-name "struggle")))
								  ((<= (length moves) 4) moves)
								  (t (loop
									with result = nil
									until (= 4 (length result))
									do (setf result
										 (remove-duplicates
										  (push (random (length moves)) result)))
									finally (return (mapcar #'(lambda (i)
												    (nth i moves))
												result)))))))))))
	 (action (attacker defender)
	   (let* ((moves (moves attacker))
		  (move (nth (random (length moves)) moves)))
	     (format t "~%~A uses ~A... " (pokemon-name (pokemon attacker)) (move-name move))
	     (if (>= (random 100) (* (value (acc attacker)) (value (eva defender)) (or (move-accuracy move) 100)))
		 (format t "It missed! ")
		 (format t "~A damage! " (let ((damage (basic-damage move attacker defender)))
					   (decf (current-hp defender) damage)
					   damage)))))
	 (hp-check (battler1 battler2)
	   (cond ((<= (current-hp battler1) 0)
		  (format t "~%~%*** ~A fainted! ~A wins! ***~%~%" (pokemon-name (pokemon battler1)) (pokemon-name (pokemon battler2)))
		  nil)
		 ((<= (current-hp battler2) 0)
		  (format t "~%~%*** ~A fainted! ~A wins! ***~%~%" (pokemon-name (pokemon battler2)) (pokemon-name (pokemon battler1)))
		  nil)
		 (t t))))
    (let* ((battle (make-instance 'battle
				  :teams `(,(mkteam) ,(mkteam))))
	   (battler1 (car (active (car (teams battle)))))
	   (battler2 (car (active (cadr (teams battle))))))
      (format t "~%~%*** ~A vs. ~A! ***~%~%"
	      (pokemon-name (pokemon battler1))
	      (pokemon-name (pokemon battler2)))
      (print-battler battler1)
      (print-battler battler2)
      (loop
	 until (not (and (active (car (teams battle)))
			 (active (cadr (teams battle)))))
	 do (progn
	      (incf (turn battle))
	      (format t "~%~%= Turn ~d =~%" (turn battle))
	      (format t "~%~A HP: ~A/~A" (pokemon-name (pokemon battler1)) (current-hp battler1) (value (hp battler1)))
	      (format t "~%~A HP: ~A/~A~%" (pokemon-name (pokemon battler2)) (current-hp battler2) (value (hp battler2)))
	      (let ((moved nil))
		(cond
		  ((> (value (spd battler1)) (value (spd battler2)))
		   (setf moved battler1)
		   (action battler1 battler2))
		  ((> (value (spd battler2)) (value (spd battler1)))
		   (setf moved battler2)
		   (action battler2 battler1))
		  (t (if (= (random 2) 0)
			 (progn (setf moved battler1) (action battler1 battler2))
			 (progn (setf moved battler2) (action battler2 battler1)))))
		(unless (hp-check battler1 battler2)
		  (return))
		(if (eql moved battler1)
		    (action battler2 battler1)
		    (action battler1 battler2))
		(unless (hp-check battler1 battler2)
		  (return))))))))
