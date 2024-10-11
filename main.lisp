(defclass rule (standard-object)
  ((num-features :initarg :num-features :initform (error "num-features not specified. Are you stupid?") :accessor num-features
		 :documentation "Number of true-false features that this rule will be able to observe. Should be a positive integer.")
   (num-states :initarg :num-states :initform 5 :accessor num-states
	       :documentation "Number of possible memory states per overall position (memorized/forgotten). Total number of states will be twice this value.")
   (class-id :initarg :class-id :initform (error "class id not specified. Are you stupid?") :accessor class-id
	     :documentation "The ID of the class this rule is trying to observe. Should be an integer 0 or higher.")
   (feature-names :initarg :feature-names :initform NIL :accessor feature-names
		  :documentation "List of feature names, which get assigned in order. The list's length must be equal to num-features. Optional.")
   (cname :initarg :cname :initform NIL :accessor cname
	  :documentation "The name of the class this rule is trying to observe. Should be a string. Not mandatory.")
   (memory :initarg :mem :initform NIL :accessor mem
	       :documentation "Memory values of the rule's feature clauses. Should be handled by initialize-instance, but can be overridden by an initial value. Should be a collection of ints.")))

(defmethod initialize-instance :after ((rule rule) &key)
					; set up mem values
  (if (not (mem rule))
      (setf (mem rule) (make-array (* 2 (num-features rule)) :initial-element (num-states rule))))) ; initialize all values to num-states, so barely forgotten

(defmethod print-rule ((rule rule) &optional (clauses-per-line 5))
  (format t "This rule is for class ~a. It has ~a features.~%~%"
	  (if (cname rule) (concatenate 'string (write-to-string (class-id rule)) ": " (cname rule)) (class-id rule))
	  (num-features rule))
  (let ((memorized-clauses NIL))
    ; collect the feature-names for the clauses which have been memorized. these clauses must be true in order for the rule to evaluate as true
    (dotimes (clause (* 2 (num-features rule)))
      (if (> (elt (mem rule) clause) (num-states rule)) 
	  (setf memorized-clauses (concatenate 'list memorized-clauses (list (elt (feature-names rule) clause))))))
    ; now make the print statement
    (format t "It returns true ~a.~%"
	    (if (not memorized-clauses) "always"
		(format nil "only if ~{~a~}" (loop for i upto (- (length memorized-clauses) 1)
					   collect (if (equal i 0) (elt memorized-clauses i) ; don't say "and" before if this is the first
						       (concatenate 'string " and " (elt memorized-clauses i))))))))
  ; now loop through each feature clause to list its memorization value
  (dotimes (clause (* 2 (num-features rule)))
    ; create a newline every so often according to clauses-per-line's value
    (if (equal (mod clause clauses-per-line) 0)
	(format t "~%"))
    ; print feature's name and memorization value of its clause
    (format t "#~a: ~a | "
	    (if (feature-names rule)
		(concatenate 'string (write-to-string clause) " (" (elt (feature-names rule) clause) ")")
		(write-to-string clause))
	    (elt (mem rule) clause)))
    ; print newline at the end
    (format t "~%"))

(defmethod eval-rule ((rule rule) input)
  (dotimes (feature (num-features rule) T)
    (if (and (> (elt (mem rule) feature) 5.5) (not (elt input feature)))
	(return-from eval-rule NIL))))

(defclass tm (standard-object)
  ((num-classes :initarg :num-classes :initform (error "num-classes not specified. Are you stupid?") :accessor num-classes
		:documentation "Number of distinct classes that this Tsetlin machine will be able to observe. Should be a positive integer.")
   (num-states :initarg :num-states :initform 5 :accessor num-states
	       :documentation "Number of possible memory states per overall position (memorized/forgotten) in this Tsetlin machine's rules. Total number of states will be twice this value.")
   (num-features :initarg :num-features :initform (error "num-features not specified. Are you stupid?") :accessor num-features
		 :documentation "Number of true-false features that this Tsetlin machine will be able to observe. Should be a positive integer.")
   (def-spec :initarg :def-spec :initform 3 :accessor def-spec
			:documentation "Default specificity, or inverse feedback rate, of this machine. If specified, should be at least 1.")
   (num-rules :initarg :num-rules :initform NIL :accessor num-rules
	      :documentation "Number of rules in the Tsetlin machine. Should be a positive integer. If unspecified, defaults to num-classes.")
   (rules-per-class :initarg :rules-per-class :initform NIL :accessor rules-per-class
		    :documentation "Collection of integers describing how many rules will cover each class. If unspecified, the rules are divided equally.")
   (class-names :initarg :class-names :initform NIL :accessor class-names
		:documentation "List of class names, which get assigned in order. The list's length must be equal to num-classes. Optional to include.")
   (feature-names :initarg :feature-names :initform NIL :accessor feature-names
		  :documentation "List of feature names, which get assigned in order. The list's length must be equal to num-features. Optional.")
   (rules :initarg :rules :initform NIL :accessor rules
	  :documentation "Rules data of this Tsetlin machine. Can be set to an initial value if you really want, but in most cases initialize-instance should handle this.")))


(defmethod initialize-instance :after ((tm tm) &key)
  ; resolve num-rules
  (if (not (num-rules tm))
      (setf (num-rules tm) (num-classes tm)))
  ; resolve rules-per-class
  (if (not (rules-per-class tm))
      (progn
	(setf (rules-per-class tm) (make-array (num-classes tm) :initial-element (floor (/ (num-rules tm) (num-classes tm)))))
	(dotimes (remainder (mod (num-rules tm) (num-classes tm)))
	  (incf (elt (rules-per-class tm) remainder)))))
  ; resolve feature-names
  (if (not (feature-names tm))
      (setf (feature-names tm) (make-array (num-features tm) :initial-contents (loop for i upto (- (num-features tm) 1) collect (write-to-string i)))))
  (setf (feature-names tm) (concatenate 'vector (feature-names tm) (make-array (num-features tm) :initial-contents (loop for i upto (- (num-features tm) 1) collect (concatenate 'string "not " (elt (feature-names tm) i))))))
  ; create the rules
  (if (not (rules tm))
      (progn
	(setf (rules tm) (make-array (num-rules tm) :fill-pointer 0))
	(dotimes (one-class (num-classes tm))
	  (dotimes (one-rule (elt (rules-per-class tm) one-class))
	    (vector-push (make-instance 'rule
					:num-states (num-states tm)
					:class-id one-class
					:num-features (num-features tm)
					:feature-names (feature-names tm)
					:cname (if (class-names tm) (elt (class-names tm) one-class) NIL))
			 (rules tm)))))))

(defmethod get-class-rules ((tm tm) class-id)
  ; return vector of all rules that observe the given class-id
  (let* ((lowest-index (loop for i upto (- class-id 1) sum (elt (rules-per-class tm) i)))
	 (highest-index (+ lowest-index (elt (rules-per-class tm) class-id) -1)))
    ; as i said on discord, we will probably store indices in the tm object so we don't have to calculate them every time
    (make-array (elt (rules-per-class tm) class-id) :initial-contents (loop for i from lowest-index upto highest-index collect (elt (rules tm) i)))))

(defmethod eval-tm ((tm tm) input &optional v)
  (let ((votes (make-array (num-rules tm) :fill-pointer 0)))
    (dotimes (rule (num-rules tm) votes)
      (if v (format t "Rule ~a returned ~a for class ~a.~%" rule (eval-rule (elt (rules tm) rule) input) (class-id (elt (rules tm) rule))))
      (vector-push (eval-rule (elt (rules tm) rule) input) votes))))

(defun random-exclude (range excluded-num)
  ; return random int in [0, range-1], except for excluded-num
  (let ((num (random (1- range))))
    (if (equal num excluded-num)
	(1- range)
	num)))

(defun print-feedback (feature-num feature-present clause-polarity rule-num probability feedback-polarity feedback-success initial-clause-mem &optional roll clause-at-limit)
  (format t "Feature ~a was ~a in the input, so the corresponding ~a clause in rule ~a may be ~a with prob. ~a.~%"
	  (write-to-string feature-num)
	  (if feature-present "present" "absent")
	  (if clause-polarity "positive" "negative")
	  (write-to-string rule-num)
	  (if feedback-polarity "rewarded" "penalized")
	  (write-to-string probability))
  (if clause-at-limit
      (format t "The clause was already at its ~a value, ~a, so no change was made.~%"
	      (if feedback-polarity "maximum" "minimum")
	      (write-to-string initial-clause-mem))
      (progn
	(format t "~ahe clause ~a receive a ~a.~%"
		(if roll (format nil "~a was rolled, so t"
				 (write-to-string roll))
		    "T")
		(if feedback-success "will" "won't")
		(if feedback-polarity "reward" "penalty"))
	(if feedback-success
	    (format t "The clause was ~a, and its memorization value was moved from ~a to ~a.~%"
		    (if feedback-polarity "rewarded" "penalized")
		    (write-to-string initial-clause-mem)
		    (write-to-string
		     (if feedback-polarity
			 (1+ initial-clause-mem)
			 (1- initial-clause-mem)))))))
  (format t "~%"))
  

(defmethod give-feedback ((tm tm) input-label input &optional v (spec (def-spec tm)) boost-positive)
  ; counterexample stuff is currently unused, it's for the part of the feedback that isn't done yet
  (let* ((counterexample-class (random-exclude (num-classes tm) input-label))
	 (label-rules (get-class-rules tm input-label))
	 (counterexample-rules (get-class-rules tm counterexample-class))
	 ; todo: make it so we don't need to calculate indices every time
	 (label-lowest-index (loop for i upto (- input-label 1) sum (elt (rules-per-class tm) i)))
	 (counterex-lowest-index (loop for i upto (- counterexample-class 1) sum (elt (rules-per-class tm) i))))
				       ; type 1 feedback, part 1
    ; if a feature is present in the input, we loop through every rule matching the input's labelled class, and reward that feature's clauses with probability (s-1)/s
    (dotimes (label-rule (length label-rules))
      (dotimes (feature (num-features tm))
	; set rand equal to random number in [0, specificity-1]. if we roll 0, then things with probability 1/s will happen. if we roll anything else, things with probability (s-1)/s will happen
	(let ((rand (random spec))
	      (init-clause-mem (elt (mem (elt label-rules label-rule)) feature)))
	  (if (elt input feature)
	      ; for each feature which is present in the input
	      (progn
		(if v
		    ; dont worry about it lol
		    (print-feedback feature t t (+ label-rule label-lowest-index) (if boost-positive 1 (/ (1- spec) spec)) t (not (equal rand 0)) init-clause-mem (if (not boost-positive) rand) (>= init-clause-mem (* 2 (num-states tm)))))
		; reward with probability 1 if boost-positive is true. otherwise, reward with probability (s-1)/s, so do it if rand is not 0
		(if (or boost-positive (not (equal rand 0)))
		    (if (< init-clause-mem (* 2 (num-states tm)))
			(incf (elt (mem (elt label-rules label-rule)) feature))))
		; todo: penalize the corresponding negative clause with probability 1 (type 2 feedback). do it inside the below parenthesis
		)
	  ; for each feature which is absent in the input. this is the "else" in (if (elt input feature)) above if you're confused where we are
	      (progn
		(if v
		    (print-feedback feature nil t (+ label-rule label-lowest-index) (/ 1 spec) nil (equal rand 0) init-clause-mem rand (<= init-clause-mem 1))
		(if (equal rand 0)
		    (if (> init-clause-mem 1)
			(decf (elt (mem (elt label-rules label-rule)) feature))))))))))))
    
