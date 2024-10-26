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
  ;; Prints a rule's memory values in human-readable format.
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

(defmethod pretty-print-memory ((rule rule))
  ;; For each feature of a rule, display a "slider" showing the Tsetlin Automaton's memory values
  (loop for f from 0 below (* 2 (num-features rule)) do
    (format t "~a (Forget):" f)
    (loop for i from 1 upto (* 2 (num-states rule)) do
      (if (= i (elt (mem rule) f))
          (format t "*")
          (format t "-"))    
      (when (= i (num-states rule))
        (format t "|")))
    (format t ":(Remember)~%")))

(defmethod print-rule-rectangle ((rule rule) &optional width (inc "#") (exc "|") (inert "."))
  ;;
  (format t "This rule is for class ~a.~%" (class-id rule))
  (format t "~a must be present, ~a must be absent, and ~a can be either value.~%" inc exc inert)
  (let* ((size (num-features rule))
	 (width (if (not width) (floor (sqrt size)) width))
	 (height (floor (/ size width))))
    (dotimes (line height)
      (dotimes (f width)
	(let ((feature-index (+ (* line width) f)))
	  (format t "~a"
		  (if (> (elt (mem rule) feature-index) (num-states rule)) inc
		      (if (> (elt (mem rule) (+ feature-index size)) (num-states rule)) exc inert)))))
      (format t "~%"))))

(defmethod eval-rule ((rule rule) input)
  ;; Evaluates a rule on a given input, returning NIL if the input conflicts with any of the rule's memorized assertions and T otherwise
  (dotimes (feature (num-features rule) T)
    (if (and (> (elt (mem rule) feature) (num-states rule)) (equal (elt input feature) 0))
	(return-from eval-rule NIL))
    (if (and (> (elt (mem rule) (+ feature (num-features rule))) (num-states rule)) (equal (elt input feature) 1))
	(return-from eval-rule NIL))))

(defclass tm (standard-object)
  ;; Tsetlin machine class, which contains several rules meant to categorize inputs.
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
   (class-indices :initform NIL :accessor class-indices
		  :documentation "Vector of indices for where in the rules list each class is. Automatically set in initialize-instance.")
   (class-names :initarg :class-names :initform NIL :accessor class-names
		:documentation "List of class names, which get assigned in order. The list's length must be equal to num-classes. Optional to include.")
   (feature-names :initarg :feature-names :initform NIL :accessor feature-names
		  :documentation "List of feature names, which get assigned in order. The list's length must be equal to num-features. Optional.")
   (rules :initarg :rules :initform NIL :accessor rules
	  :documentation "Rules data of this Tsetlin machine. Can be set to an initial value if you really want, but in most cases initialize-instance should handle this.")))

(defmethod print-tm ((tm tm) pretty)
  ; Prints the entire Tsetlin machine, using either pretty-print-memory or print-rule.
  (dotimes (one-rule (num-rules tm))
    (funcall (if pretty #'pretty-print-memory #'print-rule)
	     (elt (rules tm) one-rule))
    (format t "~%")))


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
  ; resolve class-indices
  (setf (class-indices tm) (make-array (num-classes tm) :initial-element 0))
  (let ((running-count 0))
    (dotimes (one-class (num-classes tm))
      (if (> one-class 0)
	  (progn
	    (incf running-count (elt (rules-per-class tm) (1- one-class)))
	    (setf (elt (class-indices tm) one-class) running-count)))))
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
  ;; return vector of all rules that observe the given class-id
  (let* ((lowest-index (elt (class-indices tm) class-id))
	 (highest-index (+ lowest-index (elt (rules-per-class tm) class-id) -1)))
    (make-array (elt (rules-per-class tm) class-id) :initial-contents (loop for i from lowest-index upto highest-index collect (elt (rules tm) i)))))

(defmethod eval-tm ((tm tm) input &optional v)
  ;; Returns a vector of votes, one for each of this machine's rules.
  (let ((votes (make-array (num-rules tm) :fill-pointer 0)))
    (dotimes (rule (num-rules tm) votes)
      (if v (format t "Rule ~a returned ~a for class ~a.~%" rule (eval-rule (elt (rules tm) rule) input) (class-id (elt (rules tm) rule))))
      (vector-push (eval-rule (elt (rules tm) rule) input) votes))))

(defmethod get-consensus ((tm tm) votelist)
  ;; Returns random class among those who have the most votes.
  (let ((highest-vote-count 0) (top-classes nil) (current-votes 0) (rules-per-class (rules-per-class tm)) (class-indices (class-indices tm)))
    (dotimes (one-class (length rules-per-class))
      (dotimes (one-rule (elt rules-per-class one-class))
	(if (elt votelist (+ (elt class-indices one-class) one-rule))
	    (incf current-votes)))
      (if (> current-votes highest-vote-count)
	  ; new highest vote count, erase all previous top-classes
	  (progn
	    (setf highest-vote-count current-votes)
	    (setf top-classes (list one-class)))
	  ; tie with highest vote count, add this class to top-classes
	  (if (= current-votes highest-vote-count)
	      (setf top-classes (append top-classes (list one-class)))))
      (setf current-votes 0))
    (return-from get-consensus (elt top-classes (random (length top-classes))))))

(defun random-exclude (range excluded-num)
  ;; Return random int in [0, range-1], except for excluded-num
  (let ((num (random (1- range))))
    (if (equal num excluded-num)
	(1- range)
	num)))

(defun print-feedback (feature-num feature-present clause-polarity rule-num probability feedback-polarity feedback-success initial-clause-mem &optional roll clause-at-limit)
  ;; Prints explanatory details about given feedback based on a lot of details. Doesn't do any of the actual feedback itself.
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
  

(defmethod give-feedback ((tm tm) input-label input &optional v spec boost-positive)
  ;; Gives feedback to a Tsetlin machine based on a given labeled input. Many of the machine's rules will have their memory values modified.
  (let* ((counterexample-class (random-exclude (num-classes tm) input-label))
	 (label-rules (get-class-rules tm input-label))
	 (counterexample-rules (get-class-rules tm counterexample-class))
	 (label-lowest-index (elt (class-indices tm) input-label))
	 (counterex-lowest-index (elt (class-indices tm) counterexample-class)))
    (if (not spec)
	(setf spec (def-spec tm))) ; temporary
				       ; type 1 feedback, part 1
    ; if a feature is present in the input, we loop through every rule matching the input's labelled class, and reward that feature's clauses with probability (s-1)/s
    (dotimes (label-rule (length label-rules))
      (dotimes (feature (num-features tm))
	; set rand equal to random number in [0, specificity-1]. if we roll 0, then things with probability 1/s will happen. if we roll anything else, things with probability (s-1)/s will happen
	(let ((rand (random spec))
	      (init-clause-mem (elt (mem (elt label-rules label-rule)) feature))
	      (init-negative-mem (elt (mem (elt label-rules label-rule)) (+ feature (num-features tm)))))
	  (if (equal (elt input feature) 1)
	      ; for each feature which is present in the input
	      (progn
		(if v
		    ; dont worry about it lol
		    (print-feedback feature t t (+ label-rule label-lowest-index) (if boost-positive 1 (/ (1- spec) spec)) t (not (equal rand 0)) init-clause-mem (if (not boost-positive) rand) (>= init-clause-mem (* 2 (num-states tm)))))
		; reward with probability 1 if boost-positive is true. otherwise, reward with probability (s-1)/s, so do it if rand is not 0
		(if (or boost-positive (not (equal rand 0)))
		    (if (< init-clause-mem (* 2 (num-states tm)))
			(incf (elt (mem (elt label-rules label-rule)) feature))))
		; penalize the corresponding negative clause with probability 1 (type 2 feedback)
		(if v
		    (print-feedback feature t nil (+ label-rule label-lowest-index) 1 nil t init-negative-mem nil (<= init-negative-mem 1)))
		(if (> init-negative-mem 1)
		    (decf (elt (mem (elt label-rules label-rule)) (+ feature (num-features tm))))))
	  ; for each feature which is absent in the input. this is the "else" in (if (elt input feature)) above if you're confused where we are
	      (progn
		(if v
		    (print-feedback feature nil t (+ label-rule label-lowest-index) (/ 1 spec) nil (equal rand 0) init-clause-mem rand (<= init-clause-mem 1))
		(if (equal rand 0)
		    (if (> init-clause-mem 1)
			(decf (elt (mem (elt label-rules label-rule)) feature))))))))))
    ; the second loop, for the counterexample class. this class is one of the non-present ones, which is randomly selected
    (dotimes (counter-rule (length counterexample-rules))
      (dotimes (feature (num-features tm))
	(let ((rand (random spec))
	      (init-clause-mem (elt (mem (elt counterexample-rules counter-rule)) feature))
	      (init-negative-mem (elt (mem (elt counterexample-rules counter-rule)) (+ feature (num-features tm)))))
	  (if (equal (elt input feature) 1)
	      ; for each feature which is present in the input, (n-1)/n chance to reward the corresponding negative clause in the counterexample class
	      (progn
		(if v
		    (print-feedback feature t nil (+ counterex-lowest-index counter-rule) (/ (1- spec) spec) t (not (equal rand 0)) init-negative-mem rand (>= init-negative-mem (* 2 (num-states tm)))))
		(if (and (not (equal rand 0)) (< init-negative-mem (* 2 (num-states tm))))
		    (incf (elt (mem (elt counterexample-rules counter-rule)) (+ feature (num-features tm)))))
		; for each feature which is present in the input, guaranteed penalty for matching positive clauses in the counterexample class
		(if v
		    (print-feedback feature t t (+ counterex-lowest-index counter-rule) 1 nil t init-clause-mem nil (<= init-clause-mem 1)))
		(if (> init-clause-mem 1)
		    (decf (elt (mem (elt counterexample-rules counter-rule)) feature))))
	      ; for each feature which is absent in the input, 1/n chance to penalize the corresponding negative clause in the counterexample class
	      (progn
		(if v
		    (print-feedback feature nil nil (+ counterex-lowest-index counter-rule) (/ 1 spec) nil (equal rand 0) init-negative-mem rand (<= init-negative-mem 1)))
		(if (and (equal rand 0) (> init-negative-mem 1))
		    (decf (elt (mem (elt counterexample-rules counter-rule)) (+ feature (num-features tm))))))))))))

(defun format-leading-zeroes (num max)
  ;; Turns num into a string, and adds leading zeroes to it to make it the same length as max.
  ; theres probably a fucked up format recipe that makes this trivial but i dont feel like looking that up rn
  (let ((output ""))
    (dotimes (extra-digits (- (length (write-to-string max)) (length (write-to-string num))))
      (setf output (concatenate 'string output "0")))
    (concatenate 'string output (write-to-string num))))

(defun format-time (time-units)
  ; Converts internal time units to printed ms or s.
  (let ((s (float (/ time-units internal-time-units-per-second))))
    (if (> s 10)
	(concatenate 'string (write-to-string s) "s")
	(concatenate 'string (write-to-string (* 1000 s)) "ms"))))

(defmethod one-epoch ((tm tm) input-labels input-data epoch-num epochs start-index num-examples inv-accuracy-sample-rate &optional v spec boost-positive (bar-appearance "....................") (epoch-starting-time (get-internal-real-time)) no-feedback)
  ;; Handles a single training epoch. This isn't much more complicated than running give-feedback on every example, so most of the code here is dedicated to print statements.
  (let ((epoch-correct-answers 0)
	(examples-per-char (max (floor (/ num-examples (length bar-appearance))) 1)))
    ; v is a number here rather than a boolean. this is done because train-distinct technically calls one-epoch twice for each epoch (one training, one testing), so we need a way to control which statements we want to print. this is kind of stupid but one-epoch should never directly be called so whatever.
    (if (equal v 1)
	(format t "Epoch ~a/~a: [" (format-leading-zeroes (+ 1 epoch-num) epochs) epochs))
    (dotimes (example num-examples)
      (let ((example-index (+ start-index example)))
	(if (not no-feedback)
	    (give-feedback tm (elt input-labels example-index) (elt input-data example-index) nil spec boost-positive))
	(if (> v 0)
	    (progn
	      (if (equal (mod example inv-accuracy-sample-rate) 0)
		  (if (equal (get-consensus tm (eval-tm tm (elt input-data example-index)))
			     (elt input-labels example-index))
		      (incf epoch-correct-answers)))
	      (if (and (equal (mod example examples-per-char) 0) (< (/ example examples-per-char) (length bar-appearance)))
		  ; we got customizable progress bars up in here. you won't find this in any other implementation
		  (progn
		    (format t (subseq bar-appearance (/ example examples-per-char) (1+ (/ example examples-per-char))))
		    (finish-output)))))))
    (if (>= v 1)
	(format t "] Acc: ~a%~a Time: ~a"
		(float (* 100 (/ epoch-correct-answers (min 500 num-examples))))
		(concatenate 'string (loop for i upto (- 6 (length (write-to-string (float (* 100 (/ epoch-correct-answers (min 500 num-examples))))))) collect #\ ))
		(format-time (- (get-internal-real-time) epoch-starting-time))))
    (if (>= v 1) (format t "~%"))))
    
(defmethod train ((tm tm) input-labels input-data epochs &optional v spec boost-positive (bar-appearance "...................."))
  ;; Runs add-feedback on tm for every input in the labels and data. Setting v to true adds helpful accuracy and time trackers.
  (let* ((examples-per-epoch (floor (/ (length input-labels) epochs)))
	(overall-starting-time (get-internal-real-time))
	(inv-accuracy-sample-rate (ceiling (/ examples-per-epoch 500))))
    (dotimes (epoch epochs)
      (one-epoch tm input-labels input-data epoch epochs (* epoch examples-per-epoch) examples-per-epoch inv-accuracy-sample-rate (if v 1) spec boost-positive bar-appearance))
    (if v
	(format t "~%~a examples in ~a"
		(write-to-string (* examples-per-epoch epochs))
		(format-time (- (get-internal-real-time) overall-starting-time))))))

(defmethod train-distinct ((tm tm) input-labels input-data epochs &optional (test-proportion 0.2) v spec boost-positive (bar-appearance "...................."))
  ;; Like train, but reserves a chunk of the input data for "testing". The machine will be able to learn only on the unreserved data.
  (let* ((test-cutoff (- (length input-labels) (floor (* (length input-labels) test-proportion))))
	 (testing-per-epoch (floor (/ (- (length input-labels) test-cutoff) epochs)))
	 (training-per-epoch (floor (/ test-cutoff epochs)))
	 (overall-starting-time (get-internal-real-time)))
    (dotimes (epoch epochs)
      (let ((train-starting-index (* training-per-epoch epoch))
	    (test-starting-index (+ test-cutoff (* testing-per-epoch epoch)))
	    (epoch-starting-time (get-internal-real-time)))
	(if v
	    (format t "Epoch ~a/~a: [" (1+ epoch) epochs))
	(one-epoch tm input-labels input-data epoch epochs train-starting-index training-per-epoch 4 (if v 0.5) spec boost-positive (subseq bar-appearance 0 (floor (* (- 1 test-proportion) (length bar-appearance)))))
	(if v
	    (format t "|"))
	(one-epoch tm input-labels input-data epoch epochs test-starting-index testing-per-epoch 1 (if v 1.5) spec boost-positive (subseq bar-appearance (floor (* (- 1 test-proportion) (length bar-appearance)))) epoch-starting-time)))
    (if v
	(format t "~%~a examples in ~a"
		(write-to-string (* (+ testing-per-epoch training-per-epoch) epochs))
		(format-time (- (get-internal-real-time) overall-starting-time))))))
	

(defun default-feature-gen (num-features)
  ;; Generates a random bit vector with length num-features, representing a single example.
  (let ((features (make-sequence '(vector bit) num-features :initial-element 0)))
    (dotimes (feature num-features features)
      (if (= (random 2) 1)
	  (setf (elt features feature) 1)))))

(defun generate-data (size num-features classifying-func &optional feature-gen-func)
  ;; First generates a vector of bit-vectors representing all the examples using either default-feature-gen or an optional user-provided function. Then, labels these examples using classifying-func, a user-provided function that takes in a bit vector and returns a label value. Returns a list of the label vector and the data vector.
  (let ((labels (make-array size :fill-pointer 0)) (data (make-array size :fill-pointer 0)))
    (dotimes (example size (list labels data))
      ; if we have a feature-gen-func, call that with num-features as the argument, otherwise call default-feature-gen with num-features
      (let ((features (if feature-gen-func
			  (funcall feature-gen-func num-features)
			  (default-feature-gen num-features))))
	(vector-push features data)
	; call classifying-func on this example, push the result to labels
	(vector-push (funcall classifying-func features) labels)))))

(defun print-data (label-vector data-vector &optional (num-data 10) (data-per-line 5) (starting-index 0))
  ;; Prints a human-readable sample of training data.
  (dotimes (one-data num-data)
    (if (equal (mod one-data data-per-line) 0)
	(format t "~%"))
    (format t "~a ~a | "
	    (elt label-vector (+ one-data starting-index))
	    (elt data-vector (+ one-data starting-index)))))

(defun print-data-rectangle (label-vector data-vector &optional num-data width (starting-index 0))
  (let* ((size (length (elt data-vector 0)))
	 (width (if (not width) (floor (sqrt size)) width))
	 (height (floor (/ size width)))
	 (num-data (min (length data-vector) (if num-data num-data 10))))
    (dotimes (one-example num-data)
      (dotimes (line height)
	(format t "~%")
	(dotimes (one-bit width)
	  (format t (if (= 1 (elt (elt data-vector (+ starting-index one-example)) (+ (* width line) one-bit))) "#" "."))))
      (format t " (~a)~%~%" (write-to-string (elt label-vector (+ starting-index one-example)))))))

(defun to-base-list (num base-list)
  ;; Converts a number to a specified base list. For instance, (to-base-list (35 '(5 4 3))) will return (2 3 2), because 2*(4*3) + 3*(3) + 2 = 35.
  (if (not (cdr base-list)) (list num)
      (let ((divisor (reduce #'* (cdr base-list))))
      (append (list (floor (/ num divisor))) (to-base-list (mod num divisor) (cdr base-list))))))

(defun from-base-list (num-list base-list)
  ;; Returns an output from to-base-list back to base 10.
  (if (not (cdr num-list)) (car num-list)
      (+ (from-base-list (cdr num-list) (cdr base-list)) (* (car num-list) (reduce #'* (cdr base-list))))))

(defun convolution (img-bit-vector dimensions &optional (convs-per-dimension 2) (granularity 1))
  ;; Takes in a single bit vector representing an image (or higher-dimensional input), and outputs a vector of bit vectors representing smaller sections within that image.
  (let* ((conv-radius (/ 1 convs-per-dimension))
	 (num-dimensions (length dimensions))
	 (grains-per-dimension (1+ (* (1- convs-per-dimension) granularity))) ; a "grain" is a starting point in space for an output to be created. in other words, for every grain, there will be an output created. by default, you get convs-per-dimension ^ num-dimensions grains, but this can be increased by increasing granularity.
	 (output-length (expt grains-per-dimension num-dimensions))
	 (output (make-array output-length :fill-pointer 0))
	 (bit-radius-dimensions (loop for dim below num-dimensions collect (floor (* (elt dimensions dim) conv-radius)))) ; dimensions of each output
	 (bits-per-grain (loop for dim below num-dimensions collect (floor (* (- 1 conv-radius) (/ (elt dimensions dim) (1- grains-per-dimension)))))) ; dimensions of distance between each grain. relevant because if we are working with e.g. a rectangular image, then a horizontal grain and vertical grain may be different lengths
	 (output-image-size (reduce #'* bit-radius-dimensions)) ; total number of bits in the output
	 (thermometer-size (* (1- grains-per-dimension) num-dimensions))
	 (last-img-dimension (car (last dimensions)))
	 (last-output-dimension (car (last bit-radius-dimensions))))
    (dotimes (one-output output-length output) ; for each output bit vector
      (let* ((this-output (make-sequence '(vector bit) (+ output-image-size thermometer-size)))
	     (current-lens (to-base-list one-output (loop for i below (length dimensions) collect grains-per-dimension))) ; the current lens will be essentially a measure of which grain we're looking at for this output. for example if we have a 2d image with conv-radius 0.5 and granularity 1, current-lens will be '(0 0) for the first output, then '(0 1), '(1 0), and finally '(1 1).
	     (current-location (map 'list #'* current-lens bits-per-grain))) ; current-location is like current-lens, but it's made of pixel coordinates for where the top left of the lens is
	(dotimes (one-line last-output-dimension)
	  (let ((starting-img-index (from-base-list (map 'list #'+ current-location (to-base-list (* one-line last-img-dimension) dimensions)) dimensions)) ; begin at current-location, then convert one-line to a base-list, which we read as a list of directions leading us to the start of a line. we start from here, read a line, and move to the next line
		(starting-output-index (* one-line last-output-dimension))) ; the start of the location in the output vector that we're writing
	    (dotimes (one-bit last-output-dimension)
	      ; read each bit from the input bit vector and write it to the output bit vector
	      (setf (elt this-output (+ starting-output-index one-bit)) (elt img-bit-vector (+ starting-img-index one-bit))))))
	; image section has been fully transcribed, now write the thermometer (see "thermometer encoding" in chapter 4 of granmo's book)
	(dotimes (one-dim-indicator num-dimensions)
	  (dotimes (one-positive-bit (elt current-lens one-dim-indicator))
	    (setf (elt this-output (+ output-image-size (* one-dim-indicator (1- grains-per-dimension)) one-positive-bit)) 1)))
	(vector-push this-output output)))))

(defun shuffle-two-sequences (first-seq second-seq)
  ;; Randomly shuffles two sequences in the same way.
  (let ((rng))
    (loop for i from (length first-seq) downto 2 do
					       (progn (setf rng (random i))
						      (rotatef (elt first-seq rng) (elt first-seq (1- i)))
						      (rotatef (elt second-seq rng) (elt second-seq (1- i)))
						      (if (= (mod i 10000) 0)
							  (format t "~a elements left.~%" (write-to-string i)))))
    (list first-seq second-seq)))
