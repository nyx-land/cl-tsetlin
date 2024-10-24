
(defun class-test ()
	;; Outdated, we now use bit vectors for inputs.
  (defparameter *marxist-class-tm*
    (make-instance 'tm
		   :num-classes 2
		   :num-features 10
		   :def-spec 10
		   :num-rules 10
		   :rules-per-class '(5 5)
		   :class-names '("Proletariat" "Bourgeoisie")
		   :feature-names '("sells-labor-power"
				    "receives-wages"	   		    
				    "performs-manual-labor"
				    "experiences-alienation"
				    "potential-for-class-consciousness"	

				    "owns-means-of-production"
				    "controls-capital"
				    "extracts-surplus-value"
				    "has-political-power"				    
				    "owns-private-property")))
  (initialize-instance *marxist-class-tm*)
  (eval-tm *marxist-class-tm* #(T T NIL NIL NIL T) :v))

(defun xor-categorizer (data)
  ;; Returns 1 if exactly one of the two features is present, and 0 otherwise.
  (if (equal (elt data 0) (elt data 1)) 0 1))

(defun and-categorizer (data)
  ;; Returns 1 if both features are present, and 0 otherwise.
  (if (and (equal (elt data 0) 1) (equal (elt data 1) 1)) 1 0))

(defun link-categorizer (data)
  ;; Returns the lowest-indexed feature for which both it and its immediate successor are present, counting "wrap-arounds" (last + first).
  (dotimes (f (1- (length data)) (1- (length data)))
    (if (equal (+ (elt data f) (elt data (1+ f))) 2)
	(return-from link-categorizer f))))

(defun link-generator (num-features)
  ;; Returns a bit vector containing exactly one "link" (instance of two present features in a row), again counting wrap-arounds.
  (let ((output (make-sequence '(vector bit) num-features :initial-element 0))
	; determine the location of the link first
	(link-center (+ (random num-features) 0.5)))
    (dotimes (f num-features output)
      (let ((proximity (abs (- f link-center))))
	; first set to random value, may override later
	(setf (elt output f) (random 2))
	; if this is part of the link, set it to 1
	(if (or (equal proximity 0.5) (equal proximity (- num-features 0.5)))
	    (setf (elt output f) 1))
	; if this is immediately adjacent to the link, set it to 0 so as not to create a double link
	(if (or (equal proximity 1.5) (equal proximity (- num-features 1.5)))
	    (setf (elt output f) 0))
	; if the previous value was 1, set this one to 0 if it isn't part of the link
	(if (and (> f 0) (equal (elt output (1- f)) 1) (> proximity 0.5))
	    (setf (elt output f) 0))
	; if you're at the end and the first value was 1, set this one to 0 if it isn't part of the link
	(if (and (equal f (1- num-features)) (equal (elt output 0) 1) (> proximity 0.5))
	    (setf (elt output f) 0))))))

; i have to define these here or the compiler gets mad it doesn't even stop the compilation it just gives me warnings and i don't like warnings
(progn
  (defparameter xor-tm nil)
  (defparameter xor-data nil)
  (defparameter and-tm nil)
  (defparameter and-data nil)
  (defparameter link-tm nil)
  (defparameter link-data nil)
  (defparameter llink-tm nil)
  (defparameter llink-data nil))

(defun run-xor-test ()
  ;; Runs the xor test. Uses defparameter so the user can play with the machine and data afterward if they want.
  (defparameter xor-tm (make-instance 'tm :num-classes 2 :num-features 2 :num-rules 4 :feature-names '("Input A" "Input B") :class-names '("False" "True")))
  (defparameter xor-data (generate-data 10000 2 #'xor-categorizer))
  ; the machine currently doesn't perform very well on this one, as i explained on discord
  (train xor-tm (elt xor-data 0) (elt xor-data 1) 10 t))

(defun run-and-test ()
  ;; Runs the and test. Should achieve about 60% accuracy right now.
  (defparameter and-tm (make-instance 'tm :num-classes 2 :num-features 2 :num-rules 4 :feature-names '("Input A" "Input B") :class-names '("False" "True")))
  (defparameter and-data (generate-data 10000 2 #'and-categorizer))
  (train and-tm (elt and-data 0) (elt and-data 1) 10 t))

(defun run-link-test ()
  ;; Runs the link test. Scores close to 100% accuracy.
  (defparameter link-tm (make-instance 'tm :num-classes 10 :num-features 10 :num-rules 20))
  (defparameter link-data (generate-data 10000 10 #'link-categorizer #'link-generator))
  (train link-tm (elt link-data 0) (elt link-data 1) 10 t))

(defun run-llink-test ()
  ;; Runs the lowest link test. Scores about 40%. The lowest link test is like the regular link test, except instead of the dedicated link-generator (which always produces only one link), it uses the default feature gen, so you can have many possible links.
  (defparameter llink-tm (make-instance 'tm :num-classes 10 :num-features 10 :num-rules 20))
  (defparameter llink-data (generate-data 10000 10 #'link-categorizer))
  (train llink-tm (elt llink-data 0) (elt llink-data 1) 10 t))

(defun bw-pixel-to-bit (input &optional (threshold 63))
  (if (> input threshold) 1 0))

(defun images-file-to-array (filename &optional (etype '(unsigned-byte 8)) (pixel-to-bit-func #'bw-pixel-to-bit))
  ;; Takes in a file path and returns a data vector of bit vectors.
  (let ((filestream (open filename :element-type etype)))
    ; first two bytes are 0, third is data type
    (read-byte filestream)
    (read-byte filestream)
    (read-byte filestream)
    ; get dimension sizes
    (let ((dim-count (read-byte filestream)) (dim-sizes))
      (dotimes (dimension dim-count)
        (let ((dim-size 0))
          (dotimes (dim-byte 4)
            (setf (ldb (byte 8 (* 8 (- 3 dim-byte))) dim-size) (read-byte filestream)))
          (setf dim-sizes (append dim-sizes (list dim-size)))))
      ; data starts here
      ; flatten input
      (let ((data-array (make-array (car dim-sizes) :fill-pointer 0)))
        ; read data
        (dotimes (img (car dim-sizes) data-array)
	  (let ((example-bv (make-sequence '(vector bit) (reduce #'* (cdr dim-sizes)))))
	    (dotimes (data-bit (reduce #'* (cdr dim-sizes)))
	      (setf (elt example-bv data-bit) (funcall pixel-to-bit-func (read-byte filestream))))
	    (vector-push example-bv data-array)))))))

(defun labels-file-to-array (filename &optional (etype '(unsigned-byte 8)))
  ;; Takes in a file path and returns a vector of labels.
  (let ((filestream (open filename :element-type etype)))
    (read-byte filestream)
    (read-byte filestream)
    (read-byte filestream)
    ; one dimension, so we won't bother reading it
    (read-byte filestream)
    (let ((size 0))
      (dotimes (dim-byte 4)
        (setf (ldb (byte 8 (* 8 (- 3 dim-byte))) size) (read-byte filestream)))
      ; data starts here
      (let ((data-array (make-array size :fill-pointer 0)))
        ; read data
        (dotimes (img size data-array)
          (vector-push (read-byte filestream) data-array))))))

