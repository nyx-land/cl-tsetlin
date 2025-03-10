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

(defun circlecross-categorizer (data)
  ;; Returns 1 if three features in a row are present, and 0 otherwise.
  (dotimes (one-bit (- (length data) 2) 0)
    ; O(n) = 3n lol
    (if (and (equal (elt data one-bit) 1) (equal (elt data (1+ one-bit)) 1) (equal (elt data (+ 2 one-bit)) 1))
	(return-from circlecross-categorizer 1))))

(defun circlecross-generator (num-features)
  (let* ((output (make-sequence '(vector bit) num-features :initial-element 0))
	(height (second-highest-factor num-features))
	(width (/ num-features height)))
    (if (or (< height 3) (< width 3))
	(error "I can't make a rectangle with this shit.~%Go find two whole numbers greater than 2 which go into ~S.~%Yeah, that's what I thought." num-features))
    (let ((center-coords (list (1+ (random (- height 2))) (1+ (random (- width 2)))))
	  (is-cross (if (> (random 2) 0) t nil)))
      (if is-cross
	  (setf (elt output (from-base-list center-coords (list height width))) 1))
      (progn
	; i know this sucks but dont care
	(setf (elt output (from-base-list (list (1- (elt center-coords 0)) (elt center-coords 1)) (list height width))) 1)
	(setf (elt output (from-base-list (list (1+ (elt center-coords 0)) (elt center-coords 1)) (list height width))) 1)
	(setf (elt output (from-base-list (list (elt center-coords 0) (1+ (elt center-coords 1))) (list height width))) 1)
	(setf (elt output (from-base-list (list (elt center-coords 0) (1- (elt center-coords 1))) (list height width))) 1))
      output)))

(defun 2d-xor-generator (num-features &optional width)
  (let ((output (make-sequence '(vector bit) num-features)) (config (random 4)) (criticals (list 0 0 0 0)) (width (if (not width) (floor (sqrt num-features)))))
    (dotimes (feature num-features) (setf (elt output feature) (random 2)))
    (cond ((= config 0)
	   (setf criticals (list 1 1 0 0)))
	  ((= config 1)
	   (setf criticals (list 0 0 1 1)))
	  ((= config 2)
	   (setf criticals (list 0 1 1 0)))
	  ((= config 3)
	   (setf criticals (list 1 0 0 1))))
    (progn
      (setf (elt output 0) (elt criticals 0))
      (setf (elt output 1) (elt criticals 1))
      (setf (elt output width) (elt criticals 2))
      (setf (elt output (1+ width)) (elt criticals 3)))
    output))
    
(defun 2d-xor-categorizer (input)
  (if (= (elt input 0) (elt input 1)) 0 1))

(defun noisemaker (labels-list noise-ratio &optional (num-classes 2))
  (dotimes (label (length labels-list) labels-list)
    (if (< (random 1.0) noise-ratio)
	(setf (elt labels-list label) (random num-classes)))))
; i have to define these here or the compiler gets mad it doesn't even stop the compilation it just gives me warnings and i don't like warnings
(progn
  (defparameter xor-tm nil)
  (defparameter xor-data nil)
  (defparameter and-tm nil)
  (defparameter and-data nil)
  (defparameter link-tm nil)
  (defparameter link-data nil)
  (defparameter llink-tm nil)
  (defparameter llink-data nil)
  (defparameter mnist-tm nil)
  (defparameter mnist-labels nil)
  (defparameter mnist-images nil)
  (defparameter convs-per-dimension nil)
  (defparameter granularity nil)
  (defparameter outputs-per-convolution nil)
  (defparameter mnist-conv-tm nil)
  (defparameter mnist-conv-labels nil)
  (defparameter mnist-conv-images nil)
  (defparameter circlecross-tm nil)
  (defparameter circlecross-data nil)
  (defparameter circlecross-conv-tm nil)
  (defparameter circlecross-conv-labels nil)
  (defparameter circlecross-conv-images nil)
  (defparameter noisy-xor-data nil)
  (defparameter noisy-xor-labels nil)
  (defparameter noisy-xor-images nil)
  (defparameter noisy-xor-conv-imgs nil)
  (defparameter noisy-xor-tm nil)
  (defparameter noisy-xor-conv-tm nil))

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

(defun bw-pixel-to-bit (input &optional (threshold 127))
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

(defun run-mnist-test (labels-filename images-filename)
  ;; Runs the MNIST handwritten digits test. Currently scores only slightly higher than random (10%).
  (defparameter mnist-labels (labels-file-to-array labels-filename))
  (defparameter mnist-images (images-file-to-array images-filename))
  (defparameter mnist-tm (make-instance 'tm :num-classes 10 :num-features 784 :num-rules 40))
  (train mnist-tm mnist-labels mnist-images 10 t))

(defun run-mnist-conv-test (labels-filename images-filename)
  ;; Runs the MNIST handwritten digits test, but with convolution added. Currently scores slightly higher than random.
  (defparameter convs-per-dimension 2)
  (defparameter granularity 2)
  (defparameter outputs-per-convolution (expt (1+ (* (1- convs-per-dimension) granularity)) 2)) ; todo: change this fucking variable, its only purpose is to determine the size of the thermometer and it isn't even good at doing that
  (defparameter mnist-labels (labels-file-to-array labels-filename))
  (defparameter mnist-images (images-file-to-array images-filename))
  (defparameter mnist-conv-labels (make-array (length mnist-labels) :fill-pointer 0))
  (defparameter mnist-conv-images (make-array (length mnist-images) :fill-pointer 0))
  (defparameter mnist-conv-tm (make-instance 'tm
					     :num-rules 40
					     :num-features (floor (+ (expt (floor (/ 28 convs-per-dimension)) 2) (* 2 (1- (sqrt outputs-per-convolution)))))
					     :num-classes 10
					     :def-spec 5))
  (dotimes (one-image (length mnist-images))
    (let ((conv-vector (convolution (elt mnist-images one-image) '(28 28) convs-per-dimension granularity)))
      (vector-push (elt mnist-labels one-image) mnist-conv-labels)
      (vector-push conv-vector mnist-conv-images)))
  (train mnist-conv-tm mnist-conv-labels mnist-conv-images 10 t))

(defun run-circlecross-test ()
  (let ((num-features 25) (data-size 5000))
    (defparameter circlecross-data (generate-data data-size num-features #'circlecross-categorizer #'circlecross-generator))
    (defparameter circlecross-tm (make-instance 'tm
						:num-rules 60
						:num-features num-features
						:num-classes 2
						:class-names #("Circle" "Cross")
						:def-spec 5))
    (train circlecross-tm (elt circlecross-data 0) (elt circlecross-data 1) 10 t)))

(defun run-circlecross-conv-test ()
    (let* ((num-features 25) (data-size 50000) (cvd 2) (gran 1))
    (defparameter circlecross-data (generate-data data-size num-features #'circlecross-categorizer #'circlecross-generator))
    (defparameter circlecross-conv-labels (elt circlecross-data 0))
    (defparameter circlecross-conv-images (make-array data-size :fill-pointer 0))
    (dotimes (one-image data-size)
      (vector-push (convolution
		    (elt (elt circlecross-data 1) one-image)
		    (list (/ num-features (second-highest-factor num-features)) (second-highest-factor num-features))
		    cvd
		    gran)
		   circlecross-conv-images))
    (defparameter circlecross-conv-tm (make-instance 'tm
						:num-rules 60
						:num-features (+ (expt (floor (/ (sqrt num-features) cvd)) 2) (* 2 (ceiling (+ cvd gran -2))))
						:num-classes 2
						:class-names #("Circle" "Cross")
						:def-spec 15))
    (train circlecross-conv-tm (elt circlecross-data 0) (elt circlecross-data 1) 10 t nil nil 10)))

(defun run-noisy-xor-test (&optional (noise-ratio 0.4))
  (let ((data-size 50000) (num-features 9) (width 3))
    (defparameter noisy-xor-data (generate-data data-size num-features #'2d-xor-categorizer #'2d-xor-generator))
    (defparameter noisy-xor-labels (noisemaker (elt noisy-xor-data 0) noise-ratio))
    (defparameter noisy-xor-images (elt noisy-xor-data 1))
    (defparameter noisy-xor-conv-imgs (map 'vector #'convolution noisy-xor-images (make-array data-size :initial-element (list width (/ num-features width)))))
    (defparameter noisy-xor-tm (make-instance 'tm
					      :num-rules 20
					      :num-features num-features
					      :num-classes 2
					      :class-names #("Horizontal" "Slash")
					      :def-spec 5))
    (defparameter noisy-xor-conv-tm (make-instance 'tm
						   :num-rules 20
						   :num-features (length (elt (elt noisy-xor-conv-imgs 0) 0))
						   :num-classes 2
						   :class-names #("Horizontal" "Slash")
						   :def-spec 5))
    (train noisy-xor-tm noisy-xor-labels noisy-xor-images 10 t nil nil 2)
    (format t "~%~%")
    (train noisy-xor-conv-tm noisy-xor-labels noisy-xor-conv-imgs 10 t)))
