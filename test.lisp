
(defun class-test ()
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
