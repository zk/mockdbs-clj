(ns mockdbs-clj.core-test
  (:use [mockdbs-clj.core] :reload-all)
  (:use [lazytest.describe]))

(defmacro with-clear-neurons [& body]
  `(do
     (clear-neurons)
     ~@body
     (clear-neurons)))

(describe neuron "Creating a neuron"
	  (it "Should have a non-nil name when no name is passed"
	      (let [n (neuron :hello "world")]
		(and (not (nil? (:name n)))
		     (= 0.0 (:depth n))))))

(describe stn "Stn neuron"
	  (it "should correctly initialize the type and depth"
	      (let [n (stn)
		    n2 (stn 10.0)]
		(and (and (= :stn (:type n)) (= 0.0 (:depth n)))
		     (and (= :stn (:type n2)) (= 10.0 (:depth n2)))))))

(describe clear-neurons "Clear neurons"
	  (it "Should clear neurons from the global set"
	      (add-neuron {:type :stn})
	      (add-neuron {:type :thalamus})
	      (clear-neurons)
	      (= 0 (count (get-neurons)))))

(describe add-neuron "with neurons"
	  (with-clear-neurons
	    (it "Adds the passed neuron to the global set"
		(add-neuron {:type :stn})
		(= 1 (count (get-neurons))))))

(describe remove-neuron
	  (with-clear-neurons
	    (it "Removes a neuron from the global set"
		(add-neuron {:type :thalamus})
		(remove-neuron {:type :thalamus})
		(= 0 (count (get-neurons))))))

