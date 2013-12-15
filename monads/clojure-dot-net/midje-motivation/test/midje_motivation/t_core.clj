(ns midje_motivation.t-core
  (:use midje.sweet)
  (:use [midje_motivation.core]
        [swiss-arrows.core]))

(facts "about `swiss-arrows"
  (fact "diamond wand pushes values into <> markers"
    (-<> 2 (* <> 5))                     => 10
    (-<> 2 (* <> 5) [1 2 <> 3 4])        => [1 2 10 3 4]
    (-<> 2 (* <> 5) (vector 1 2 <> 3 4)) => [1 2 10 3 4]
    (-<> 'foo {:a <> :b 'bar})           => {:a 'foo :b 'bar}
    (-<> :a
         (map <> [{:a 1} {:a 2}])
         (map (partial + 2) <>)
         reverse)                        => [4 3]
    )
  (fact "diamond wand's push locale defaults to first position, like ->"
    (-<> 0 [1 2 3])                      => [0 1 2 3]
    (-<> 0 (list 1 2 3))                 => '(0 1 2 3)
    )
  (fact "diamond wand does not work as expected with list literals"
    (-<> 0 '(1 2 3))                     =not=> '(0 1 2 3)
    (-<> 0 '(<> 1 2 3))                  =not=> '(0 1 2 3)
    )
  )

(facts "about `first-element`"
  (fact "it normally returns the first element"
    (first-element [1 2 3] :default) => 1
    (first-element '(1 2 3) :default) => 1)

  ;; I'm a little unsure how Clojure types map onto the Lisp I'm used to.
  (fact "default value is returned for empty sequences"
    (first-element [] :default) => :default
    (first-element '() :default) => :default
    (first-element nil :default) => :default
    (first-element (filter even? [1 3 5]) :default) => :default))
