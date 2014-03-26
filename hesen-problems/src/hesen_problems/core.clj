(ns hesen-problems.core
  (:gen-class))



(defn rev-apply
  [xs f]
  (let [{horiz :horizontal vert :vertical} (f (:vertical xs))]
    {:vertical vert,
     :accumulated-results (conj (:accumulated-results xs) horiz)}))

(defn chain-all
  [fn-seq horizontal-arg-seq initial-vertical]
  (reduce
   rev-apply
   initial-vertical
   (map partial fn-seq horizontal-arg-seq)))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
