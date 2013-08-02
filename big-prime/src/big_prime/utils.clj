(ns big-prime.utils
  (:require clojure.string
            clojure.pprint))

(defmacro pdump [x]
  `(let [x# (try ~x (catch Exception e# (str "pdump caught exception: " (.getMessage e#))))]
     (do (println "----------------")
         (clojure.pprint/pprint '~x)
         (println "~~>")
         (clojure.pprint/pprint x#)
         (println "----------------")
         x#)))

(defn bar
  "I don't do a whole lot."
  [x]
  (println x "Hello, Bar!"))
