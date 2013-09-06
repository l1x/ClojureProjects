(ns maybe.core
  (:use clojure.algo.monads))

(defmacro pdump [x]
  `(let [x# (try ~x (catch Exception e#
                      (str "pdump caught exception: " (.getMessage e#))))]
     (do (println "----------------")
         (clojure.pprint/pprint '~x)
         (println "~~>")
         (clojure.pprint/pprint x#)
         (println "----------------")
         x#)))

(defn maybe-t-2
  [m]
  (monad [m-result (with-monad m m-result)
          m-bind   (with-monad m
                     (fn [mv f]
                       (m-bind mv
                               (fn [x]
                                 (if (nil? x)
                                   (m-result nil)
                                   (f x))))))
          m-zero   (with-monad m m-zero)
          m-plus   (with-monad m m-plus)
          ]))

(defmonad if-not-m
  [m-result (fn [[value test]] [value test])
   m-bind   (fn [[value test] f]
              (if-not (test value)
                (pdump [(f value) test])
                (pdump [value     test])))
   m-zero   [nil identity]
   ])

(domonad if-not-m
         [a2 [42                            :error]
          a3 [(quot (first a2) 2)           :error]
          a5 [{:error "not divisible by 5"} :error]
          a7 [(quot (first a5) 7)           :error]
          ]
         a7)
