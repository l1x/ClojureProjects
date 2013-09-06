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
  [m-result (fn [[value test transform]] [value test transform])
   m-bind   (fn [[value test transform] f]
              (if-not (test value)
                (pdump [(transform value) test transform])
                (pdump [value             test transform])))
   ])

(domonad if-not-m
         [a2 [42 :error #(quot % 2)]
          a3 [(first a2) :error #(quot % 3)]
          a5 [(first a3) :error (fn [_] {:error "not divisible by 5"})]
          a7 [(first a5) :error #(quot % 7)]
          ]
         a7)
