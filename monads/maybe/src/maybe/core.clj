(ns maybe.core
  (:use clojure.algo.monads))

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


