(ns big-prime.core
  (:use [big-prime.utils]))

(defn foo
  "I don't do a whole lot."
  [x]
  (bar 42)
  (pdump (str x ": Hello, Foo!")))
