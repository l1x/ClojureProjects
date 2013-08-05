(ns big-prime.sqrt
  (:require [big-prime.core :refer [sum]]))

(defn nt-average
  "Number-theoretic mean using `quot' instead of `/', which latter produces rationals"
  ([] 0)
  ([x] x)
  ([x & more]
     (quot (+ x (apply sum more))
           (inc (count more)))))

(defn abs
  "Absolute value"
  [x]
  (if (< x 0) (- x) x))

(defn square [x] (* x x))

(defn- nt-improve
  "Improve a guess of a square root by number-theoretic average with the quotient of the guess with the target: an adaptation of Newton's method to the integer domain."
  [guess x]
  (nt-average guess (quot x guess)))

(defn- good-enough?
  "A guess is good enough if its square is lte the target and the square of its increment is gt the target"
  [guess x]
  (and
   (<= (square guess) x)
   (>  (square (inc guess)) x)))

(defn- nt-try-sqrt [guess x]
  (if (good-enough? guess x)
    guess
    (recur (nt-improve guess x) x)))

(defn nt-sqrt
  "Number-theoretic square root (largest integer whose square is less than or equal to the target)"
  [x]
  (nt-try-sqrt 1 (bigint x)))

