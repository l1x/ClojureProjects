(ns nuprng.core
  (:use clojure.pprint)
  (:require [clojure.math.numeric-tower :as mathEx]))

;;; This is the form of data returned by "frequencies,"
(def loaded-die {:A 37, :B 0, :C 17, :D 5, :E 12, :F 11 :G 44})

(defn total
  "The sum of frequencies in a distribution, which is a sequence of
pairs of outcomes and frequencies."
  [frqs] (apply + (map second frqs)))

(defn N
  "The number of outcomes in a distribution."
  [frqs] (count frqs))

(defn S
  "The total of the frequencies in a distribution."
  [frqs] (total frqs))

(defn L
  "A new total of augmented frequencies after redistribution."
  [frqs] (mathEx/lcm (N frqs) (S frqs)))

(defn H
  "The heights of bins after redistribution."
  [frqs] (/ (L frqs) (N frqs)))

(defn augmentation-factor [frqs] (/ (L frqs) (S frqs)))

(defn augmented      [frqs] (map #(vector
                                (first %)
                                (* (second %) (augmentation-factor frqs)))
                              frqs))

;;; Preprocessing

(defn fill-shortest [target [filled frqs]]
  (let [sorted   (sort-by second frqs)
        tallest  (last    sorted)
        shortest (first   sorted)
        deficit  (- target (second shortest))
        to-do    (drop 1 sorted)]
    {:filled
     (conj filled {:home shortest, :other [(first tallest) deficit]})
     :remaining
     (if (empty? to-do)
       to-do
       (conj (drop-last to-do)
             [(first tallest) (- (second tallest) deficit)]))}))

(defn redistribute [target frqs] ; TODO: precondition frqs not empty?
  (loop [result (fill-shortest target [[] frqs])]
    (if (empty? (:remaining result))
      (:filled result)
      (recur (fill-shortest target
                            [(:filled result) (:remaining result)])))))

(defn sample-1 [target redistributed]
  (let [bucket (rand-nth redistributed)
        height (rand-int target)]
    (if (< height (second (:home bucket)))
      (first (:home bucket))
      (first (:other bucket))
      )))

(defn sample [n redistributed]
  (let [ansatz (first redistributed)
        target (+ (second (:home ansatz)) (second (:other ansatz)))]
    (map (fn [_] (sample-1 target redistributed))
         (range n))))

(defn -main [] (pprint
                (let [redis (augmented loaded-die)
                      h     (H loaded-die)
                      n     (N loaded-die)
                      s     (S loaded-die)
                      l     (L loaded-die)]
                 {"loaded-die" loaded-die
                  ,"original count"  n
                  ,"original total"  s
                  ,"gcd count total" (mathEx/gcd n s)
                  ,"lcm count total" l
                  ,"target height:"  h
                  ,"target total:"   (* h n)
                  ,"augmented heights" redis
                  ,"total augmented heights" (total redis)
                  ,"tallest and shortest" (fill-shortest h [[] redis])
                  ,"redistributed" (redistribute h redis)
                  ,"sample 10000"
                  (time
                   (frequencies
                    (sample (* 10000 s)
                            (redistribute h redis))))
                  })))
