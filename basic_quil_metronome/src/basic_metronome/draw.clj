(ns basic-metronome.draw
  (:use [basic-metronome.setup :only [tick]])
  (:require [quil.core :as qc]))

(defmethod print-method clojure.lang.PersistentQueue
  [q, w]
  (print-method '<- w)
  (print-method (seq q) w)
  (print-method '-< w))

(defn- mx [dim ang]  (* 0.5 dim (Math/sin ang)))
(defn- tx [ang]      (mx (qc/width)  ang))
(defn- ty [ang]      (mx (qc/height) ang))

(defn draw
  []
  (swap! tick inc)
  (qc/background 0 0 64)
  (qc/translate (* 0.5 (qc/width)) (* 0.5 (qc/height)))
  (let [theta (* 0.05 @tick)
        _ (dorun (for [j (range (- @tick 20) @tick)
                       t [(* 0.05 j)]]
                   (qc/ellipse (tx t) (ty (* 1.1 t)) 20 20)))
        ]
    (qc/ellipse (tx (inc theta)) (ty (* 1.1 (inc theta))) 20 20)
    (qc/ellipse (tx theta) (ty (* 1.1 theta)) 20 20)))
