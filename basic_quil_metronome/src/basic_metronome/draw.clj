(ns basic-metronome.draw
  (:use [basic-metronome.setup :only [tick]])
  (:require [quil.core :as qc]))

(defn draw
  []
  (swap! tick inc)
  (qc/background 0 0 0)
  (qc/translate (* 0.5 (qc/width)) (* 0.5 (qc/height)))
  (let [theta (* 0.05 @tick)
        x (* 0.5 (qc/width) (Math/sin theta))]
    (qc/ellipse x 0 20 20)))
