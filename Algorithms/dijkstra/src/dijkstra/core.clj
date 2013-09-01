(ns dijkstra.core
  (:require [clojure.data.priority-map :as pmap]))

;;; confer http://hueypetersen.com/posts/2013/07/09/dijkstra-as-a-sequence/?utm_source=dlvr.it&utm_medium=twitter

(defprotocol IGraph
  (vertices  [ g         ])
  (neighbors [ g v       ])
  (add       [ g v1 v2 c ])
  (cost      [ g v1 v2   ]))

(defrecord UndirectedGraph [vs]
  IGraph
  (vertices  [_  ]      (keys vs))
  (neighbors [_ v]      (keys (vs v {})))
  (cost      [_ v1 v2]  (get-in vs [v1 v2]))
  (add       [_ v1 v2 c]
    (-> vs
        (assoc-in [v1 v2] c)
        (assoc-in [v2 v1] c)
        (UndirectedGraph.))))

(defrecord DirectedGraph [vs]
  IGraph
  (vertices  [_  ]      (keys vs))
  (neighbors [_ v]      (keys (vs v {})))
  (cost      [_ v1 v2]  (get-in vs [v1 v2]))
  (add       [_ v1 v2 c]
    (-> vs
        (assoc-in [v1 v2] c)
        (DirectedGraph.))))

;;; Frontier has the form of a map of pairs. Each key in the map is
;;; the name of a neighboring vertex. Each value in the map is a pair
;;; (a 2-vector) of a cost and the predecessor vertex name. For
;;; instance, a frontier like this
;;;
;;; { :v [1 :s] :w [4 :s] }
;;;
;;; means we have two successor vertices named :v and :w that connect
;;; to :s by costs 1 and 4, respectively. To get the successor vertex
;;; with the minimum cost, apply min-key of (comp first second) on a
;;; frontier structure. Min-key produces a vertex with its cost pair,
;;; so it's a one-hop segment of a path, i.e., something like
;;;
;;; { :v [1 :s] }

(defn shortest-paths-linear [g start]
  ((fn explore [explored frontier]
     (lazy-seq
      (if (empty? frontier)
        nil
        (let [;; "frontier" has the form
              ;; '{' ( <vertex> '[' <cost> <predecessor> ']' ) ... '}'
              [v [total-cost predecessor]] (apply min-key (comp first second) frontier)
              ;; path will be a vector of vertices; [] is the default
              ;; of the lookup of predecessor in the explored map
              path         (conj (explored predecessor []) v)
              explored     (assoc explored v path)
              unexplored   (remove explored (neighbors g v))
              new-frontier (into {} (for [n unexplored]
                                      [n [(+ total-cost (cost g v n)) v]]))
              frontier     (merge-with (partial min-key first)
                                       (dissoc frontier v)
                                       new-frontier)]
          (cons [v total-cost path]
                (explore explored frontier))))))
   {}                                   ; first val of explored
   { start [0] }))                      ; first val of frontier

(defn shortest-paths-log-linear [g start]
  ((fn explore [explored frontier]
     (lazy-seq
      (when-let [[v [total-cost predecessor]] (peek frontier)]
        (let [path         (conj (explored predecessor []) v)
              explored     (assoc explored v path)
              unexplored   (remove explored (neighbors g v))
              new-frontier (into {} (for [n unexplored]
                                      [n [(+ total-cost (cost g v n)) v]]))
              frontier     (merge-with (partial min-key first)
                                       (pop frontier)
                                       new-frontier)]
          (cons [v total-cost path]
                (explore explored frontier))))))
   {}                                   ; first val of explored
   (pmap/priority-map start [0]) ))

(defn shortest-path [g start dest]
  (let [not-destination? (fn [[vertex _]] (not= vertex dest))]
    (-> (shortest-paths-log-linear g start)
        (->> (drop-while not-destination?))
        first
        (nth 2))))

;;; http://hueypetersen.com/posts/2013/06/25/graph-traversal-with-clojure/

(defn- seq-graph-traverse [g s strategy]
  ((fn rec-bfs [explored frontier]
     (lazy-seq
       (if (empty? frontier)
         nil
         (let [v (peek frontier)
               neighbors (g v)]
           (cons v (rec-bfs
                     (into explored neighbors)
                     (into (pop frontier) (remove explored neighbors))))))))
   #{s} strategy))

(defn seq-graph-bfs [g s]
  (seq-graph-traverse g s (conj (clojure.lang.PersistentQueue/EMPTY) s)))

(defn seq-graph-dfs [g s]
  (seq-graph-traverse g s (conj [] s)))
