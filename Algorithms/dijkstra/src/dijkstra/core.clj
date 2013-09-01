(ns dijkstra.core)

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

(defn shortest-paths [g start]
  ((fn explore [explored frontier]
     (lazy-seq
      (if (empty? frontier)
        nil
        (let [[v [total-cost predecessor]] (apply min-key (comp first second) frontier)
              path                         (conj (explored predecessor []) v)
              explored                     (assoc explored v path)
              unexplored-neighbors         (remove explored (neighbors g v))
              new-frontier                 (into {} (for [n unexplored-neighbors]
                                                      [n [(+ total-cost (cost g v n)) v]]))
              frontier                     (merge-with (partial min-key first)
                                                       (dissoc frontier v)
                                                       new-frontier)]
          (cons [v total-cost path]
                (explore explored frontier))))))
   {}                                   ; explored
   { start [0] }))                      ; frontier

