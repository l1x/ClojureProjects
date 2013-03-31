(ns milkshakes.core
  (:require clojure.set)
  (:use [clojure.java.io :as cjio])
  (:use [clojure.string  :as cstr :only [split split-lines]])
  (:use [clojure.pprint  :as pp   :only [pprint]])
  (:gen-class))

(defn get-current-directory []
  (. (java.io.File. ".") getCanonicalPath))

(defn do-per-line [f]
  (with-open [rdr (cjio/reader "input.txt")]
    (doseq [line (line-seq rdr)]
      (f line))))

(defmacro dbg [x]
  `(let [x# ~x]
     (do (println '~x "~~>" x#)
         x#))) 

;;; and pretty-printing version

(defmacro ppdbg [x]
  `(let [x# ~x]
     (do (println "----------------")
         (clojure.pprint/pprint '~x)
         (println "~~>")
         (clojure.pprint/pprint x#)
         (println "----------------")
         x#)))

(defn case-lines [pre-ls]
  (let [ls pre-ls
        c (read-string (first ls))
        prefs (reduce
               []
               (fn [prefs line] (conj prefs (map read-string (cstr/split line #"\s"))))
               (take c (drop 1 ls)))
        ]
    ))

(defn nest-case-lines [pre-ls]
  (case-lines pre-ls)
  )

(defn parse-case [ls]
  ls
  )

(defn parse-lines [ls]
  (let [ncases (read-string (first ls))
        acases (rest ls)]
    (dbg ncases)
    (map parse-case acases)))

(defn -main
  "Basic husk for programming problems."
  [& args]
  ;; work around dangerous default behaviour in Clojure
  (alter-var-root #'*read-eval* (constantly false))
  (println (get-current-directory))

  (let [input (slurp
               ;"/Users/rebcabin/Downloads/C-large-practice.in"
               "input.txt"
               )
        _     (spit "input.txt" input)
        lines (cstr/split-lines input)
        answs (map-indexed
               (fn [i l]
                 (str "Case #" (inc i) ": "
                      (identity l)
                      "\n")
                 )
               (parse-lines lines)
               )
        ]
    (with-open [w (cjio/writer "output.txt")]
      (doseq [line answs] (.write w line)))
    ))

