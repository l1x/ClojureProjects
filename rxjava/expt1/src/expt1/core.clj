;;; Run this file by going to the project directory (the directory with
;;; 'project.clj' in it) and saying 'lein repl'.

;;; If you're using emacs with nrepl (see
;;; http://clojure-doc.org/articles/tutorials/emacs.html for setup info),
;;; run this entire file by first "jacking in" (Ctrl-c, Meta-j), then
;;; evaluating the whole file (Ctrl-c, Ctrl-k). Eval individual expressions
;;; by placing the cursor after the last closing-parenthesis and typing
;;; Ctrl-c, Ctrl-e (for "evaluate"). Access documentation for any Clojure
;;; primitive by putting the cursor (which emacs calls "point") inside or
;;; behind the primitive and typing Ctrl-c, Ctrl-d. Find the help for the
;;; rest of the nrepl mode by typing Ctrl-h, m.

;;; With emqcs, the most important thing to learn is "Paredit." It takes most
;;; of the pain out of parentheses and nesting. There is a lot of info about
;;; it on the web (see http://emacsrocks.com/e14.html particularly), and the
;;; help is good. The two biggies are paredit-forward-slurp-sexp, whose help
;;; you can find by typing Ctrl-h, k, Ctrl-Shift-) and paredit-splice-sexp
;;; (Ctrl-h, k, Meta-s). Take the time to learn them. Slurp has three friends:
;;; paredit-forward-barf-sexp (Ctrl-h, k, Ctrl-Shift-} ) and the backwards
;;; versions of slurp and barf. They're next most important.

;;; You can re-indent any Clojure code that gets deranged by putting point
;;; at the beginning of the code and typing Ctrl-Alt-Q. You can move around
;;; at the expression level by typing Ctrl-Alt-F (forward) and Ctrl-Alt-B
;;; (backward); Ctrl-Alt-D (down a level) and Ctrl-Alt-U (up a level).

;;; Here are the namespaces we're going to use; compare this list with the
;;; :dependencies in the project.clj file, which specifies the libraries and
;;; packages to download that contain these namespaces:

(ns expt1.core
  (:require [expt1.k2               :as k2     ]
            [clojure.zip            :as zip    ]
            [clojure.xml            :as xml    ]
            [net.cgrand.enlive-html :as html   ]
            [clj-http.client        :as http   ]
            [clojure.data.json      :as cdjson ]
            clojure.string
            clojure.pprint
            [clojure.reflect        :as r      ]
            )
  (:use     [clojail.core           :only [sandbox]]
            [clojail.testers        :only [blacklist-symbols
                                           blacklist-objects
                                           secure-tester
                                           ]])
  (:refer-clojure :exclude [distinct])
  (:import [rx Observable subscriptions.Subscriptions])
  )

;;; The following is a debugging macro that acts like the identity function.
;;; It returns whatever you pass to it, and pretty-prints the input and
;;; output by side-effect in the repl or on the console:

(defmacro pdump [x]
  `(let [x# ~x]
     (do (println "----------------")
         (clojure.pprint/pprint '~x)
         (println "~~>")
         (clojure.pprint/pprint x#)
         (println "----------------")
         x#)))

;;; TODO -- move most of this to the unit-test file.

;;;   ___                  _       ___  _
;;;  / __|___ _ _  ___ _ _(_)__   / _ \| |__ ___ ___ _ ___ _____ _ _
;;; | (_ / -_) ' \/ -_) '_| / _| | (_) | '_ (_-</ -_) '_\ V / -_) '_|
;;;  \__/\___|_||_\___|_| |_\__|  \___/|_.__/__/\___|_|  \_/\___|_|
;;;

;;; The current rx library has no co-monadic operators such as "first" and
;;; "last". Let us make atomic, external collectors for extracting items
;;; from an obl (observable) by mutating side-effects.

(defn- or-default [val default] (if val (first val) default))

(defn subscribe-collectors [obl & optional-wait-time]
  (let [wait-time (or-default optional-wait-time 1000)
        ;; Keep a sequence of all values sent:
        onNextCollector      (agent    [])
        ;; Only need one value if the observable errors out:
        onErrorCollector     (atom    nil)
        ;; Use a promise for 'completed' so we can wait for it on
        ;; another thread:
        onCompletedCollector (promise    )]
    (letfn [ ;; When observable sends a value, relay it to our agent:
            (collect-next      [item] (send onNextCollector
                                            (fn [state] (conj state item))))
            ;; If observable errors out, just set our exception;
            (collect-error     [excp] (reset!  onErrorCollector     excp))
            ;; When observable completes, deliver on the promise:
            (collect-completed [    ] (deliver onCompletedCollector true))
            ;; In all cases, report out the back end with this:
            (report-collectors [    ]
              (identity ;; pdump ;; for verbose output, use pdump.
               { ;; Wait at most "wait-time" for the promise to complete; if
                ;; it does not complete, then produce 'false'. We must wait
                ;; on the onCompleted BEFORE waiting on the onNext because
                ;; the onNext-agent's await-for only waits for messages sent
                ;; to the agent from THIS thread, and our asynchronous
                ;; observable may be sending messages to the agent from
                ;; another thread. The onNext-agent's await-for will return
                ;; too quickly, allowing this onCompleted await to return,
                ;; losing some messages. This code depends on
                ;; order-of-evaluation assumptions in the map.
                :onCompleted (deref onCompletedCollector wait-time false)
                ;; Wait for everything that has been sent to the agent
                ;; to drain (presumably internal message queues):
                :onNext      (do (await-for wait-time onNextCollector)
                                 ;; Then produce the results:
                                 @onNextCollector)
                ;; If we ever saw an error, here it is:
                :onError     @onErrorCollector
                }))]
      ;; Recognize that the observable 'obl' may run on another thread:
      (-> obl
          (.subscribe collect-next collect-error collect-completed))
      ;; Therefore, produce results that wait, with timeouts, on both the
      ;; completion event and on the draining of the message queue to the
      ;; onNext-agent.
      (report-collectors))))

;;;  ___ _        _      _   _
;;; / __| |_  _ _(_)_ _ | |_(_)_ _  __ _
;;; \__ \ ' \| '_| | ' \| / / | ' \/ _` |
;;; |___/_||_|_| |_|_||_|_\_\_|_||_\__, |
;;;                                |___/


;;; There is a class of operators for shrinking a sequence. They include
;;; "take", "takeUntil", etc.; "skip*"; and "filter". To start, take the
;;; first two numbers out of an obl. This illustrates "take", a method that
;;; often shortens sequences.

(-> (Observable/toObservable [1 2 3])   ; an obl of length 3
    (.take 2)                           ; an obl of length 2
    subscribe-collectors                ; waits for completion
    pdump                               ; pretty-prints
    )

;;; Now, filter out the odd numbers and keep just the first two of that
;;; intermediate result. 

(-> (Observable/toObservable [1 2 3 4 5 6])
    (.filter (fn [n] (== 0 (mod n 2))))  ; passes only evens along
    (.take 2)                           ; keeps only the first two
    subscribe-collectors
    pdump
    )

;;;   ___                _
;;;  / __|_ _ _____ __ _(_)_ _  __ _
;;; | (_ | '_/ _ \ V  V / | ' \/ _` |
;;;  \___|_| \___/\_/\_/|_|_||_\__, |
;;;                            |___/


;;; Let's transform each number x into a vector of numbers, adding x to some
;;; familiar constants, then flattening the results exactly one time. This
;;; is a way to grow a shorter sequence into a longer one. Filters typically
;;; shorten sequences; maps leave sequences the same length. Most methods
;;; that lengthen sequences rely on mapMany, which is called "SelectMany" in
;;; many Rx documents (.e.g., http://bit.ly/18Bot23) and is similar to
;;; Clojure's "mapcat", up to order of parameters.

(-> (Observable/toObservable [1 2 3])
    (.take 2)
    (.mapMany                           ; convert each number to a vector
     #(Observable/toObservable (map (partial + %) [42 43 44])))
    subscribe-collectors
    pdump
    )

;;; Look at an observable sequence of strings, shortening it now, because
;;; that's familiar.

(-> (Observable/toObservable ["one" "two" "three"])
    (.take 2)
    subscribe-collectors
    pdump
    )

;;; "seq" explodes strings into lazy sequences of characters:

(seq "one")

;;; For self-documenting code, define an alias:

(def string-explode seq)

;;; Now, grow a sequence of strings into a sequence of chars:

(-> (Observable/toObservable ["one" "two" "three"])
    (.mapMany #(Observable/toObservable (string-explode %)))
    subscribe-collectors
    pdump
    )

;;;   __
;;;  / _|_ _ ___ _ __ ___ ___ ___ __ _
;;; |  _| '_/ _ \ '  \___(_-</ -_) _` |
;;; |_| |_| \___/_|_|_|  /__/\___\__, |
;;;                                 |_|


;;; Clean up the repeated, ugly #(Observable/toObservable ...) into a
;;; composition, but we can't (comp Observable/toObservable ...) since it's
;;; a Java method and does not implement Clojure IFn. Fix this by wrapping
;;; it in a function:

(defn from-seq [s] (Observable/toObservable s))

;;; Now we have a pretty function we can compose with string-explode:

(-> (from-seq ["one" "two" "three"])
    (.mapMany (comp from-seq string-explode))
    subscribe-collectors
    pdump
    )

;;; Theoretically, both Clojure's sequences and Rx's observables are
;;; sequences, in the sense of an ordered collection. We sometimes use the
;;; term "sequence" to mean just Clojure's sequences.

;;;          _
;;;  _ _ ___| |_ _  _ _ _ _ _
;;; | '_/ -_)  _| || | '_| ' \
;;; |_| \___|\__|\_,_|_| |_||_|


;;; Monadic "return" lifts a value into a collection of length 1 so that the
;;; collection can be composed with others via the standard query operators.
;;; This and "mapMany" are the two primitive operators in the library, and
;;; all the others can be built in terms of them.
;;;
;;; Return is missing from "rxjava 0.9.0". Add it as follows. This does some
;;; junk-work -- puts the item in a vector just so we can take it out again
;;; into an obl. A native implementation would be preferable.

(defn return [item] (from-seq [item]))

(-> (from-seq ["one" "two" "three"])
    (.mapMany (comp from-seq string-explode))
    (.mapMany return)
    subscribe-collectors
    pdump
    )

;;;     _ _    _   _         _
;;;  __| (_)__| |_(_)_ _  __| |_
;;; / _` | (_-<  _| | ' \/ _|  _|
;;; \__,_|_/__/\__|_|_||_\__|\__|


;;; Rx is has a couple of operators: "disinct" and "distinctUntilChanged",
;;; but RxJava 0.9.0 doesn't have them yet. Fake them as follows:

(-> (from-seq ["one" "two" "three"])
    (.mapMany (comp from-seq string-explode))

    ;; The following two implement "distinct".

    (.reduce #{} conj)
    ;; We now have a set of unique characters. To promote this back into an
    ;; obl of chars, do:

    (.mapMany from-seq)
    ;; This is ok because "distinct" MUST consume the entire obl sequence
    ;; before producing its values. The operator "distinct" won't work on a
    ;; non-finite obl sequence.

    subscribe-collectors
    pdump
    )

;;; Package and test.

(defn distinct [oseq]
  (-> oseq
      (.reduce #{} conj)
      (.mapMany from-seq)))

(-> (from-seq ["one" "two" "three"])
    (.mapMany (comp from-seq string-explode))
    distinct
    subscribe-collectors
    pdump
    )

;;; Notice that distinct is "unstable" in the sense that it reorders its
;;; input. TODO: a stable implementation: use the set to check uniqueness
;;; and build a vector to keep order.

;;;     _ _    _   _         _
;;;  __| (_)__| |_(_)_ _  __| |_
;;; / _` | (_-<  _| | ' \/ _|  _|
;;; \__,_|_/__/\__|_|_||_\__|\__|
;;;      _   _     _   _ _  ___ _                          _
;;;     | | | |_ _| |_(_) |/ __| |_  __ _ _ _  __ _ ___ __| |
;;;     | |_| | ' \  _| | | (__| ' \/ _` | ' \/ _` / -_) _` |
;;;      \___/|_||_\__|_|_|\___|_||_\__,_|_||_\__, \___\__,_|
;;;                                           |___/


;;; DistinctUntilChanged collapses runs of the same value in a sequence into
;;; single instances of each value. [a a a x x x a a a] becomes [a x a].
;;; 
;;; The following solution is correct but unacceptable because it consumes
;;; the entire source obl seq before producing values. Such is not necessary
;;; with distinct-until-changed: we only need to remember one back. Still,
;;; to make the point:

(-> (from-seq ["onnnnne" "tttwo" "thhrrrrree"])

    (.mapMany (comp from-seq string-explode))


    (.reduce [] (fn [acc x]
                  (let [l (last acc)]
                    (if (and l (= x l)) ; accounts for legit nils
                      acc
                      (conj acc x)))))

    ;; We now have a singleton obl containing representatives of runs of non-
    ;; distinct characters. Slurp it back into the monad:
    (.mapMany from-seq)

    subscribe-collectors
    pdump
    )

;;; Better is to keep a mutable buffer of length one. It could be an atom if
;;; we had the opposite of "compare-and-set!." We want an atomic primitive
;;; that sets the value only if it's NOT equal to its current value.
;;; "compare-and set!" sets the atom to a new val if its current value is
;;; EQUAL to an old val. It's easy enough to get the desired semantics with a
;;; Ref and software-transactional memory, the only wrinkle being that the
;;; container must be defined outside the function that mapMany applies.
;;; However, this solution will not materialize the entire input sequence.

(let [exploded (-> (from-seq ["onnnnne" "tttwo" "thhrrrrree"])
                   (.mapMany (comp from-seq string-explode)))
      last-container (ref [])]
  (-> exploded
      (.mapMany (fn [x]
                  (dosync
                   (let [l (last @last-container)]
                     (if (and l (= x l))
                       (Observable/empty)
                       (do
                         (ref-set last-container [x])
                         (return x)))))))
      subscribe-collectors
      pdump))

;;; Package and test:

(defn distinct-until-changed [oseq]
  (let [last-container (ref [])]
    (-> oseq
        (.mapMany (fn [x]
                    (dosync
                     (let [l (last @last-container)]
                       (if (and l (= x l))
                         (Observable/empty)
                         (do
                            (ref-set last-container [x])
                           (return x))))))))))

(->  (from-seq ["onnnnne" "tttwo" "thhrrrrree"])
     (.mapMany (comp from-seq string-explode))
     distinct-until-changed
     subscribe-collectors
     pdump
     )

;;; It's well-behaved on an empty input:

(->  (from-seq [])
     (.mapMany (comp from-seq string-explode))
     distinct-until-changed
     subscribe-collectors
     pdump
     )

;;;  ___               _          _
;;; | _ \___ _ __  ___| |_ ___ __| |
;;; |   / -_) '  \/ _ \  _/ -_) _` |
;;; |_|_\___|_|_|_\___/\__\___\__,_|
;;;    ___               _
;;;   / _ \ _  _ ___ _ _(_)___ ___
;;;  | (_) | || / -_) '_| / -_|_-<
;;;   \__\_\\_,_\___|_| |_\___/__/

;;; Be sure to set a .java.policy file in the appropriate directory
;;; (HOME if you are running this as an ordinary user). Here is a very
;;; liberal policy file:
;;;
;;; grant {
;;;   permission java.security.AllPermission;
;;; };
;;;
;;; Note that user-supplied symbols must be fully qualified for the
;;; Clojail sandbox.

(defn run-jailed-queries
  [source queries]
  (let [sb (sandbox secure-tester)
        es (read-string source)
        qs (map read-string queries)
        ]
    (sb `(-> ~es ~@qs subscribe-collectors pdump ))))

(let [source "(expt1.core/from-seq [\"onnnnne\" \"tttwo\" \"thhrrrrree\"])"
      queries ["(.mapMany (comp expt1.core/from-seq expt1.core/string-explode))"
               "expt1.core/distinct-until-changed"
              ]
      ]
  (run-jailed-queries source queries))


;;;  ___              _                             
;;; / __|_  _ _ _  __| |_  _ _ ___ _ _  ___ _  _ ___
;;; \__ \ || | ' \/ _| ' \| '_/ _ \ ' \/ _ \ || (_-<
;;; |___/\_, |_||_\__|_||_|_| \___/_||_\___/\_,_/__/
;;;      |__/                                       
;;;   ___  _                         _    _     
;;;  / _ \| |__ ___ ___ _ ___ ____ _| |__| |___ 
;;; | (_) | '_ (_-</ -_) '_\ V / _` | '_ \ / -_)
;;;  \___/|_.__/__/\___|_|  \_/\__,_|_.__/_\___|
;;;                                             

;;; An observable has a "subscribe" method, which is a function of an
;;; observer. When called, the subscribe method subscribes the observer to the
;;; observable sequence of values produced by the observable.

(defn synchronous-observable [the-seq]
  "A custom Observable whose 'subscribe' method does not return until
   the observable completes, that is, a 'blocking' observable.

  returns Observable<String>"
  (Observable/create
   (fn [observer]
     ;; This subscribe method just calls the observer's "onNext" handler until
     ;; exhausted.
     (doseq [x the-seq] (-> observer (.onNext x)))
     ;; After sending all values, complete the sequence:
     (-> observer .onCompleted)
     ;; Return a NoOpSubsription. Since this observable does not return from
     ;; its subscription call until it sends all messages and completes, the
     ;; thread receiving the "subscription" can't unsubscribe until the
     ;; observable completes, at which time there is no value in
     ;; unsubscribing. We say that this observable "blocks."
     (Subscriptions/empty))))

;;; Flip is always needed in functional programming! It takes a function and
;;; produces a new function that calls the old function with arguments in the
;;; opposite order.
(defn flip [f2] (fn [x y] (f2 y x)))

;;; Test the synchronous observable:

(-> (synchronous-observable (range 50)) ; produces 0, 1, 2, ..., 50
    (.map    #(str "SynchronousValue_" %)) ; produces strings
    (.map    (partial (flip clojure.string/split) #"_")) ; splits at "_"
    (.map    (fn [[a b]] [a (Integer/parseInt b)])) ; converts seconds 
    (.filter (fn [[a b]] (= 0 (mod b 7)))) ; keeps only multiples of 7
    subscribe-collectors
    pdump
    )

;;; Compare rxjava's ".map" with Clojure's "map". The biggest difference is
;;; that Rx's .map takes the collection in the privileged first position. Such
;;; makes "fluent composition" with the "->" macro easy. Clojure's map takes
;;; the function in the first argument slot. This difference complicates the
;;; translation of fluent code from Clojure into fluent code for rxjava,
;;; though "flip" can help. Ditto rxjava/.filter and core/filter: their
;;; argument lists are the flips of one another.

;;; Consider the example above, and write a non-reactive version of it.

(-> (range 50)
    ((flip map)    #(str "NonReactiveValue_" %))
    ((flip map)    (partial (flip clojure.string/split) #"_"))
    ((flip map)    (fn [[a b]] [a (Integer/parseInt b)]))
    ((flip filter) (fn [[a b]] (= 0 (mod b 7))))
    pdump
    )

;;; The code above looks very similar to the reactive code-block prior to it.
;;; Specifically, the arguments are identical. The device used to bring the
;;; collection arguments into first position is "flip". To make the
;;; resemblance even more complete, we might do the following

(let [-map    (flip map)
      -filter (flip filter)]
  (-> (range 50)
      (-map    #(str "NonReactiveValue2.0_" %))
      (-map    (partial (flip clojure.string/split) #"_"))
      (-map    (fn [[a b]] [a (Integer/parseInt b)]))
      (-filter (fn [[a b]] (= 0 (mod b 7))))
      pdump
      )
  )

;;; With these local definitions, "-map" and "-filter", the non-reactive
;;; version looks just like the reactive version.

;;;    _                    _                             
;;;   /_\   ____  _ _ _  __| |_  _ _ ___ _ _  ___ _  _ ___
;;;  / _ \ (_-< || | ' \/ _| ' \| '_/ _ \ ' \/ _ \ || (_-<
;;; /_/ \_\/__/\_, |_||_\__|_||_|_| \___/_||_\___/\_,_/__/
;;;            |__/                                       
;;;   ___  _                         _    _     
;;;  / _ \| |__ ___ ___ _ ___ ____ _| |__| |___ 
;;; | (_) | '_ (_-</ -_) '_\ V / _` | '_ \ / -_)
;;;  \___/|_.__/__/\___|_|  \_/\__,_|_.__/_\___|
;;;                                             

(defn asynchronous-observable [the-seq]
  "A custom Observable whose 'subscribe' method returns immediately and whose
   other actions -- namely, onNext, onCompleted, onError -- occur on another
   thread.

  returns Observable<String>"
  (Observable/create
   (fn [observer]
     (let [f (future (doseq [x the-seq] (-> observer (.onNext x)))
                     ;; After sending all values, complete the sequence:
                     (-> observer .onCompleted))]
       ;; Return a subscription that cancels the future:
       (Subscriptions/create #(future-cancel f))))))

(-> (asynchronous-observable (range 50))
    (.map #(str "AsynchronousValue_" %))
    (.map (partial (flip clojure.string/split) #"_"))
    (.map (fn [[a b]] [a (Integer/parseInt b)]))
    (.filter (fn [[a b]] (= 0 (mod b 7))))
    subscribe-collectors
    pdump
    )

;;;    _                    _     __      __   _      ___                  
;;;   /_\   ____  _ _ _  __| |_   \ \    / /__| |__  | _ \__ _ __ _ ___ ___
;;;  / _ \ (_-< || | ' \/ _| ' \   \ \/\/ / -_) '_ \ |  _/ _` / _` / -_|_-<
;;; /_/ \_\/__/\_, |_||_\__|_||_|   \_/\_/\___|_.__/ |_| \__,_\__, \___/__/
;;;            |__/                                           |___/        

(defn asynchWikipediaArticle [names]
  "Fetch a list of Wikipedia articles asynchronously
   with proper error handling.

   return Observable<String> of HTML"
  (Observable/create
   (fn [observer]
     (let [f (future
               (try
                 (doseq [name names]
                   (-> observer
                       (.onNext
                        (html/html-resource
                         (java.net.URL.
                          (str "http://en.wikipedia.org/wiki/" name))))
                       ;; Netflix originally used strings, but...
                       ))
                 ;; (catch Exception e (prn "exception")))
                 (catch Exception e (-> observer (.onError e))))
               ;; after sending response to onNext, complete the sequence
               (-> observer .onCompleted))]
       ;; a subscription that cancels the future if unsubscribed
       (Subscriptions/create #(future-cancel f))))))

(defn zip-str [s]
  (zip/xml-zip 
   (xml/parse 
    (java.io.ByteArrayInputStream. 
     (.getBytes s)))))

(->>
 ((subscribe-collectors
   (asynchWikipediaArticle
    [(rand-nth ["Atom" "Molecule" "Quark" "Boson" "Fermion"])
     "NonExistentTitle"
     (rand-nth ["Lion" "Tiger" "Bear" "Shark"])])
   5000)
  :onNext)
 (map #(html/select % [:title]))
 pdump)

;;;  _  _     _    __ _ _      __   ___    _            
;;; | \| |___| |_ / _| (_)_ __ \ \ / (_)__| |___ ___ ___
;;; | .` / -_)  _|  _| | \ \ /  \ V /| / _` / -_) _ (_-<
;;; |_|\_\___|\__|_| |_|_/_\_\   \_/ |_\__,_\___\___/__/
                                                    
(defn simulatedSlowMapObjectObservable [nullaryFnToMapObject & optionalDelayMSec]
  (let [delay (or-default optionalDelayMSec 50)]
    (Observable/create
     (fn [observer]
       (let [f (future
                 (try
                   ;; simulate fetching user data via network service call with latency
                   (Thread/sleep delay)
                   (-> observer (.onNext (nullaryFnToMapObject)))
                   (-> observer .onCompleted)
                   (catch Exception e (-> observer (.onError e))))) ]
         ;; a subscription that cancels the future if unsubscribed
         (Subscriptions/create #(future-cancel f)))))))

(defn getUser [userId]
  "Asynchronously fetch user data. Returns Observable<Map>"
  (simulatedSlowMapObjectObservable
   (fn []
     {:user-id userId
      :name "Sam Harris"
      :preferred-language (if (= 0 (rand-int 2)) "en-us" "es-us") })
   60))

(defn getVideoBookmark [userId, videoId]
  "Asynchronously fetch bookmark for video. Returns Observable<Integer>"
  (simulatedSlowMapObjectObservable
   (fn []
     {:video-id videoId
      ;; 50/50 chance of giving back position 0 or 0-2500
      :position (if (= 0 (rand-int 2)) 0 (rand-int 2500))})
   20))

(defn getVideoMetadata [videoId, preferredLanguage]
  "Asynchronously fetch movie metadata for a given language. Return Observable<Map>"
  (simulatedSlowMapObjectObservable
   (fn []
     {:video-id videoId
      :title (case preferredLanguage
               "en-us" "House of Cards: Episode 1"
               "es-us" "Cámara de Tarjetas: Episodio 1"
               "no-title")
      :director "David Fincher"
      :duration 3365})
   50))

(defn getVideoForUser [userId videoId]
  "Get video metadata for a given userId
  - video metadata
  - video bookmark position
  - user data
  Returns Observable<Map>"
  (let [user-observable
        (-> (getUser userId)
            (.map (fn [user] {:user-name (:name user)
                             :language (:preferred-language user)})))
        bookmark-observable
        (-> (getVideoBookmark userId videoId)
            (.map (fn [bookmark] {:viewed-position (:position bookmark)})))

        ;; getVideoMetadata requires :language from user-observable; nest
        ;; inside map function
        video-metadata-observable
        (-> user-observable
            (.mapMany
             ;; fetch metadata after a response from user-observable is
             ;; received
             (fn [user-map]
               (getVideoMetadata videoId (:language user-map)))))]
    ;; now combine 3 async sequences using zip
    (-> (Observable/zip
         bookmark-observable video-metadata-observable user-observable
         (fn [bookmark-map metadata-map user-map]
           {:bookmark-map bookmark-map
            :metadata-map metadata-map
            :user-map user-map}))
        ;; and transform into a single response object
        (.map (fn [data]
                {:video-id       videoId
                 :user-id        userId
                 :video-metadata (:metadata-map    data)
                 :language       (:language        (:user-map data))
                 :bookmark       (:viewed-position (:bookmark-map data))})))))

(-> (getVideoForUser 12345 78965)
    subscribe-collectors
    pdump
    )

;;;     _       __            _  _               _      
;;;  _ | |__ _ / _|__ _ _ _  | || |_  _ ___ __ _(_)_ _  
;;; | || / _` |  _/ _` | '_| | __ | || (_-</ _` | | ' \ 
;;;  \__/\__,_|_| \__,_|_|   |_||_|\_,_/__/\__,_|_|_||_|
;;;  ___                _            
;;; | __|_ _____ _ _ __(_)___ ___ ___
;;; | _|\ \ / -_) '_/ _| (_-</ -_|_-<
;;; |___/_\_\___|_| \__|_/__/\___/__/

;;;    ____                 _           ____
;;;   / __/_ _____ ________(_)__ ___   / __/
;;;  / _/ \ \ / -_) __/ __/ (_-</ -_) /__ \ 
;;; /___//_\_\\__/_/  \__/_/___/\__/ /____/ 
                                        
;;; Exercise 5: Use map() to project an array of videos into an array of
;;; {id,title} pairs For each video, project a {id,title} pair.

;;; (in Clojure, iterpret "pair" to mean "a map with two elements")

(defn jslurp [filename]
  (-> (str "./src/expt1/" filename)
      slurp
      cdjson/read-str
      pdump
      ))

(-> (jslurp "Exercise_5.json")
    ;; Make all levels asynchronous (maximize fuggliness):
    asynchronous-observable

    ;; The following line is the one that should be compared / contrasted with
    ;; JavaScript & Datapath -- the surrounding lines are just input & output.
    ;; I do likewise with all the other exercises: surrounding the "meat" in
    ;; the sandwich with blank lines.

    (.map (fn [vid] {:id (vid "id") :title (vid "title")}))

    subscribe-collectors
    pdump)

;;; in JavsScript, interpret "pair" to mean "an object with two
;;; properties"

;;; return newReleases
;;;   .map(
;;;     function (r) {
;;;       return {
;;;         id: r.id,
;;;         title: r.title
;;;       };
;;;     });

;;; Datapath

;;; (exist (r)
;;;   (and
;;;     (.* newReleases r)
;;;     (= result {
;;;          id: (. r "id"),
;;;          title: (. r "title"),
;;;        }
;;;     )
;;;   )
;;; )

;;;    ____                 _           ___ 
;;;   / __/_ _____ ________(_)__ ___   ( _ )
;;;  / _/ \ \ / -_) __/ __/ (_-</ -_) / _  |
;;; /___//_\_\\__/_/  \__/_/___/\__/  \___/ 
                                        
;;; Exercise 8: Chain filter and map to collect the ids of videos that have a
;;; rating of 5.0

;;; Select all videos with a rating of 5.0 and project the id field.

(-> (jslurp "Exercise_8.json")
    asynchronous-observable

    (.filter (fn [vid] (== (vid "rating") 5.0)))
    (.map (fn [vid]  (vid "id")))

    subscribe-collectors
    pdump)

;;;  Javascript
;;; 
;;; return newReleases
;;;   .filter(
;;;     function(r) {
;;;       return r.rating === 5.0;
;;;     })
;;;   .map(
;;;     function(r){
;;;       return r.id;
;;;     });


;;;  Datapath
;;; 
;;; (exist (r)
;;;   (and
;;;     (.* newReleases r)
;;;     (. r "rating" 5.0)
;;;     (. r "id" id)
;;;   )
;;; )

;;;    ____                 _           ______
;;;   / __/_ _____ ________(_)__ ___   <  <  /
;;;  / _/ \ \ / -_) __/ __/ (_-</ -_)  / // / 
;;; /___//_\_\\__/_/  \__/_/___/\__/  /_//_/  
                                          
;;; Exercise 11: Use map() and mergeAll() to project and flatten the
;;; movieLists into an array of video ids

;;; Produce a flattened list of video ids from all movie lists.

;;; Remark: No "mergeAll" in rxjava / Clojure; look up "merge" here:
;;; http://netflix.github.io/RxJava/javadoc/rx/Observable.html

(-> (jslurp "Exercise_11.json")
    asynchronous-observable 

    (.map (fn [genre] (asynchronous-observable (genre "videos"))))

    (Observable/merge)
    (.map (fn [vid] (vid "id")))

    subscribe-collectors
    pdump)   

;;; Javascript
;;; 
;;; return movieLists
;;;   .map(
;;;     function(x) {
;;;       return x.videos;
;;;     })
;;;   .mergeAll()
;;;   .map(
;;;     function(x) {
;;;       return x.id;
;;;     });

;;; Datapath
;;; 
;;; (. (.* (. (.* movieLists) "videos")) "id" id)

;;;    ____                 _           _______
;;;   / __/_ _____ ________(_)__ ___   <  / / /
;;;  / _/ \ \ / -_) __/ __/ (_-</ -_)  / /_  _/
;;; /___//_\_\\__/_/  \__/_/___/\__/  /_/ /_/  

;;; Exercise 14: Use mapMany() to retrieve id, title, and 150x200 box art url
;;; for every video.
;;;
;;; I changed the original slightly so that "Chamber" has no 150x200 box art
;;; (to test the case where some input does not pass the filter) and so that
;;; "Fracture" has two 150x200 boxarts (to test that they're not improperly
;;; nested)

(-> (jslurp "Exercise_14.json")
    asynchronous-observable 

    (.mapMany (fn [genres] (-> (genres "videos") asynchronous-observable)))

    (.mapMany (fn [vid]    (-> (vid "boxarts")   asynchronous-observable
                              (.filter (fn [art] (and (== 150 (art "width"))
                                                     (== 200 (art "height")))))
                              (.map (fn [art] ;; note the closure over "vid"
                                      {:id    (vid "id")
                                       :title (vid "title")
                                       :url   (art "url")})))))

    subscribe-collectors
    pdump)   

;;; 
;;; Javascript
;;; 
;;; return movieLists
;;;   .mapMany(function(m) { return m.videos })
;;;   .mapMany(
;;;     function(v) {
;;;       return v
;;;         .boxarts
;;;         .filter(
;;;           function(x) {
;;;             return x.width === 150
;;;               && x.height === 200;
;;;           })
;;;         .map(
;;;           function(x) {
;;;             return {
;;;               id: v.id,
;;;               title: v.title,
;;;               boxart: x.url
;;;             };
;;;           });
;;;     });
;;; Datapath
;;; 

;;; Datapath avoids closure issues by instantiating all variables in a
;;; "unification" style. Bravo!

;;; (exist (v x)
;;;   (and
;;;     (.* (. (.* movieLists) "videos") v)
;;;     (.* (. v "boxarts") x)
;;;     (. x "width" 150)
;;;     (. x "height" 200)
;;;     (= result {
;;;          id: (. v "id"),
;;;          title: (. v "title"),
;;;          boxart: (. x "url")
;;;        }
;;;     )
;;;   )
;;; )

;;;    ____                 _           ___ ____
;;;   / __/_ _____ ________(_)__ ___   |_  / / /
;;;  / _/ \ \ / -_) __/ __/ (_-</ -_) / __/_  _/
;;; /___//_\_\\__/_/  \__/_/___/\__/ /____//_/  
                                            
;;; Exercise 24: Retrieve each video's id, title, middle interesting moment
;;; time, and smallest box art url.

(-> (jslurp "Exercise_24.json")
    asynchronous-observable

    (.mapMany (fn [genre] (-> (genre "videos") asynchronous-observable)))
    (.mapMany (fn [vid]
                (let [arts (-> (vid "boxarts")
                               asynchronous-observable
                               (.reduce (fn [c p]
                                          (if (< (* (c "height") (c "width"))
                                                 (* (p "height") (p "width")))
                                            c p))))
                      moments (-> (vid "interestingMoments")
                                  asynchronous-observable
                                  (.filter (fn [moment] (= (moment "type") "Middle"))))
                      ]
                  (Observable/zip
                   arts
                   moments
                   (fn [art moment]
                     {:id    (vid    "id")
                      :title (vid    "title")
                      :time  (moment "time")
                      :url   (art    "url")}
                     )))
                ))

    subscribe-collectors
    pdump)

;;; Javascript
;;; 
;;; return movieLists
;;;   .mapMany(
;;;     function(movieList) {
;;;       return movieList.videos;
;;;     })
;;;   .mapMany(
;;;     function(video) {
;;;       return Array.zip(
;;;         video
;;;           .boxarts
;;;           .reduce(
;;;             function(p, c) {
;;;               return
;;;                 c.width * c.height <
;;;                 p.width * p.height ? c : p;
;;;             }),
;;;         video
;;;           .interestingMoments
;;;           .filter(
;;;             function(m) {
;;;               return m.type === "Middle";
;;;             }),
;;;         function(b,m) {
;;;           return {
;;;             id: video.id,
;;;             title: video.title,
;;;             time: m.time,
;;;             url: b.url
;;;           };
;;;         });
;;;     });

;;; Datapath
;;; 
;;; (exist (video boxart moment)
;;;   (and
;;;     (.* (. (.* movieLists) "videos") video)
;;;     (min
;;;       (size boxart)
;;;       (and
;;;         (.* (. video "boxarts") boxart)
;;;         (*
;;;           (. boxart "width")
;;;           (. boxart "height")
;;;           size))
;;;       boxart)
;;;     (.* (. video "interestingMoments") moment)
;;;     (. moment "type" "Middle")
;;;     (= result
;;;        {
;;;          id: (. video "id"),
;;;          title: (. video "title"),
;;;          url: (. boxart "url"),
;;;          time: (. moment "time")
;;;        })
;;;   )
;;; )

;;;    ____                 _           ___  ____
;;;   / __/_ _____ ________(_)__ ___   |_  |/ __/
;;;  / _/ \ \ / -_) __/ __/ (_-</ -_) / __//__ \ 
;;; /___//_\_\\__/_/  \__/_/___/\__/ /____/____/ 
                                             
;;; Exercise 25: Converting from Arrays to Trees

;;; We have 2 arrays each containing lists, and videos respectively. Each
;;; video has a listId field indicating its parent list. We want to build an
;;; array of list objects, each with a name and a videos array. The videos
;;; array will contain the video's id and title.

;;; Input

;;; lists:
;;;         [
;;;             {
;;;                 "id": 5434364,
;;;                 "name": "New Releases"
;;;             },
;;;             {
;;;                 "id": 65456475,
;;;                 name: "Thrillers"
;;;             }
;;;         ]
;;;  
;;; videos:
;;;         [
;;;             {
;;;                 "listId": 5434364,
;;;                 "id": 65432445,
;;;                 "title": "The Chamber"
;;;             },
;;;             {
;;;                 "listId": 5434364,            
;;;                 "id": 675465,
;;;                 "title": "Fracture"
;;;             },
;;;             {
;;;                 "listId": 65456475,            
;;;                 "id": 70111470,
;;;                 "title": "Die Hard"
;;;             },
;;;             {
;;;                 "listId": 65456475,                        
;;;                 "id": 654356453,
;;;                 "title": "Bad Boys"
;;;             }
;;;         ]
;;; Output
;;; 
;;; [
;;;     {
;;;         "name": "New Releases",
;;;         "videos": [
;;;             {
;;;                 "id": 65432445,
;;;                 "title": "The Chamber"
;;;             },
;;;             {
;;;                 "id": 675465,
;;;                 "title": "Fracture"
;;;             }
;;;         ]
;;;     },
;;;     {
;;;         "name": "Thrillers",
;;;         "videos": [
;;;             {
;;;                 "id": 70111470,
;;;                 "title": "Die Hard"
;;;             },
;;;             {
;;;                 "id": 654356453,
;;;                 "title": "Bad Boys"
;;;             }
;;;         ]
;;;     }
;;; ]

;;; Javascript
;;; 
;;; return lists.map(
;;;   function (list) {
;;;     return {
;;;       name: list.name,
;;;       videos: videos
;;;         .filter(
;;;           function (video) {
;;;             return video.listId === list.id;
;;;           })
;;;         .map(
;;;           function (video) {
;;;             return {
;;;               id: video.id,
;;;               title: video.title
;;;             };
;;;           })
;;;     };
;;;   });

;;; Datapath
;;; 
;;; (exist (list)
;;;   (and
;;;     (.* lists list)
;;;     (= result {
;;;         name: (. list "name"),
;;;         videos: (list (v)
;;;           (exist (video)
;;;             (and
;;;               (.* videos video)
;;;               (. video "listId" (. list "id"))
;;;               (= v {
;;;                   id: (. video "id"),
;;;                   title: (. video "title")
;;;                 })
;;;             )
;;;           ))
;;;       })
;;;   )
;;; )

(let [lists  (-> (jslurp "Exercise_25_lists.json")  asynchronous-observable)
      videos (-> (jslurp "Exercise_25_videos.json") asynchronous-observable)]

  (-> lists
      (.map (fn [lyst]
              {:name (lyst "name")
               :videos
               (-> videos
                   (.filter (fn [vid] (== (vid "listId") (lyst "id"))))
                   (.map (fn [vid] {:id (vid "id") :title (vid "title")}))
                   )
               }
              ))

      subscribe-collectors
      pdump
      :onNext
      ((flip map)
       (fn [lyst]
         {:name (lyst :name)
          :videos (-> (lyst :videos)
                      subscribe-collectors
                      :onNext)}
         )))
  )

;;;           __ _        _   _          
;;;  _ _ ___ / _| |___ __| |_(_)___ _ _  
;;; | '_/ -_)  _| / -_) _|  _| / _ \ ' \ 
;;; |_| \___|_| |_\___\__|\__|_\___/_||_|
                                     
;;; The following is an example of how to use reflection to print the
;;; current members of the Observable class.

(pdump (into #{}
             (map (comp #(% 1) first)
                  (sort-by
                   :name
                   (filter
                    :exception-types
                    (:members (r/reflect Observable)))))))
