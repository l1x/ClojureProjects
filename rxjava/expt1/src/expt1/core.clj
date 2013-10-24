;;; Run this file by going to the project directory (the directory with
;;; 'project.clj' in it) and saying 'lein repl'.

;;; If you're using emacs with nrepl (see
;;; http://clojure-doc.org/articles/tutorials/emacs.html for setup
;;; info), run this entire file by first "jacking in" (Ctrl-c, Meta-j),
;;; then evaluating the whole file (Ctrl-c, Ctrl-k).  Eval individual
;;; expressions by placing the cursor after the last closing-parenthesis
;;; and typing Ctrl-c, Ctrl-e (for "evaluate").  Access documentation
;;; for any Clojure primitive by putting the cursor (which emacs calls
;;; "point") inside or behind the primitive and typing Ctrl-c,
;;; Ctrl-d. Find the help for the rest of the nrepl mode by typing
;;; Ctrl-h, m.

;;; With emqcs, the most important thing to learn is "Paredit."  It
;;; takes most of the pain out of parentheses and nesting.  There is a
;;; lot of info about it on the web (see http://emacsrocks.com/e14.html
;;; particularly), and the help is good.  The two biggies are
;;; paredit-forward-slurp-sexp, whose help you can find by typing
;;; Ctrl-h, k, Ctrl-Shift-) and paredit-splice-sexp (Ctrl-h, k, Meta-s).
;;; Take the time to learn them.  Slurp has three friends:
;;; paredit-forward-barf-sexp (Ctrl-h, k, Ctrl-Shift-} ) and the
;;; backwards versions of slurp and barf.  They're next most important.

;;; Re-indent deranged code by putting point at the beginning of the
;;; code and typing Ctrl-Alt-Q.  Move around at the expression level by
;;; Ctrl-Alt-F (forward) and Ctrl-Alt-B (backward); Ctrl-Alt-D
;;; (down a level) and Ctrl-Alt-U (up a level).

;;; Here are the namespaces we use; compare this list with the
;;; :dependencies in the project.clj file, which specifies the libraries
;;; and packages to download that contain these namespaces:

(ns expt1.core
  (:require [clojure.zip             :as zip    ]
            [clojure.xml             :as xml    ]
            [net.cgrand.enlive-html  :as html   ]
            [clj-http.client         :as http   ]
            [clojure.data.json       :as cdjson ]
            clojure.string
            clojure.pprint
            [clojure.reflect         :as r      ]
            [rx.lang.clojure.interop :as rx     ]
            )
  (:use     [clojail.core            :only [sandbox]]
            [clojail.testers         :only [blacklist-symbols
                                            blacklist-objects
                                            secure-tester
                                            ]])
  (:refer-clojure :exclude [distinct])
  (:import [rx
            Observable
            Observer
            subscriptions.Subscriptions
            subjects.Subject
            subjects.PublishSubject])
  )

;;; Set this to 'false' or 'nil' during development so we don't hit
;;; wikipedia too much.

(def hit-wikipedia true)

;;; The following is a debugging macro that acts like the identity
;;; function.  It returns whatever you pass to it, and pretty-prints the
;;; input and output by side-effect in the repl or on the console:

(defmacro pdump [x]
  `(let [x# (try ~x (catch Exception e# (str e#)))]
     (do (println "----------------")
         (clojure.pprint/pprint '~x)
         (println "~~>")
         (clojure.pprint/pprint x#)
         x#)))

;;; We need a 'catch-less' variant to run inside the jail:

(defmacro catchless-pdump [x]
  `(let [x#  ~x]
     (do (println "----------------")
         (clojure.pprint/pprint '~x)
         (println "~~>")
         (clojure.pprint/pprint x#)
         x#)))

;;; TODO -- move most of this to the unit-test file.

;;;   ___                  _       ___  _
;;;  / __|___ _ _  ___ _ _(_)__   / _ \| |__ ___ ___ _ ___ _____ _ _
;;; | (_ / -_) ' \/ -_) '_| / _| | (_) | '_ (_-</ -_) '_\ V / -_) '_|
;;;  \__/\___|_||_\___|_| |_\__|  \___/|_.__/__/\___|_|  \_/\___|_|
;;;

;;; Fetch optional values from function arguments preceded by & (and
;;; therefore packaged in sequences, hence the "first").

(defn- or-default [val default] (if val (first val) default))

;;; The current rx library has no co-monadic operators such as "first"
;;; and "last".  Let us make atomic, external collectors for extracting
;;; items from an obl (observable) by mutating side-effects.

(defn subscribe-collectors [obl & optional-wait-time]

  ;; Recognize that the observable 'obl' may run on another thread:
  ;; Therefore, produce results that wait, with timeouts, on both the
  ;; completion event and on the draining of the message queue to the
  ;; onNext-agent.

  (let [wait-time (or-default optional-wait-time 1000)

        ;; Keep a sequence of all values sent:

        onNextCollector      (agent [])

        ;; Only need one value if the observable errors out:

        onErrorCollector     (atom  nil)

        ;; Use a promise for 'completed' so we can wait for it on
        ;; another thread:

        onCompletedCollector (promise)]

    (let [ ;; When observable sends a value, relay it to our agent:

          collect-next
          (rx/action [item] (send onNextCollector
                                  (fn [state] (conj state item))))

          ;; If observable errors out, just set our exception;

          collect-error
          (rx/action [excp] (reset! onErrorCollector excp))

          ;; When observable completes, deliver on the promise:

          collect-completed
          (rx/action [    ] (deliver onCompletedCollector true))

          ;; In all cases, report out the back end with this:

          report-collectors
          (fn [    ]
            (identity ;; pdump ;; for verbose output, use pdump.
             { ;; Wait at most "wait-time" for the promise to complete;
               ;; if it does not complete, produce 'false'.  We must
               ;; wait on onCompleted BEFORE waiting on onNext because
               ;; the onNext-agent's await-for only waits for messages
               ;; sent to the agent from THIS thread, and our
               ;; asynchronous observable may be sending messages to the
               ;; agent from another thread.  The onNext-agent's
               ;; await-for will return too quickly.  When the
               ;; onCompleted deref-await returns, some of the agent's
               ;; messages will be lost.  This code depends on
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
      {:subscription
       (.subscribe obl collect-next collect-error collect-completed)

       :reporter
       report-collectors})))

(defn report [subscribed-collectors]
  (pdump ((:reporter subscribed-collectors))))

;;;  ___ _        _      _   _
;;; / __| |_  _ _(_)_ _ | |_(_)_ _  __ _
;;; \__ \ ' \| '_| | ' \| / / | ' \/ _` |
;;; |___/_||_|_| |_|_||_|_\_\_|_||_\__, |
;;;                                |___/

;;; When working with collections of objects distributed in space, we
;;; have familiar higher-order operators for reducing the number of
;;; elements in the collection. For instance, we can take some number
;;; from the front of a sequence.

(->> [1 2 3]
    (take 2))

(take 2 [1 2 3])

;;; The BIG IDEA of reactive programming is that COLLECTIONS OF DATA
;;; DISTRIBUTED IN TIME ACT JUST LIKE COLLECTIONS DISTRIBUTED IN SPACE.
;;; We should expect to have the same operators.

;;; Another view is COORDINATE-FREE PROGRAMMING. In functional
;;; programming, we replace index-loops with higher-order operators.
;;; The higher-order operators take arguments that are functions (lambda
;;; expressions or closures).  The functions operate individually on the
;;; values in the collection.  We replace expressions like
;;;
;;;     for (i = 0; i < ARRAY_LEN; i++)
;;;         println(array[i]);
;;;
;;; with expressions like
;;;
;;;     array.map(element => println(element))
;;;
;;; We replace the oordinate-FULL expression array[i] with the
;;; coordinate-free expression "element," which is a parameter that is
;;; iteratively *bound* to the values in the collection (the array).
;;; The lambda expression is a "callback" invoked by the higher-order
;;; operator "map."  "Map" is called "higher-order" because it is a
;;; function that takes functions as arguments.
;;;
;;; Once the coordinates are gone, there is nothing in the expression to
;;; say where the value came from.  Thus, the expression can, in
;;; principle, be used over any collection independently of the
;;; "coordinate system" used to index the values.  We can easily shift
;;; from a space coordinate system to a time coordinate system without
;;; changing the "business logic" underneath, often without changing the
;;; code at all (Dave Ray from netflix has a short video on this).  This
;;; abstraction away from coordinates or indices is the principal
;;; abstraction of functional -- and therefore reactive -- programming.
;;;
;;; This is the "monadic" abstraction.  Our values are "in a monad"
;;; rather than in an indexed collection of this or that kind.  This
;;; abstraction affords many very powerful generalizations, only one of
;;; which is reactive programming.
;;;

(-> (Observable/from [1 2 3])   ; an obl of length 3
    (.take 2)                   ; an obl of length 2
    subscribe-collectors        ; waits for completion
    report                      ; pretty-prints
    )

;;; The "take" in the sequence monad also works for infinite, lazy
;;; sequences.  These are an intermediate step between sequences fully
;;; realized in memory and sequences that produce values over time.
;;; Lazy sequences produce values over time, driven by internal forces.
;;; Observables produce values over time, driven by anything, including
;;; external forces.

(->> (repeat 42)
     (take 2))

;;; Operating on infinite sequences works in the observable monad, but
;;; it's harder to demonstrate in our toy examples.  Don't call
;;; "Observable/from" on an infinite lazy sequence; it realizes the
;;; whole thing.

(-> (Observable/from (take 100 (repeat 42)))
    (.take 2)
    subscribe-collectors
    report)

;;; Now, filter out the odd numbers and keep just the first two of that
;;; intermediate result.  Here, we write the prediction
;;; function-argument of the higher-order "filter" operator as an
;;; anonymous "rxjava" function.  Since java doesn't (yet) have
;;; first-class anonymous functions, we must declare it with an rx/fn
;;; rather than with the standard, Clojure "fn."  This is a gargoyle
;;; that I hope will go away soon (because it limits our claim that
;;; EXACTLY the same code can run iteratively as reactively).

(-> (Observable/from [1 2 3 4 5 6])
    (.filter (rx/fn [n] (== 0 (mod n 2)))) ; passes only evens along
    (.take 2)                              ; keeps only the first two
    subscribe-collectors
    report)

;;; Here is another way of doing the same thing, only with a named
;;; predicate-function argument.  "rx/fn*" converts a first-class
;;; Clojure function into a rxjava function.

(-> (Observable/from [1 2 3 4 5 6])
    (.filter (rx/fn* even?))
    (.take 2)
    subscribe-collectors
    report)

;;; Here is the space version:

(->> [1 2 3 4 5 6 7]
     (filter even?)
     (take 2))

;;;   ___                _
;;;  / __|_ _ _____ __ _(_)_ _  __ _
;;; | (_ | '_/ _ \ V  V / | ' \/ _` |
;;;  \___|_| \___/\_/\_/|_|_||_\__, |
;;;                            |___/

;;; Filtering removes values.  Mapping transforms values.  How do we
;;; make a collection bigger?

;;; Let's transform each number x into a vector of numbers, adding x to
;;; some familiar constants, then flattening the results exactly one
;;; time.  Most methods that lengthen sequences rely on mapMany, called
;;; "SelectMany" in many Rx documents (.e.g., http://bit.ly/18Bot23) and
;;; is similar to Clojure's "mapcat", up to order of parameters.

(->> [1 2 3]
     (take 2)
     ;; For each x in the collection, map a function over the fixed
     ;; vector [42 43 44].  The function will add its argument x to its
     ;; input; that is, the function is a closure over x. mapcat takes
     ;; care of flattening the results just once.
     (mapcat (fn [x] (map (partial + x) [42 43 44]))))

(-> (Observable/from [1 2 3])
    (.take 2)
    ;; With a collection distributed over time, the function must
    ;; produce a nested observable, which rxjava's mapMany will flatten
    ;; exactly once:
    (.mapMany            ; convert each number to an obl of more numbers
     (rx/fn [x] (Observable/from (map (partial + x) [42 43 44]))))
    subscribe-collectors
    report)

;;; Look at an observable sequence of strings, back to shortening it
;;; now, because that's familiar.

(-> (Observable/from ["one" "two" "three"])
    (.take 2)
    subscribe-collectors
    report
    )

;;; "seq" explodes strings into lazy sequences of characters:

(seq "one")

;;; Define an alias, since we have a specific purpose in mind:

(def string-explode seq)

;;; Now, grow a sequence of strings into a sequence of chars:

(-> (Observable/from ["one" "two" "three"])
    (.mapMany (rx/fn* #(Observable/from (string-explode %))))
    subscribe-collectors
    report
    )

;;;   __
;;;  / _|_ _ ___ _ __ ___ ___ ___ __ _
;;; |  _| '_/ _ \ '  \___(_-</ -_) _` |
;;; |_| |_| \___/_|_|_|  /__/\___\__, |
;;;                                 |_|


;;; Clean up the repeated, ugly (Observable/from ...) calls into a
;;; composition, but we can't (comp Observable/from ...) since it's a
;;; Java method and does not implement Clojure IFn.  Fix this by
;;; wrapping it in a function:

(defn from-seq [s] (Observable/from s))

;;; Now we have a pretty function we can compose with string-explode:

(-> (from-seq ["one" "two" "three"])
    (.mapMany (rx/fn* (comp from-seq string-explode)))
    subscribe-collectors
    report
    )

;;;          _
;;;  _ _ ___| |_ _  _ _ _ _ _
;;; | '_/ -_)  _| || | '_| ' \
;;; |_| \___|\__|\_,_|_| |_||_|


;;; Monadic "return" lifts a value into a collection of length 1 so that the
;;; collection can be composed with others via the standard query operators.
;;; This and "mapMany" are the two primitive operators in the library, and
;;; almost all the others can be built in terms of them.
;;;
;;; Let's add "return" as follows.  This does some junk-work -- puts the
;;; item into the vector monad just so we can take it out again into an
;;; obl.  A native implementation would be preferable.

(defn return [item] (from-seq [item]))

(-> (from-seq ["one" "two" "three"])
    (.mapMany (rx/fn* (comp from-seq string-explode)))

    ;; Here, return acts like the identity.  In fact, it *is* the
    ;; identity function in any monad.  It illustrates the point that
    ;; all higher-order function arguments to mapMany have the same,
    ;; special signature as does "return.".  Functions like this have
    ;; "monadic signature": they take a value and return
    ;; values-in-a-box, where the box refers to the prevailing monad,
    ;; known from context.

    (.mapMany (rx/fn* return))
    subscribe-collectors
    report
    )

;;;     _ _    _   _         _
;;;  __| (_)__| |_(_)_ _  __| |_
;;; / _` | (_-<  _| | ' \/ _|  _|
;;; \__,_|_/__/\__|_|_||_\__|\__|


;;; Rx is has a couple of operators: "disinct" and "distinctUntilChanged."
;;; Fake them as follows, to show how to build new operators in Clojure.

(-> (from-seq ["one" "two" "three"])
    (.mapMany (rx/fn* (comp from-seq string-explode)))

    ;; The following two implement "distinct".

    (.reduce #{} (rx/fn* conj))

    ;; We now have a hash-set of unique characters.  Because Clojure
    ;; hash-sets are automatically seq'able, that is, convertible into
    ;; the sequence monad, promote the hash-set back into an obl of
    ;; chars as follows:

    (.mapMany (rx/fn* from-seq))

    ;; This is ok because "distinct" MUST consume the entire obl sequence
    ;; before producing its values. The operator "distinct" won't work on a
    ;; non-finite obl sequence.

    subscribe-collectors
    report
    )

;;; Package and test.

(defn distinct [obl]
  (-> obl
      (.reduce #{} (rx/fn* conj))
      (.mapMany (rx/fn* from-seq))))

(-> (from-seq ["one" "two" "three"])
    (.mapMany (rx/fn* (comp from-seq string-explode)))
    distinct
    subscribe-collectors
    report
    )

;;; Distinct is "unstable" in the sense that it reorders its input.
;;; EXERCISE: a stable implementation.  HINT: use the set to check
;;; uniqueness and build a vector to keep order.

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
;;; The following solution is correct but unacceptable because it
;;; consumes the entire source obl seq before producing values.  Such is
;;; not necessary with distinct-until-changed: we only need to remember
;;; one back.  Still, as a step to a better solution:

(-> (from-seq ["onnnnne" "tttwo" "thhrrrrree"])

    (.mapMany (rx/fn* (comp from-seq string-explode)))

    (.reduce [] (rx/fn [acc x]
                  (let [l (last acc)]
                    (if (and l (= x l)) ; accounts for legit nils
                      acc               ; nil is "falsey"
                      (conj acc x)))))

    ;; We now have a fully realized, non-lazy, singleton obl containing
    ;; representatives of runs of non-distinct characters.  Slurp it
    ;; back into the monad:

    (.mapMany (rx/fn* from-seq))

    subscribe-collectors
    report)

;;; Better is to keep a mutable buffer of length one. It could be an
;;; atom if we had the opposite of "compare-and-set!."  We want an
;;; atomic primitive that sets the value only if it's NOT equal to its
;;; current value.  "compare-and set!" sets the atom to a new val if its
;;; current value is EQUAL to an old val.  It's easy enough to get the
;;; desired semantics with a Ref and software-transactional memory, the
;;; only wrinkle being that the container must be defined outside the
;;; function that mapMany applies.  However, this solution will not
;;; materialize the entire input sequence.

(let [exploded (-> (from-seq ["onnnnne" "tttwo" "thhrrrrree"])
                   (.mapMany (rx/fn* (comp from-seq string-explode))))
      last-container (ref [])]
  (-> exploded
      (.mapMany (rx/fn [x]
                  (dosync
                   (let [l (last @last-container)]
                     (if (and l (= x l))
                       (Observable/empty) ; shiny-new! like [] or ()
                       (do
                         (ref-set last-container [x])
                         (return x)))))))
      subscribe-collectors
      report))

;;; Package and test:

(defn distinct-until-changed [obl]
  (let [last-container (ref [])]
    (-> obl
        (.mapMany (rx/fn [x]
                    (dosync
                     (let [l (last @last-container)]
                       (if (and l (= x l))
                         (Observable/empty)
                         (do
                            (ref-set last-container [x])
                           (return x))))))))))

(->  (from-seq ["onnnnne" "tttwo" "thhrrrrree"])
     (.mapMany (rx/fn* (comp from-seq string-explode)))
     distinct-until-changed
     subscribe-collectors
     report
     )

;;; It's well-behaved on an empty input:

(->  (from-seq [])
     (.mapMany (rx/fn* (comp from-seq string-explode)))
     distinct-until-changed
     subscribe-collectors
     report
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
;;; (HOME if you are running this as an ordinary user; in a configured
;;; directory on a server).  Here is a very liberal policy file:
;;;
;;; grant {
;;;   permission java.security.AllPermission;
;;; };
;;;
;;; Cannot inject a 'catch' into the sandbox; hence the catchless-dump.

(defn run-jailed-queries
  [source queries]
  (let [sb (sandbox secure-tester)
        es (read-string source)
        qs (map read-string queries)
        ]
    (sb `(-> ~es ~@qs subscribe-collectors catchless-pdump ))))

;;; Symbols must be fully qualified (no implicit namespaces) in the
;;; sandbox.

(let [source "(expt1.core/from-seq [\"onnnnne\" \"tttwo\" \"thhrrrrree\"])"
      queries ["(.mapMany (rx.lang.clojure.interop/fn* (comp expt1.core/from-seq expt1.core/string-explode)))"
               "expt1.core/distinct-until-changed"
              ]
      ]
  ((:reporter (run-jailed-queries source queries))))


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
;;; observer.  When called, the subscribe method subscribes the observer
;;; to the observations (a.k.a. "messages," "events," "notifications"):
;;; the values produced by the observable.

(defn synchronous-observable [the-seq]
  "A custom Observable whose 'subscribe' method does not return until
   the observable completes, that is, a 'blocking' observable."
  (Observable/create
   (rx/fn [observer]

     ;; Just call the observer's "onNext" handler until exhausted.

     (doseq [x the-seq] (-> observer (.onNext x)))

     ;; After sending all values, complete the sequence:

     (-> observer .onCompleted)

     ;; Return a no-op subscription.  Since this observable does not
     ;; return from its subscription call until it sends all messages
     ;; and completes, the thread receiving the "subscription" can't
     ;; unsubscribe until the observable completes, at which time there
     ;; is no point in unsubscribing.  We say that this observable
     ;; "blocks."  The returned subscription is pro-forma only.

     (Subscriptions/empty))))

;;; Test and Demo:

;;; Flip is always needed in functional programming!  It takes a
;;; function and produces a new function that calls the old function
;;; with arguments in the opposite order.

(defn flip [g] (fn [x y] (g y x)))

;;; Test the synchronous observable:

(-> (synchronous-observable (range 50)) ; produces 0, 1, 2, ..., 50
    (.map    (rx/fn* #(str "SynchronousValue_" %)))
    (.map    (rx/fn* (partial (flip clojure.string/split) #"_")))
    (.map    (rx/fn [[a b]] [a (Integer/parseInt b)]))
    (.filter (rx/fn [[a b]] (= 0 (mod b 7))))
    subscribe-collectors
    report
    )

;;; Compare rxjava's ".map" with Clojure's "map".  The biggest
;;; difference is that Rx's .map takes the collection in the privileged
;;; first position -- the privilege borrowed from object-oriented
;;; programming, where "this" implicitly appears there.
;;;
;;; This small difference "fluent composition" with the "->" macro easy.
;;; Clojure's map takes the function in the first argument slot.  This
;;; difference complicates the translation of fluent code from Clojure
;;; into fluent code for rxjava, though "flip" can help.  Ditto
;;; rxjava/.filter and core/filter: their argument lists are the flips
;;; of one another.

;;; Consider the example above, and write a non-reactive version of it.

(-> (range 50)
    ((flip map)    #(str "NonReactiveValue_" %))
    ((flip map)    (partial (flip clojure.string/split) #"_"))
    ((flip map)    (fn [[a b]] [a (Integer/parseInt b)]))
    ((flip filter) (fn [[a b]] (= 0 (mod b 7))))
    pdump
    )

;;; The code above looks very similar to the reactive code-block prior
;;; to it.  Specifically, the arguments are identical (up to the
;;; "fn"-"rx/fn" dichotomy mentioned above).  The device used to bring
;;; the collection arguments into first position is "flip".  To make the
;;; resemblance even more complete, we might do the following (this is
;;; the opposite of Dave Ray's demonstration -- he makes Rx look like
;;; Clojure; we make Clojure look like Rx.)

(let [-map    (flip map)
      -filter (flip filter)]
  (-> (range 50)
      (-map    #(str "NonReactiveValue2.0_" %))
      (-map    (partial (flip clojure.string/split) #"_"))
      (-map    (fn [[a b]] [a (Integer/parseInt b)]))
      (-filter (fn [[a b]] (= 0 (mod b 7))))
      pdump
      ))

;;; With these local definitions, "-map" and "-filter", the non-reactive
;;; version looks almost just like the reactive version.

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
  "A custom Observable whose 'subscribe' method returns immediately and
   whose other actions -- namely, onNext, onCompleted, onError -- occur
   on another thread."
  (Observable/create
   (rx/fn [observer]                    ; this is "subscribe"
     (let [f (future (doseq [x the-seq] (-> observer (.onNext x)))

                     ;; After sending all values, complete the sequence:

                     (-> observer .onCompleted))]

       ;; Return a subscription that cancels the future:

       (Subscriptions/create (rx/action [] (future-cancel f)))))))

(-> (asynchronous-observable (range 50))
    (.map    (rx/fn* #(str "AsynchronousValue_" %)))
    (.map    (rx/fn* (partial (flip clojure.string/split) #"_")))
    (.map    (rx/fn [[a b]] [a (Integer/parseInt b)]))
    (.filter (rx/fn [[a b]] (= 0 (mod b 7))))
    subscribe-collectors
    report
    )

;;;    _                    _     __      __   _      ___
;;;   /_\   ____  _ _ _  __| |_   \ \    / /__| |__  | _ \__ _ __ _ ___ ___
;;;  / _ \ (_-< || | ' \/ _| ' \   \ \/\/ / -_) '_ \ |  _/ _` / _` / -_|_-<
;;; /_/ \_\/__/\_, |_||_\__|_||_|   \_/\_/\___|_.__/ |_| \__,_\__, \___/__/
;;;            |__/                                           |___/


(defn asynchWikipediaArticle [names]
  "Fetch a sequence of Wikipedia articles asynchronously with proper
   error handling."
  (Observable/create
   (rx/fn [observer]
     (let [f (future
               (try
                 (doseq [name names]
                   (-> observer
                       (.onNext
                        (html/html-resource
                         (java.net.URL.
                          (str "https://en.wikipedia.org/wiki/" name))))

                       ;; Netflix originally used strings, but...

                       ))

                 (catch Exception e (-> observer (.onError e))))

               ;; after sending response to onNext, complete the sequence

               (-> observer .onCompleted))]

       ;; a subscription that cancels the future if unsubscribed

       (Subscriptions/create (rx/action [] (future-cancel f)))))))

;;; The following is a left-over reminder that we can use the "zipper"
;;; to traverse and modify the html.  Powerful mojo; worth the reminder
;;; here.

(defn zip-str [s]
  (zip/xml-zip
   (xml/parse
    (java.io.ByteArrayInputStream.
     (.getBytes s)))))

;;; Have a develop-time switch so we don't pound the site too much.

(when hit-wikipedia
  (->>
   (((:reporter (subscribe-collectors
                 (asynchWikipediaArticle
                  [(rand-nth ["Atom" "Molecule" "Quark" "Boson" "Fermion"])
                   "NonExistentTitle"
                   (rand-nth ["Lion" "Tiger" "Bear" "Shark"])])
                 5000)))
    :onNext)
   (map #(html/select % [:title]))
   pdump))

;;;  _  _     _    __ _ _      __   ___    _
;;; | \| |___| |_ / _| (_)_ __ \ \ / (_)__| |___ ___ ___
;;; | .` / -_)  _|  _| | \ \ /  \ V /| / _` / -_) _ (_-<
;;; |_|\_\___|\__|_| |_|_/_\_\   \_/ |_\__,_\___\___/__/


(defn simulatedSlowMapObjectObservable
  [nullaryFnToMapObject & optionalDelayMSec]

  (let [delay (or-default optionalDelayMSec 50)]
    (Observable/create
     (rx/fn [observer]
       (let [f (future
                 (try

                   ;; simulate fetching user data via network service
                   ;; call with latency

                   (Thread/sleep delay)
                   (-> observer (.onNext (nullaryFnToMapObject)))
                   (-> observer .onCompleted)
                   (catch Exception e (-> observer (.onError e))))) ]

         ;; a subscription that cancels the future if unsubscribed

         (Subscriptions/create f))))))

(defn getUser [userId]
  "Asynchronously fetch user data. Returns observable of a hash-map."
  (simulatedSlowMapObjectObservable
   (fn []
     {:user-id userId
      :name "Sam Harris"
      :preferred-language (if (= 0 (rand-int 2)) "en-us" "es-us") })
   60))

(defn getVideoBookmark [userId, videoId]
  "Asynchronously fetch bookmark for video. Returns observable of an
integer."
  (simulatedSlowMapObjectObservable
   (fn []
     {:video-id videoId
      ;; 50% chance of giving back position 0 or 0-2500
      :position (if (= 0 (rand-int 2)) 0 (rand-int 2500))})
   20))

(defn getVideoMetadata [videoId, preferredLanguage]
  "Asynchronously fetch movie metadata for a given language. Return
observable of a hash-map."
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
  Returns observable of a hash-map."
  (let [user-observable
        (-> (getUser userId)
            (.map (rx/fn [user] {:user-name (:name user)
                                 :language (:preferred-language user)})))
        bookmark-observable
        (-> (getVideoBookmark userId videoId)
            (.map (rx/fn [bookmark] {:viewed-position (:position bookmark)})))

        ;; getVideoMetadata requires :language from user-observable; nest
        ;; inside map function

        video-metadata-observable
        (-> user-observable
            (.mapMany
             ;; fetch metadata after a response from user-observable is
             ;; received
             (rx/fn [user-map]
               (getVideoMetadata videoId (:language user-map)))))]

    (-> (Observable/zip

         ;; "zip" takes N observables ...

         bookmark-observable
         video-metadata-observable
         user-observable

         ;; and an N-ary function ...

         (rx/fn [bookmark-map metadata-map user-map]
           {:bookmark-map bookmark-map
            :metadata-map metadata-map
            :user-map user-map}))

        ;; and produces a single response observable

        (.map (rx/fn [data]
                {:video-id       videoId
                 :user-id        userId
                 :video-metadata (:metadata-map    data)
                 :language       (:language        (:user-map data))
                 :bookmark       (:viewed-position (:bookmark-map data))})))))

(-> (getVideoForUser 12345 78965)
    subscribe-collectors
    report
    )

;;;     _       __            _  _               _
;;;  _ | |__ _ / _|__ _ _ _  | || |_  _ ___ __ _(_)_ _
;;; | || / _` |  _/ _` | '_| | __ | || (_-</ _` | | ' \
;;;  \__/\__,_|_| \__,_|_|   |_||_|\_,_/__/\__,_|_|_||_|
;;;  ___                _
;;; | __|_ _____ _ _ __(_)___ ___ ___
;;; | _|\ \ / -_) '_/ _| (_-</ -_|_-<
;;; |___/_\_\___|_| \__|_/__/\___/__/

;;; Compare-contrast Java, JavaScript, and Datapath (a dialect of
;;; mini-Kanren)

;;;    ____                 _           ____
;;;   / __/_ _____ ________(_)__ ___   / __/
;;;  / _/ \ \ / -_) __/ __/ (_-</ -_) /__ \
;;; /___//_\_\\__/_/  \__/_/___/\__/ /____/

;;; Exercise 5: Use map() to project an array of videos into an array of
;;; {id,title} pairs For each video, project a {id,title} pair.

;;; (in Clojure, iterpret "pair" to mean "a hash-map with two elements")

(defn jslurp [filename]
  (-> (str "./src/expt1/" filename)
      slurp
      cdjson/read-str
      pdump
      ))

(-> (jslurp "Exercise_5.json")

    ;; Make all levels asynchronous (maximize fuggliness):

    asynchronous-observable

    ;; The following line is the one that should be compared /
    ;; contrasted with JavaScript & Datapath -- the surrounding lines
    ;; are just input & output.  I do likewise with all the other
    ;; exercises: surrounding the "meat" in the sandwich with blank
    ;; lines.

    (.map (rx/fn [vid] {:id (vid "id") :title (vid "title")}))

    subscribe-collectors
    report)

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

;;; Exercise 8: Chain filter and map to collect the ids of videos that
;;; have a rating of 5.0

;;; Select all videos with a rating of 5.0 and project the id field.

(-> (jslurp "Exercise_8.json")
    asynchronous-observable

    (.filter (rx/fn [vid] (== (vid "rating") 5.0)))
    (.map    (rx/fn [vid]  (vid "id")))

    subscribe-collectors
    report)

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
;;; movie lists into an array of video ids.

;;; Produce a flattened list of video ids from all movie lists.

;;; Remark: No "mergeAll" in rxjava / Clojure; look up "merge" here:
;;; http://netflix.github.io/RxJava/javadoc/rx/Observable.html

(-> (jslurp "Exercise_11.json")
    asynchronous-observable

    ;; Fetch the "videos" key out of each genre.
    (.map (rx/fn [genre] (asynchronous-observable (genre "videos"))))

    (Observable/merge)

    ;; Fetch the "id" key out of each vid.
    (.map (rx/fn [vid] (vid "id")))

    subscribe-collectors
    report)

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

;;; Exercise 14: Use mapMany() to retrieve id, title, and 150x200 box
;;; art url for every video.
;;;
;;; I changed the original slightly so that "Chamber" has no 150x200 box
;;; art (to test the case where some input does not pass the filter) and
;;; so that "Fracture" has two 150x200 boxarts (to test that they're not
;;; improperly nested)

(-> (jslurp "Exercise_14.json")
    asynchronous-observable

    (.mapMany (rx/fn [genres] (-> (genres "videos")
                                  asynchronous-observable)))

    (.mapMany (rx/fn [vid]    (-> (vid "boxarts")
                                  asynchronous-observable
                                  (.filter (rx/fn [art]
                                             (and (== 150 (art "width"))
                                                  (== 200 (art "height")))))
                              (.map (rx/fn [art] ;; note closure over "vid"
                                      {:id    (vid "id")
                                       :title (vid "title")
                                       :url   (art "url")})))))

    subscribe-collectors
    report)

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
;;;     )))


;;;    ____                 _           ___ ____
;;;   / __/_ _____ ________(_)__ ___   |_  / / /
;;;  / _/ \ \ / -_) __/ __/ (_-</ -_) / __/_  _/
;;; /___//_\_\\__/_/  \__/_/___/\__/ /____//_/

;;; Exercise 24: Retrieve each video's id, title, middle interesting
;;; moment time, and smallest box art url.

(-> (jslurp "Exercise_24.json")
    asynchronous-observable

    (.mapMany (rx/fn [genre] (-> (genre "videos")
                                 asynchronous-observable)))
    (.mapMany (rx/fn [vid]
                (let [arts (-> (vid "boxarts")
                               asynchronous-observable
                               (.reduce (rx/fn [c p]
                                          (if (< (* (c "height") (c "width"))
                                                 (* (p "height") (p "width")))
                                            c p))))
                      moments (-> (vid "interestingMoments")
                                  asynchronous-observable
                                  (.filter (rx/fn [moment]
                                             (= (moment "type") "Middle"))))
                      ]
                  (Observable/zip

                   arts
                   moments

                   (rx/fn [art moment]
                     {:id    (vid    "id")
                      :title (vid    "title")
                      :time  (moment "time")
                      :url   (art    "url")}
                     )))
                ))

    subscribe-collectors
    report)

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
;;;   ))

;;;    ____                 _           ___  ____
;;;   / __/_ _____ ________(_)__ ___   |_  |/ __/
;;;  / _/ \ \ / -_) __/ __/ (_-</ -_) / __//__ \
;;; /___//_\_\\__/_/  \__/_/___/\__/ /____/____/

;;; Exercise 25: Join Arrays to Tree

;;; We have 2 arrays containing genre ids and videos respectively.  Each
;;; video has a listId field indicating its genre.  We want an array of
;;; genres, each with a name and a videos array.  The videos array will
;;; contain the video's id and title.

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
      (.map (rx/fn [lyst]
              {:name (lyst "name")
               :videos
               (-> videos
                   ;; The following contains the "join condition."
                   (.filter (rx/fn [vid] (== (vid "listId") (lyst "id"))))
                   (.map    (rx/fn [vid] {:id (vid "id")
                                          :title (vid "title")}))
                   )
               }))

      subscribe-collectors
      report
      :onNext

      ((flip map)
       (fn [lyst]
         {:name (lyst :name)
          :videos (-> (lyst :videos)
                      subscribe-collectors
                      report
                      :onNext)
          }))
      pdump))

;;;    _  __           __             _____
;;;   / |/ /_ ____ _  / /  ___ ____  / ___/__ ___ _  ___ ___
;;;  /    / // /  ' \/ _ \/ -_) __/ / (_ / _ `/  ' \/ -_|_-<
;;; /_/|_/\_,_/_/_/_/_.__/\__/_/    \___/\_,_/_/_/_/\__/___/


(defn magic [x y]
  (lazy-seq (cons y (magic y (+ x y)))))

(def fibs (magic 1N 1N))

(pdump (first (drop 1000 fibs)))

(defn divides?
  "Tests whether k divides n; the order of the arguments is in the sense of
   an infix operator: read (divides? k n) as \"k divides? n\"."
  [k n] (== 0 (rem n k)))

(def does-not-divide? (complement divides?))

(defn sieve [xs]
  (if (empty? xs)
    ()
    (cons (first xs)
          (lazy-seq (sieve
                     (filter (partial does-not-divide? (first xs))
                             (rest xs)))))))

(def primes (sieve (cons 2 (iterate (partial + 2N) 3))))

(pdump (first (drop 1000 primes)))

;;;    ____     __     _         __
;;;   / __/_ __/ /    (_)__ ____/ /_
;;;  _\ \/ // / _ \  / / -_) __/ __/
;;; /___/\_,_/_.__/_/ /\__/\__/\__/
;;;              |___/

;;; This enables mapping of the reactive scheme to REST.  Transformed
;;; observables are "Subjects:" both observers and observables.  As
;;; observers, they subscribe (in a privileged subscriber list) to their
;;; antecedents.  As observables, they are ordinary.  They may suffer
;;; additional transformations, becoming antecedents of other
;;; observables.  Or they may be simply observed by egress observers.

(pdump
 (let [obl1 (PublishSubject/create)]

   (.onNext obl1 41)

   (let [obl2 (-> obl1
                  (.map (rx/fn [x] (+ 100 x)))
                  (.filter (rx/fn* even?))
                  (.mapMany (rx/fn [obn]
                              (Observable/create (rx/fn [obr]
                                                   (.onNext obr obn)
                                                   (.onNext obr (* obn obn))
                                                   (.onCompleted obr))))))
         result (subscribe-collectors obl2)]

     (.onNext obl1 42)
     (.onNext obl1 43)
     (.onNext obl1 44)

     (.unsubscribe (:subscription result))

     (.onNext obl1 45)
     (.onNext obl1 46)

     (.onCompleted obl1)

     ((:reporter result))
     )))

;;;           __ _        _   _
;;;  _ _ ___ / _| |___ __| |_(_)___ _ _
;;; | '_/ -_)  _| / -_) _|  _| / _ \ ' \
;;; |_| \___|_| |_\___\__|\__|_\___/_||_|

;;; The following shows how to print the current members of the
;;; Observable class.

(pdump (into #{}
             (map (comp #(% 1) first)
                  (sort-by
                   :name
                   (filter
                    :exception-types
                    (:members (r/reflect Observable :ancestors true)))))))
