(ns esa-wordseg.trie
  "A simple implementation of tries.

  Keys may be any sequence types, every node is represented by a map
  holding the map of successors and the current value. The get-trie,
  assoc-trie, keys-trie, seq-trie and contains?-trie provide an
  associative abstraction of the trie, while the conj-trie, disj-trie
  and into-trie provide a multiset abstraction storing the frequencies
  of sequences in the trie.

  None of the operations performs any removal of nodes as this is unnecessary
  for the task at hand. Also, a better implementation of tries would implement
  the standard Clojure collection interfaces and would then be accessed by
  the same functions as regular collections. However, this is now beyond
  my current grasp of Clojure.")

(def empty-trie nil)

;; ASSOCIATIVE SEMANTICS

(defn get-trie
  "Retrieves the value in the trie which belongs to the key sequence.
  If such a value is not present, returns not-found."
  ([trie key]
     (get-trie trie key nil))
  ([trie key not-found]
     (if trie
       (if (seq key)
         (get-trie (get-in trie [:succs (first key)]) (rest key) not-found)
         (get trie :val not-found))
       not-found)))

(defn assoc-trie
  "Associates the sequence key to the value val in the trie. Multiple pairs
  of sequence keys and values can be specified."
  ([trie key val]
     (if (seq key)
       (update-in trie [:succs (first key)] assoc-trie (rest key) val)
       (assoc trie :val val)))
  ([trie key val & kvs]
     (reduce (partial apply assoc-trie) (assoc-trie trie key val) (partition 2 kvs))))

(defn keys-trie
  "Returns all the key sequences which have a value defined in the trie as lists."
  [trie]
  (when trie
    (let [keys-succs (mapcat (fn [[ch succ]] (map #(conj % ch) (keys-trie succ)))
                             (:succs trie))]
      (if (contains? trie :val)
        (conj keys-succs '())
        keys-succs))))

(defn seq-trie
  "Returns a lazy seq of pairs of keys as vectors and the values bound to them
  in the trie."
  [trie]
  (letfn [(seq-trie' [trie prefix]
            (when trie
              (lazy-seq
               (let [seq-succs (mapcat (fn [[ch succ]]
                                         (seq-trie' succ (conj prefix ch)))
                                       (:succs trie))]
                 (if (contains? trie :val)
                   (cons [prefix (:val trie)]
                         seq-succs)
                   seq-succs)))))]
    (seq-trie' trie [])))

; A lazy trick.
(defn contains?-trie
  "Returns a boolean indicating whether the trie carries a value for the
  given key sequence."
  [trie key]
  (let [not-found (gensym)]
    (not= not-found (get-trie trie key not-found))))

;; MULTISET SEMANTICS

(defn conj-trie
  "Returns a trie with the xs 'added'. Here, the trie has multiset semantics
  and every addition of a sequence is realized by incrementing it's
  (:freq)uency in the trie."
  ([trie x]
     (let [current-freq (get-trie trie x 0)]
       (assoc-trie trie x (inc current-freq))))
  ([trie x & xs]
     (reduce conj-trie (conj-trie trie x) xs)))

(defn disj-trie
  "Returns a trie with the xs 'removed'. For this operation, the trie
  is considered as a multiset and removal of a sequence is implemented via
  decrementing its frequency."
  ([trie x]
     (let [current-freq (get-trie trie x 0)]
       (assoc-trie trie x (max 0 (dec current-freq)))))
  ([trie x & xs]
     (reduce disj-trie (disj-trie trie x) xs)))

(defn into-trie
  "Returns a new trie consisting of trie with the sequences in xs conjoined."
  [trie xs]
  (reduce conj-trie trie xs))


;; This could have been a neat macro to use, but I ran into a problem when trying
;; to have a macro expanded inside a macro form (defn), which would have to result
;; in me either writing my own defn or severely limiting the conciseness
;; of the macro. Since neither of these were appealing, I am just going to store
;; this macro here for future reference.
(defmacro variadic-overload
  "Generates a variadic overload for the function named fn-name,
  with the arglist = params. The generated overload takes the first
  parameter and calls the named function on it repeatedly with the other
  arguments. The arguments are supplied to the function in groups
  of size derived from the arity of the other unused arguments in
  params."
  [fn-name params]
  (let [[ord-params amp [rest-param]] (partition-by #{'&} params)]
    `(~params
      (reduce (partial apply ~fn-name)
              (~fn-name ~@ord-params)
              (partition ~(dec (count ord-params)) ~rest-param)))))
