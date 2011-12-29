(ns esa-wordseg.trie
  "A simple implementation of tries.

  Keys may be any sequence types, every node is represented by a map and it is
  therefore easy to pack the tries with different kinds of data. The get-trie
  and assoc-trie provide an associative abstraction of the trie, while the
  conj-trie, disj-trie and into-trie provide a multiset abstraction storing the
  frequencies of sequences in the trie nodes.

  None of the operations performs any removal of nodes as this is unnecessary
  for the task at hand. Also, a better implementation of tries would implement
  the standard Clojure collection interfaces and would then be accessed by
  the same functions as regular collections. However, this is now beyond
  my current grasp of Clojure.")

(def empty-trie nil)

;; ASSOCIATIVE SEMANTICS

(defn get-trie
  "Retrieves the value under key bound to the sequence x in the given trie.
  If such a value is not present, returns not-found."
  ([trie x key]
     (get-trie trie x key nil))
  ([trie x key not-found]
     (if trie
       (if (seq x)
         (get-trie (get (:succs trie) (first x)) (rest x) key not-found)
         (get trie key not-found))
       not-found)))

(defn assoc-trie
  "Associates key to val in the node of the sequence x. Multiple triplets
  of sequence, key, value can be specified."
  ([trie x key val]
     (if (seq x)
       (assoc-in trie [:succs (first x)]
                 (assoc-trie (get (:succs trie) (first x)) (rest x) key val))
       (assoc trie key val)))
  ([trie x key val & xkvs]
     (reduce (partial apply assoc-trie) (assoc-trie trie x key val) (partition 3 xkvs))))

(defn keys-trie
  "Returns all the character sequences which have a value defined
  for the key 'key' in the given trie."
  [trie key]
  (when trie
    (let [keys-succs (mapcat (fn [[ch succ]] (map #(conj % ch) (keys-trie succ key)))
                             (:succs trie))]
      (if (contains? trie key)
        (conj keys-succs '())
        keys-succs))))

(defn seq-trie
  "Returns a lazy seq of pairs of character vectors and the nodes which
  belong to them in the trie."
  [trie]
  (letfn [(seq-trie' [trie prefix]
            (when trie
              (lazy-seq
               (cons [prefix trie]
                     (mapcat (fn [[ch succ]] (seq-trie' succ (conj prefix ch)))
                             (:succs trie))))))]
    (seq-trie' trie [])))

;; MULTISET SEMANTICS

(defn conj-trie
  "Returns a trie with the xs 'added'. Here, the trie has multiset semantics
  and every addition of a sequence is realized by incrementing it's
  (:freq)uency in the trie."
  ([trie x]
     (let [current-freq (get-trie trie x :freq 0)]
       (assoc-trie trie x :freq (inc current-freq))))
  ([trie x & xs]
     (reduce conj-trie (conj-trie trie x) xs)))

(defn disj-trie
  "Returns a trie with the xs 'removed'. For this operation, the trie
  is considered as a multiset and removal of a sequence is implemented via
  decrementing its frequency."
  ([trie x]
     (let [current-freq (get-trie trie x :freq 0)]
       (assoc-trie trie x :freq (max 0 (dec current-freq)))))
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
