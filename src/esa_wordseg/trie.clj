(ns esa-wordseg.trie)

;; A simple implementation of tries.

;; Keys may be any sequence types, every node is represented by a map and it is
;; therefore easy to pack the tries with different kinds of data. The get-trie
;; and assoc-trie provide an associative abstraction of the trie, while the
;; conj-trie, disj-trie and into-trie provide a multiset abstraction storing the
;; frequencies of sequences in the trie nodes.

;; None of the operations performs any removal of nodes as this is unnecessary
;; for the task at hand.

(def empty-trie nil)

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
     (apply assoc-trie (assoc-trie x key val) xkvs)))

(defn conj-trie
  "Returns a trie with the xs 'added'. Here, the trie has multiset semantics
  and every addition of a sequence is realized by incrementing it's
  (:freq)uency in the trie."
  ([trie x]
     (let [current-freq (get-trie trie x :freq 0)]
       (assoc-trie trie x :freq (inc current-freq))))
  ([trie x & xs]
     (apply conj-trie (conj-trie trie x) xs)))

(defn disj-trie
  "Returns a trie with the xs 'removed'. For this operation, the trie
  is considered as a multiset and removal of a sequence is implemented via
  decrementing its frequency."
  ([trie x]
     (let [current-freq (get-trie trie x :freq 0)]
       (assoc-trie trie x :freq (max 0 (dec current-freq)))))
  ([trie x & xs]
     (apply disj-trie (disj-trie trie x) xs)))

(defn into-trie
  "Returns a new trie consisting of trie with the sequences in xs conjoined."
  [trie xs]
  (reduce conj-trie trie xs))
