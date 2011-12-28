(ns esa-wordseg.preproc
  "The preprocessing part of the algorithm which takes care of chunking
  the input and calculating all of its relevant statistics."
  (:import (java.lang Character$UnicodeScript))
  (:use (esa-wordseg (trie :only [empty-trie into-trie assoc-trie keys-trie]))))


;;; PRELIMINARY CHUNKING OF INPUT

(defn further-partition-by
  "Applies f to each value in the collections in colls, splitting them
  each time f returns a new value. Returns a lazy seq of all the resulting
  partitions."
  [f colls]
  (mapcat #(partition-by f %) colls))

(defn split-on-newline
  "Takes a sequence of character sequences and further subdivides them
  on any occurrence of a newline. The sequences of newlines are preserved
  in the stream of sequences."
  [xs]
  (further-partition-by (comp boolean #{\newline \return}) xs))

(defn split-on-punct
  "Takes a sequence of character sequences and further subdivides them on
  boundaries between punctuation and non-punctuation. "
  [xs]
  (let [punct? #(->> % str (re-matches #"\p{P}") boolean)]
    (further-partition-by punct? xs)))

(defn split-on-char-class
  "Takes a sequence of character sequences and further subdivides them
  on the boundaries between Chinese and non-Chinese characters."
  [xs]
  (let [chinese? #(->> % int Character$UnicodeScript/of
                       (= Character$UnicodeScript/HAN))]
    (further-partition-by chinese? xs)))


;;; CALCULATING THE STATISTICS OF THE INPUT

(defn subvecs
  "Returns all subvectors of vector x with 1 <= length <= limit."
  [x limit]
  (let [n (count x)]
    (for [i (range n)
          d (range 1 (+ 1 limit))
          :let [j (+ i d)]
          :when (<= j n)]
      (subvec x i j))))

(defn get-frequency-trie
  "Takes a sequence of character vectors xs and returns a trie which holds
  the frequencies of all character subsequences having length <= limit."
  [xs limit]
  (let [all-subvecs (mapcat #(subvecs % limit) xs)]
    (into-trie empty-trie all-subvecs)))

;; COMPUTING THE ENTROPIES OF PRECEDING/FOLLOWING CHAR

(defn plus-one-left-freqs
  "Returns a seq of frequencies (as given by the trie) of all character
  sequences of the form c . x, where . is concatenation
  and c is any character."
  [trie x]
  (map #(get-trie % x :freq 0) (vals (:succs trie))))

(defn plus-one-right-freqs
  "Returns a seq of frequencies (as given by the trie) of all character
  sequences of the form x . c, where . is concatenation
  and c is any character."
  [trie x]
  (map :freq (vals (get-trie trie x :succs))))

(defn entropy
  "Calculates the entropy of a distribution given a seq
  of its frequencies/probabilities."
  [freqs]
  (let [sum (apply + freqs)]
    (if (== sum 0)
      0
      (let [probs (map #(/ % sum) freqs)
            contribs (map #(if (== % 0) 0 (* % (Math/log %))) probs)]
        (- (apply + contribs))))))

(defn add-entropies-to-trie
  "Takes a trie of character sequence frequencies and computes
  and stores the entropies of Sequence Plus One Left
  and Sequence Plus One Right."
  [trie]
  (let [trie-keys (keys-trie trie :freq)
        left-entropy-entries (mapcat #(list % :left-entropy
                                            (entropy (plus-one-left-freqs trie %)))
                                     trie-keys)
        right-entropy-entries (mapcat #(list % :right-entropy
                                             (entropy (plus-one-right-freqs trie %)))
                                      trie-keys)]
    (apply assoc-trie trie (concat left-entropy-entries right-entropy-entries))))
