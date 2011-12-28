(ns esa-wordseg.preproc
  (:import (java.lang Character$UnicodeScript))
  (:use (esa-wordseg (trie :only [empty-trie into-trie]))))

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

(defn subvecs
  "Returns all subvectors of vector x with 1 <= length <= limit."
  [x limit]
  (let [n (count x)]
    (for [i (range n)
          d (range 1 (+ 1 limit))
          :let [j (+ i d)]
          :when (<= j n)]
      (subvec x i j))))

(defn frequency-trie
  "Takes a sequence of character vectors xs and returns a trie which holds
  the frequencies of all character subsequences having length <= limit."
  [xs limit]
  (let [all-subvecs (mapcat #(subvecs % limit) xs)]
    (into-trie empty-trie all-subvecs)))
