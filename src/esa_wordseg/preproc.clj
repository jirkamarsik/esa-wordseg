(ns esa-wordseg.preproc
  "The preprocessing part of the algorithm which takes care of chunking
  the input and calculating all of its relevant statistics."
  (:import (java.lang Character$UnicodeScript))
  (:use (esa-wordseg trie [eval :only [gap-goodness]])))


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
  (let [sum (reduce + freqs)]
    (if (zero? sum)
      0
      (let [probs (map #(/ % sum) freqs)
            contribs (map #(if (zero? %) 0 (* % (Math/log %))) probs)]
        (- (reduce + contribs))))))

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

;; TAKING THE AVERAGES OF THE STATISTICS

(defn averages-by-length
  "Returns a vector whose i-th element is the average of the values bound
  to 'key' on the i-th level of the supplied tries."
  [tries key]
  (when (seq tries)
    (let [values-here (keep #(get % key) tries)
          average-here (when (not (zero? (count values-here)))
                         (/ (reduce + values-here) (count values-here)))
          successors (mapcat (comp vals :succs) tries)]
      (into [average-here] (averages-by-length successors key)))))

(defn get-average-statistics
  "Given a trie with recorded frequencies and entropies, returns a map
  from the names of these properties to the vectors of their averages
  by length."
  [trie]
  (let [averages (map (fn [key] {key (averages-by-length [trie] key)})
                      [:freq :left-entropy :right-entropy])]
    (reduce merge averages)))


;; BREAKING UP THE LARGER CHUNKS

; Possible refinement over the original: Make the minimum length of
; the two parts to be split be a secondary criterion. This way, we can
; make sure we make less "blind" splits if the scoring function will be flat.
(defn split-by-goodness
  "Furthers subdivides the given sequence of character sequences xs into
  chunks with length <= limit. The division points are selected according
  to gap goodness, which is computed using the supplied statistics
  exponent values."
  [stats xs limit exp]
  (loop [acc [], [x & more] xs]
    (if x
      (if (<= (count x) limit)
        (recur (conj acc x) more)
        (let [candidates (for [point (range 1 (- (count x) 1))
                               :let [left-x (subvec x 0 point)
                                     right-x (subvec x point)
                                     score (gap-goodness stats left-x right-x exp)]]
                           [score [left-x right-x]])
              [top-score [left-x right-x]] (last (sort candidates))]
          (recur acc (concat [left-x right-x] more))))
      acc)))
