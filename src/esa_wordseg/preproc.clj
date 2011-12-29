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

(defn get-alphabet-size
  "Returns the number of distinct characters in the sequence of character
  sequences xs."
  [xs]
  (count (reduce into (map set xs))))

(defn entropy
  "Estimates the entropy of a distribution given frequencies of the witnessed
  outcomes and the total number of possible outcomes. Smooths the distribution
  using Good-Turing before taking the entropy."
  [freqs total-outcomes]
  {:pre [(> (reduce + freqs) 0)]}
  (when (seq freqs)
    (let[zero-probs (- total-outcomes (count freqs))
         freqs-of-freqs (apply merge-with + (cons {0 zero-probs}
                                                  (map (fn [f] {f 1}) freqs)))
         raw-est (fn [f] (/ (* (+ f 1) (freqs-of-freqs (+ f 1) 0))
                           (* total-outcomes (freqs-of-freqs f 0))))
         raw-probs (map raw-est freqs)
         raw-zero (raw-est 0)
         sum (reduce + (* zero-probs raw-zero) raw-probs)
         normalize (fn [p] (/ p sum))
         probs (map normalize raw-probs)
         prob-zero (normalize raw-zero)
         entropy-summand (fn [p] (if (zero? p) 0 (* p (Math/log p))))
         contribs (map entropy-summand probs)
         contrib-zero (entropy-summand prob-zero)]
      (- (reduce + (* zero-probs contrib-zero) contribs)))))

; It might be nicer to split this function up (the consituent fns
; are already there).
(defn add-entropy-tries
  "Takes a trie with sequence frequencies and the size of the data's
  alphabet and returns a map containing the original trie along with
  tries of entropies of the Sequence Plus One sets."
  [freq-trie alphabet-size]
  (let [find-sp1 (fn [key]
                   (fn [trie [x node]]
                     (if (< (count x) 2)
                       trie
                       (let [sub-x (case key
                                         :sp1-left-freqs (subvec x 1)
                                         :sp1-right-freqs (subvec x 0 (- (count x) 1)))
                             freq (:freq node)
                             old-freqs (get-trie trie sub-x key [])
                             new-freqs (conj old-freqs freq)]
                         (assoc-trie trie sub-x key new-freqs)))))
        [sp1-left-freqs-trie, sp1-right-freqs-trie]
        (map (fn [key]
               (reduce (find-sp1 key)
                       empty-trie
                       (seq-trie freq-trie)))
             [:sp1-left-freqs, :sp1-right-freqs])
        make-entropy-trie (fn [sp1-freqs-key sp1-entropy-key]
                            (fn [trie [x node]]
                              (if-let [sp1-freqs (sp1-freqs-key node)]
                                (let [sp1-entropy (entropy sp1-freqs alphabet-size)]
                                  (assoc-trie trie x sp1-entropy-key sp1-entropy))
                                trie)))
        [sp1-left-entropy-trie, sp1-right-entropy-trie]
        (map (fn [[sp1-freqs-key sp1-entropy-key sp1-freqs-trie]]
               (reduce (make-entropy-trie sp1-freqs-key sp1-entropy-key)
                       empty-trie
                       (seq-trie sp1-freqs-trie)))
             [[:sp1-left-freqs :sp1-left-entropy sp1-left-freqs-trie]
              [:sp1-right-freqs :sp1-right-entropy sp1-right-freqs-trie]])]
    {:freq freq-trie
     :sp1-left-entropy sp1-left-entropy-trie
     :sp1-right-entropy sp1-right-entropy-trie}))


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
; the two parts to be split a secondary criterion. This way, we can
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
