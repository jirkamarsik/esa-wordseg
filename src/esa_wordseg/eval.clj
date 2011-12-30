(ns esa-wordseg.eval
  "The evaluation part of ESA. Here we compute the goodness values
  using the extracted statistics."
  (:use (esa-wordseg [trie :only [get-trie]])))

;; Note to myself: The trie's missing implementation of the associative
;; interface makes for less symmetry in accessing nested maps.

(defn individual-goodness
  "Takes the (stat)istic(s), a map containing the 'trie' of per-sequence
  quantities and the vector of their 'averages', and using them computes
  the individual goodness score of the supplied character sequence x."
  [stats x]
  (let [length (count x)
        freq (get-trie (get-in stats [:tries :freq]) x)
        average-freq (get-in stats [:averages :freq length])]
    (Math/pow (/ freq average-freq) length)))

(defn gap-goodness
  "Takes the (stat)istic(s), a map containing the 'trie' of per-sequence
  quantities and the vector of their 'averages', and using them computes
  the goodness of a gap between the sequences x and y. 'exp' is the exponent
  used in the formula."
  [stats x y exp]
  (let [right-entropy-of-x (get-trie (get-in stats [:tries :sp1-right-entropy]) x)
        average-right-entropy (get-in stats [:averages :sp1-right-entropy (count x)])
        x-contrib (/ right-entropy-of-x average-right-entropy)
        left-entropy-of-y (get-trie (get-in stats [:tries :sp1-left-entropy]) y)
        average-left-entropy (get-in stats [:averages :sp1-left-entropy (count y)])
        y-contrib (/ left-entropy-of-y average-left-entropy)]
    (Math/pow (* x-contrib y-contrib) exp)))

(defn combined-goodness
  "Takes the (stat)istic(s) computed from the data and using them computes
  the combined goodness of two consecutive sequences x and y, applying
  'exp' as the exponent in the LRV (gap-goodness) formula."
  [stats x y exp]
  (* (individual-goodness stats x)
     (individual-goodness stats y)
     (gap-goodness stats x y exp)))
