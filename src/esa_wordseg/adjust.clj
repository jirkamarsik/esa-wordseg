(ns esa-wordseg.adjust
  "The Adjustment part of ESA where we discount the frequencies
  of proper subsequences of words from the previous Selection."
  (:use (esa-wordseg [trie :only [disj-trie]]
                     [misc :only [flatten-leafs proper-subvecs]])))

(defn discount-subsegs
  "Takes the results of the previous Selection (nested vectors
  describing the segmentation) and the original frequency trie
  derived from the data. Returns the adjusted frequency trie,
  with frequencies of proper subwords decreased."
  [freq-trie segs]
  (reduce disj-trie freq-trie (mapcat proper-subvecs (flatten-leafs segs))))
