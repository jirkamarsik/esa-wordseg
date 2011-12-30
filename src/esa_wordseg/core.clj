(ns esa-wordseg.core
  (:use (esa-wordseg trie
                     preproc
                     eval
                     select
                     adjust
                     misc)))

;; Some definitions which were useful at the REPL to test and probe the program.
(def test-defs
  '[(def test-file "D:\\Dev\\esa-wordseg\\icwb2-data\\testing\\cityu_test.utf8")
    (def test-data (slurp test-file))
    (def test-charvecs (map vec (map (partial map int) (split-on-newline [test-data]))))
    (def test-alphabet-size (get-alphabet-size test-charvecs))
    (def test-limit 10)
    (def test-freq-trie (get-frequency-trie test-charvecs test-limit))
    (def test-smooth-param 0.000001)
    (def test-tries (add-entropy-tries test-freq-trie test-alphabet-size test-smooth-param))
    (def test-averages (get-average-statistics test-tries))
    (def test-stats {:tries test-tries, :averages test-averages})
    (def test-exp 0.01)
    (def test-final-split (split-by-goodness test-stats test-charvecs test-limit test-exp))
    (def test-segmented (map #(segment test-stats % test-exp) test-final-split))])


;; THE MAIN ESA LOOP AND PUBLIC INTERFACE

(defn esa-main
  "The main ESA loop. Takes the string which is to be segmented and
  the following configuration parameters: max-iters for the maximum
  number of ESA iterations (non-positive max-iters means no limit),
  limit for the maximum length of segments to processed by the Selection
  process, exp for the exponent used in the gap goodness (LRV) formula,
  smooth-param for the parameter used in the additive smoothing of SP1
  probability distributions and splits for the indication of how should
  the input be split before processing (a sequence containing some
  of the following keywords: :newline, :punct, :char-class)."
  [data max-iters limit exp smooth-param splits]
  (let [split-fn (reduce comp (map {:newline    split-on-newline
                                    :punct      split-on-punct
                                    :char-class split-on-char-class}
                                   (reverse splits)))
        charvecs (map vec (split-fn [data]))
        alphabet-size (get-alphabet-size charvecs)
        freq-trie (get-frequency-trie charvecs limit)
        tries (add-entropy-tries freq-trie alphabet-size smooth-param)
        averages (get-average-statistics tries)
        stats {:tries tries, :averages averages}
        charvecs-split (split-by-goodness stats charvecs limit exp)]
    (loop [iter 0, stats stats, last-seg nil]
      (let [segmented (map #(segment stats % exp) charvecs-split)
            flat-segments (flatten-leafs segmented)
            adjusted-freq-trie (discount-subsegs freq-trie flat-segments)]
        (println "Iteration " iter)
        (if (or (and (pos? max-iters) (= iter max-iters))
                (= flat-segments last-seg))
          flat-segments
          (recur (inc iter)
                 (assoc-in stats [:tries :freq] adjusted-freq-trie)
                 flat-segments))))))

(defn esa-on-files
  "Reads text from the in-file, segments it using ESA and prints
  the segments seperated by spaces to the out-file. The last five
  arguments have the same meaning as in esa-main."
  [in-file out-file max-iters limit exp smooth-param splits]
  (let [data (slurp in-file)
        segments (esa-main data max-iters limit exp smooth-param splits)
        output (apply str (apply concat (interleave segments (repeat [\space]))))]
    (println "Done!")
    (spit out-file output)))
