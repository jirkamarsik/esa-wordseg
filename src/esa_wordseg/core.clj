(ns esa-wordseg.core
  (:use (esa-wordseg trie preproc eval)))

(def test-file "D:\\Dev\\esa-wordseg\\icwb2-data\\testing\\cityu_test.utf8")

(def test-data (slurp test-file))

(def test-charvecs (map vec (split-on-newline [cityu-data])))

(def test-alphabet-size (get-alphabet-size test-charvecs))

(def test-limit 10)

(def test-freq-trie (get-frequency-trie test-charvecs test-limit))

(def test-smooth-param 0.000001)

(def test-tries (add-entropy-tries test-freq-trie test-alphabet-size test-smooth-param))

(def test-averages (get-average-statistics test-tries))

(def test-stats {:tries test-tries, :averages test-averages})

(def test-exp 1)

(def the-word (subvec (nth test-charvecs 2) 0 2))

(def the-following-word (subvec (nth test-charvecs 2) 2 4))
