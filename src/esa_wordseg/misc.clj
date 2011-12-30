(ns esa-wordseg.misc
  "Definitions needed in more than one namespace but not intrinsic
  to any of them.")

(defn subvecs
  "Returns all subvectors of vector x with 1 <= length <= limit.
  The limit defaults to (count x)."
  ([x]
     (subvecs x (count x)))
  ([x limit]
     (let [n (count x)]
       (for [i (range n)
             d (range 1 (+ 1 limit))
             :let [j (+ i d)]
             :when (<= j n)]
         (subvec x i j)))))

(defn proper-subvecs
  "Returns all proper subvectors of a vector x."
  [x]
  (let [proper? (fn [sub] (< (count sub) (count x)))]
    (filter proper? (subvecs x))))

(defn flatten-leafs
  "Flattens the given nested sequences down to the point where we
  end up with a sequence of the leaf sequences."
  [xs]
  (let [leaf? (fn [x] (not (coll? (first x))))]
    (filter leaf? (tree-seq (complement leaf?) identity xs))))
