(ns esa-wordseg.select
  "The selection part of ESA. Here, we take character vectors and using
  our accumulated statistics we try to break them up into smaller segments."
  (:use (esa-wordseg [eval :only [individual-goodness gap-goodness]])))

(defn optimal-segmentation
  "This should be invisible."
  [stats charvec exp]
  (let [splits (for [point (range 1 (- (count charvec) 1))]
                 (let [left-seg (subvec charvec 0 point)
                       right-seg (subvec charvec point)
                       left-optimum (optimal-segmentation stats left-seg exp)
                       right-optimum (optimal-segmentation stats right-seg exp)
                       ]
                   {:segmentation [(:segmentation left-optimum)
                                   (:segmentation right-optimum)]
                    :goodness (* (:goodness left-optimum)
                                 (:goodness right-optimum)
                                 (gap-goodness stats left-seg right-seg exp))}))]
    (reduce (partial max-key :goodness)
            {:segmentation charvec
             :goodness (individual-goodness stats charvec)}
            splits)))

(def optimal-segmentation
  "Returns the best segmentation of a character vector and the goodness
  of the segmentation. Arguments stats and exp supply the statistical
  data and exponent for the LRV, gap goodness, formula."
  (memoize optimal-segmentation))

(defn segment
  "Returns the optimal segmentation of the charvec represented as nested
  vectors. The statistical data and exponent used in the computation of LRV,
  the gap goodness are taken from the stats and exp args, respectively."
  [stats charvec exp]
  (:segmentation (optimal-segmentation stats charvec exp)))
