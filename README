# esa-wordseg

An implementation of the ESA unsupervised word segmentation algorithm
in Clojure.

http://www.mitpressjournals.org/doi/pdfplus/10.1162/COLI_a_00058

## Usage

Simply open a REPL with the program classes in the classpath
(e.g. through Leiningen while standing in the project directory),

      lein repl

load the namespace with the main functions,

     (use 'esa-wordseg.core)

and issue a call to either the esa-main function to segment strings
into sequences of words or esa-on-files to process entire files.

     (esa-on-files "icwb2-data/testing/cityu_test.utf8"
                   "icwb2-data/testing/cityu_test.out"
                   0 10 0.5 0.000001 [:newline])

The last five arguments correspond to the following options:

    * the maximum number of ESA iterations (no limit of the value is
      positive)
    * the maximum length of a sequence being processed by the Segment
      algorithm in Selection (limits accuracy but makes the problem
      manageable computationally)
    * the exponent used in the LRV formula (Equation (10) in the
      paper) used to compute the goodness of a gap
    * the parameter used in additive smoothing of the distributions of
      the SP1 sets
    * the methods which will be used to initially divide the input
      data into smaller blocks, expressed as a sequence of keywords,
      where :newline stands for splitting on newlines, :punct stands
      for splitting on Unicode punctuation and :char-class stands for
      splitting on the boundaries between characters in and out of the
      Chinese script

## Notes on current performance

The current implementation isn't shy about taking about 1.77 GB of
memory and taking 2 minutes to fully process the humble
icwb2-data/testing/cityu_test.utf8 file.

Given the state of the implementation, hashmaps might yield both a
better memory footprint and better performance.

## What next

Next I would like to open a new branch in which to explore the benefit
of replacing my tries with Clojure hashmaps, which are themselves
implemented as 32-ary tries.

Currently, the SP1 distributions are smoothed using additive
smoothing, because Good-Turing wasn't suitable due to the sparsity of
the frequency frequencies and otherwise the zero entropies break the
entire algorithm.

The approach is also riddled with the same issues which befall
language modelling. For computing LRV, the gap goodness, we rely on
the entropy of a distribution of characters preceding/following a
given n-gram. The original approach doesn't discern between the
reliability of the smaller n-grams and the detail of the larger
n-grams and ends up using the most detailed, least reliable ones all
the time. Instead of just using additive smoothing on the SP1
distributions, it could be more sensible to interpolate the SP1
distributions for the given n-grams and its subsequences.

## License

Copyright (C) 2011 Jiri Marsik

Distributed under the Eclipse Public License, the same as Clojure.
