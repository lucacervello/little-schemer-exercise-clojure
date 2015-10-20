(ns little-schemer-exercise-clojure.core)

(defn atom?
  "Atom in the meaning of Scheme e CommonLisp"
  [x]
  (not (coll? x)))
