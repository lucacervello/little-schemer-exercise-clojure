(ns little-schemer-exercise-clojure.core)

(defn atom?
  "Atom in the meaning of Scheme e CommonLisp"
  [x]
  (not (coll? x)))

(defn lat?
  "lat? return true if the sequence is composed only by atoms"
  [lst]
  (cond
    (nil? (seq lst)) true
    (atom? (first lst)) (lat? (rest lst))
    :else false))

(defn my-member?
  "return true if in the seq there is a"
  [a lst]
  (if (seq lst)
    (if (= a (first lst))
      true
      (my-member? a (rest lst)))
    false))

(defn member? [a lst]
  (if (seq lst)
    (or (= a (first lst))
        (member? a (rest lst)))
    false))

;; 3. CONS THE MAGNIFICENT

(defn rember [a lst]
  (if (seq lst)
    (if (= a (first lst))
      (rest lst)
      (cons (first lst) (rember a (rest lst))))
    []))
