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
      (cons (first lst) (rember a (rest lst))))))

(defn firsts [lst]
  (if (seq lst)
    (cons (first (first lst))
          (firsts (rest lst)))))

(defn insertR [new old lat]
  (if (seq lat)
    (if (= old (first lat))
      (cons (first lat) (cons new (rest lat)))
      (cons (first lat) (insertR new old (rest lat))))))

(defn insertL [new old lat]
  (if (seq lat)
    (if (= old (first lat))
      (cons new lat)
      (cons (first lat) (insertL new old (rest lat))))))

(defn subst [new old lat]
  (if (seq lat)
    (if (= old (first lat))
      (cons new (rest lat))
      (cons (first lat) (subst new old (rest lat))))))

(defn multirember [a lat]
  (if (seq lat)
    (if (= a (first lat))
      (multirember a (rest lat))
      (cons (first lat) (multirember a (rest lat))))))

(defn multiinsertR [new old lat]
  (if (seq lat)
    (if (= old (first lat))
      (cons (first lat) (cons new (multiinsertR new old (rest lat))))
      (cons (first lat) (multiinsertR new old (rest lat))))))

(defn multiinsertL [new old lat]
  (if (seq lat)
    (if (= old (first lat))
      (cons new (cons (first lat) (multiinsertL new old (rest lat))))
      (cons (first lat) (multiinsertL new old (rest lat))))))

(defn multisubst [new old lat]
  (if (seq lat)
    (if (= old (first lat))
      (cons new (multisubst new old (rest lat)))
      (cons (first lat) (multisubst new old (rest lat))))))


;; 4. NUMBERS GAMES

(defn plus
  "define plus only with zero? inc dec"
  [n m]
  (if (zero? m)
    n
    (plus (inc n) (dec m))))

(defn minus
  "define minus only with zero inc dec"
  [n m]
  (if (zero? m)
    n
    (minus (dec n) (dec m))))

;; The book example is different

(defn addtup
  "apply with +"
  [lat]
  (if (seq lat)
    (+ (first lat) (addtup (rest lat)))
    0))

(defn addtup-tail-rec "tail recursive addtup"
  [lat]
  (loop [x lat]
    (if-not (seq x)
      0
      (recur (rest x)))))

(defn mult [n m]
  (if (zero? m)
    0
    (plus n (mult n (dec m)))))

(defn tup+ [tup1 tup2]
  (cond
    (not (seq tup1)) tup2
    (not (seq tup2)) tup1
    :else (cons (plus (first tup1) (first tup2))
                (tup+ (rest tup1) (rest tup2)))))

(defn greater-then [n m]
  (cond
    (zero? n) false
    (zero? m) true
    :else (greater-then (dec n) (dec m))))

(defn minor-then [n m]
  (cond
    (zero? m) false
    (zero? n) true
    :else (minor-then (dec n) (dec m))))

(defn equal [n m]
  (not (or (greater-then n m) (minor-then n m))))
