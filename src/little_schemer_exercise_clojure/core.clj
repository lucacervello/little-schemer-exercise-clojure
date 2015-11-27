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


;; 4. Numbers Games

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

(defn pow [n m]
  (if (zero? m)
    1
    (mult n (pow n (dec m)))))

(defn division [n m]
  (if (minor-then n m)
    0
    (inc (division (- n m) m))))

(defn length [lat]
  (if (seq lat)
    (inc (length (rest lat)))
    0))

(defn pick [n lat]
  (if (zero? n)
    (first lat)
    (pick (dec n) (rest lat))))

(defn rempick [n lat]
  (if (or (zero? n) (empty? lat))
    (rest lat)
    (cons (first lat) (rempick (dec n) (rest lat)))))

(defn no-nums [lat]
  (cond
    (empty? lat) []
    (number? (first lat)) (no-nums (rest lat))
    :else (cons (first lat) (no-nums (rest lat)))))

(defn all-nums [lat]
  (cond
    (empty? lat) []
    (not (number? (first lat))) (all-nums (rest lat))
    :else (cons (first lat) (all-nums (rest lat)))))

(defn occur [a lat]
  (if (seq lat)
    (if (= a (first lat))
      (inc (occur a (rest lat)))
      (occur a (rest lat)))
    0))

(defn one? [n]
  (= n 1))


;;; 5. *OH MY GAWD*: IT'S FULL OF STARS

(defn rember* [a lat]
  (cond
    (empty? lat) []
    (atom? (first lat)) (if (= (first lat) a)
                          (rember* a (rest lat))
                          (cons (first lat)
                                (rember* a (rest lat))))
    :else (cons (rember* a (first lat))
                (rember* a (rest lat)))))

(defn insertR* [new old lat]
  (cond
    (empty? lat) []
    (atom? (first lat)) (if (= (first lat) old)
                          (cons (first lat)
                                (cons new (insertR* new old (rest lat))))
                          (cons (first lat) (insertR* new old (rest lat))))
    :else (cons (insertR* new old (first lat))
                (insertR* new old (rest lat)))))

(defn occur* [a lat]
  (cond
    (empty? lat) 0
    (atom? (first lat)) (if (= a (first lat))
                          (inc (occur* a (rest lat)))
                          (occur* a (rest lat)))
    :else (+ (occur* a (first lat)) (occur* a (rest lat)))))

(defn subst* [new old lat]
  (cond
    (empty? lat) []
    (atom? (first lat)) (if (= old (first lat))
                          (cons new (subst* new old (rest lat)))
                          (cons (first lat) (subst* new old (rest lat))))
    :else (cons (subst* new old (first lat))
                (subst* new old (rest lat)))))

(defn insertL* [new old lat]
  (cond
    (empty? lat) []
    (atom? (first lat)) (if (= old (first lat))
                          (cons new (cons (first lat)
                                          (insertL* new old (rest lat))))
                          (cons (first lat) (insertL* new old (rest lat))))
    :else (cons (insertL* new old (first lat))
                (insertL* new old (rest lat)))))

(defn member* [a lat]
  (cond
    (empty? lat) false
    (atom? (first lat)) (or (= (first lat) a) (member* a (rest lat)))
    :else (or (member* a (first lat)) (member* a (rest lat)))))

(defn leftmost [lat]
  (if (atom? (first lat))
    (first lat)
    (leftmost (first lat))))

(defn eqlist? [col1 col2]
  (cond
    (and (empty? col1) (empty? col2)) true
    (or (empty? col1) (empty? col2)) false
    (and (atom? (first col1)) (atom? (first col2))) (and (= (first col1)
                                                           (first col2))
                                                         (eqlist? (rest col1)
                                                                  (rest col2)))
    (or (atom? (first col1)) (atom? (first col2))) false
    :else (and (eqlist? (first col1) (first col2))
               (eqlist? (rest col1) (rest col2)))))


;;; 6. SHADOWS


(defn numbered? [aexp]
  (cond
    (atom? aexp) (number? aexp)
    :else (and (numbered? (first aexp))
               (numbered? (first (rest (rest aexp)))))))

(defn value [nexp]
  (cond
    (atom? nexp) nexp
    (= (first (rest nexp)) '+) (+ (value (first nexp))
                                  (value (first (rest (rest nexp)))))
    (= (first (rest nexp)) 'x) (* (value (first nexp))
                                  (value (first (rest (rest nexp)))))
    :else (pow (value (first nexp))
               (value (first (rest (rest nexp)))))))

;; Ho deciso di saltare l'ultima parte del capitolo...
;; quella con le parentesi al posto dei numeri


;;; 7. FRIENDS AND RELATIONS

(defn set? [lat]
  (cond
    (empty? lat) true
    (member? (first lat) (rest lat)) false
    :else (set? (rest lat))))

(defn make-set [lat]
  (cond
    (empty? lat) []
    (member? (first lat) (rest lat)) (make-set (rest lat))
    :else (cons (first lat) (make-set (rest lat)))))

(defn make-set-with-multi [lat]
  (if (empty? lat)
    []
    (cons (first lat)
          (make-set-with-multi (multirember (first lat) (rest lat))))))

(defn make-set1 [lat]
  (if (seq lat)
    (cons (first lat) (make-set1 (multirember (first lat) (rest lat))))))

(defn subset? [set1 set2]
  (if (and (set? set1) (set? set2))
    (cond
      (empty? set1) true
      (member? (first set1) set2) (subset? (rest set1) set2)
      :else false)))

(defn eqset? [set1 set2]
  (and (subset? set1 set2)
       (subset? set2 set1)))

(defn intersect? [set1 set2]
  (if (empty? set1)
    false
    (or (member? (first set1) set2)
        (intersect? (rest set1) set2))))

(defn intersect [set1 set2]
  (cond
    (empty? set1) []
    (member? (first set1) set2) (cons (first set1)
                                      (intersect (rest set1) set2))
    :else (intersect (rest set1) set2)))

(defn union [set1 set2]
  (cond
    (empty? set1) set2
    (member? (first set1) set2) (union (rest set1) set2)
    :else (cons (first set1) (union (rest set1) set2))))

(defn intersectall [l-set]
  (if (empty? (rest l-set))
    (first l-set)
    (intersect (first l-set)
               (intersectall (rest l-set)))))

(defn a-pair? [x]
  (cond
    (atom? x) false
    (empty? x) false
    (empty? (rest x)) false
    (empty? (rest (rest x))) true
    :else false ))

;; First and second are bult-in in the language

(defn build [f s]
  (cons f (cons s [])))

(defn third [lst]
  (first (rest (rest lst))))

(defn fun? [rel]
  (set? (firsts rel)))
