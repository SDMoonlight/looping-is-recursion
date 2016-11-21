(ns looping-is-recursion)

(defn power [base exp]
  (let [hp (fn [acc p]
             (if (zero? p)
               acc
               (recur (* acc base) (dec p))
               ))]
    (hp 1 exp)
    ))

(defn last-element [a-seq]
  (if (empty? (rest a-seq))
    (first a-seq)
    (recur (rest a-seq))
    ))

(defn seq= [seq1 seq2]
   (if (= (count seq1) (count seq2))
     (if (empty? (rest seq1))
        (= (first seq1) (first seq2))
        (recur (rest seq1) (rest seq2))
        )
     false))

(defn find-first-index [pred a-seq]
  (loop [ind 0
         iseq a-seq]
    (if (empty? iseq)
      nil
      (if (pred (first iseq))
        ind
        (recur (inc ind) (rest iseq))
        ))))

(defn avg [a-seq]
   (let [denom (count a-seq)]
     (if (zero? denom)
       0
       (/ (loop [rt 0
               rem a-seq]
           (if (empty? rem)
             rt
             (recur (+ rt (first rem)) (rest rem))
             ))
          denom)
       )))

(defn parity [a-seq]
  (loop [par #{}
         rem a-seq]
    (if (empty? rem)
      par
      (recur (if (contains? par (first rem))
               (disj par (first rem))
               (conj par (first rem)))
             (rest rem))
      )))

(defn fast-fibo [n]
  (cond (< n 1)
        0
        (= n 1)
        1
        :else
        (loop [cnt 2
               m1 1
               m2 0]
          (if (= cnt n)
            (+ m1 m2)
            (recur (inc cnt) (+ m1 m2) m1)
            ))))

(defn cut-at-repetition [a-seq]
  (loop [s #{}
         v []
         r a-seq]
    (if (or (contains? s (first r)) (empty? r))
      v
      (recur (conj s (first r)) (conj v (first r)) (rest r))
      )))

