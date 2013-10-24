(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [acc base exp]
                 (if (zero? exp)
                   acc
                   (recur (* acc base) base (dec exp))))]
    (helper 1 base exp)))

(defn last-element [a-seq]
  (let [helper (fn [fir res]
                 (if (empty? res)
                   fir
                   (recur (first res) (rest res))))]
    (helper (first a-seq) (rest a-seq))))

(defn seq= [seq1 seq2]
  (let [a (first seq1)
        b (first seq2)]
    (cond
     (and (empty? seq1) (empty? seq2)) true
     (or (empty? seq1) (empty? seq2)) false
     (not= (compare a b) 0) false
     :else (recur (rest seq1) (rest seq2)))))

(defn find-first-index [pred a-seq]
  (loop [index 0
         p pred
         A a-seq]
    (cond
     (empty? A) nil
     (p (first A)) index
     :else (recur (inc index) p (rest A)))))

(defn avg [a-seq]
  (loop [sum 0
         A a-seq
         c (count a-seq)]
    (if (empty? A)
      (/ sum c)
      (recur (+ sum (first A)) (rest A) c))))

(defn toggle [a-set elem]
  (if (contains? a-set elem) (disj a-set elem) (conj a-set elem)))

(defn parity [a-seq]
  (loop [B #{}
         A a-seq]
    (if (empty? A)
      B
      (recur (toggle B (first A)) (rest A)))))

(defn fast-fibo [n]
  (loop [f1 0
         f2 1
         n n]
    (if (== n 0)
      f1
      (recur f2 (+ f1 f2) (dec n)))))

(defn cut-at-repetition [a-seq]
  (loop [B []
         A a-seq]
    (cond
     (empty? A) B
     (contains? (set B) (first A)) B
     :else (recur (conj B (first A)) (rest A)))))

