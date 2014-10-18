(ns looping-is-recursion)

(defn power [base exp]
    (loop [cnt exp acc 1]
       (if (zero? cnt)
            acc
          (recur (dec cnt) (* acc base))
     ))
  )

(defn last-element [a-seq]
  (cond (empty? a-seq) nil
        :else  (if (== 1 (count a-seq))
                  (first a-seq)
                  (recur (rest a-seq))
               )
        )


  )

(defn seq= [seq1 seq2]
  (cond (not (== (count seq1) (count seq2))) false
        :else
                    (if(and (empty? seq1)(empty? seq2))
                            true
                           (cond (== (first seq1) (first seq2)) (recur (rest seq1) (rest seq2))
                             :else false
                           )
                      )

    )
  )

(defn find-first-index [pred a-seq]
(loop [counter 0 currentSeq a-seq]
   (cond (empty? currentSeq) nil
         :else  (if (pred (first currentSeq))
                     counter
                     (recur (inc counter) (rest currentSeq))
                )
    )

)
)

(defn avg [a-seq]
 (loop [counter 0 currentAvg 0 currentSeq a-seq]
   (cond (empty? currentSeq) (/ currentAvg counter)
         :else (recur (inc counter) (+ currentAvg (first currentSeq)) (rest currentSeq))
    )
  )
)

(defn toggle [a-set elem]
 (loop [counter 0 currentSeq a-set temp 0]
   (cond (empty? currentSeq)  temp
          (contains? (set currentSeq) elem)  (recur (inc counter) (rest currentSeq) (inc temp))
         :else (recur (inc counter) (rest currentSeq) temp)
    )
  )
)

(defn countOf[a-seq elem]
   (loop [number 0 currentSeq a-seq]
      (cond (empty? currentSeq) number
          (= (first currentSeq) elem) (recur (inc number) (rest currentSeq))
            :else (recur number (rest currentSeq))
      )
   )
 )

(defn filteredSeq[a-seq elemToRemove]
  (loop [currentSeq a-seq finalSeq []]
      (cond (empty? currentSeq) finalSeq
             (and (contains? (set finalSeq) elemToRemove) (= (first currentSeq) elemToRemove)) (recur (rest currentSeq) finalSeq)
             (even? (countOf currentSeq elemToRemove)) (recur (rest currentSeq) finalSeq);
            :else (recur (rest currentSeq) (conj finalSeq (first currentSeq)))
       )
    )
  )

(defn parity [a-seq]
  (loop [resultset (set []) currentSeq a-seq]
    (cond (empty? currentSeq) resultset
           (odd? (countOf currentSeq (first currentSeq))) (recur (conj resultset (first currentSeq)) (filteredSeq (rest currentSeq) (first currentSeq)))
           :else (recur resultset (filteredSeq (rest currentSeq) (first currentSeq)))
  )
)
)

(defn fast-fibo [n]
 (loop [fibn 1 fibnm1 0 currentN 1]
   (cond (== 0 n) fibnm1
         (== currentN n) fibn
         :else (recur (+ fibn fibnm1) ( - (+ fibn fibnm1) fibnm1) (inc currentN))
    )
   )
  )

(defn cut-at-repetition [a-seq]
 (loop [consSeq [] orginSeq a-seq]

   (if (or (contains? (set consSeq) (first orginSeq)) (empty? orginSeq))
       consSeq
       (recur (conj consSeq (first orginSeq)) (rest orginSeq))
     )
   )
  )

