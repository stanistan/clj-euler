
(load-file "utils.clj");
  
(defn problem1 []
  (reduce + (filter #(or (zero? (mod % 3))
	                       (zero? (mod % 5))) 
            (range 1000))))

(defn problem2 []
  (let [func-size #(%1 (count %2))
        push-thing #(conj %2 (func-size %1 %2))
        last-or-zero #(if (nil? (last %)) 0 (last %))
        lazy-fib #(cond (>= (last-or-zero %2) %1) (pop %2)
                        :else (recur %1 (push-thing fib %2)))]
  (reduce + (filter even? (lazy-fib 4000000 [0])))))

(defn problem3 []
  (last (prime-factors 600851475143)))

(defn problem4 [] 
  (let [r (range 100 1000)]
    (reduce max (for [x r y r :when (palindrome? (* x y))] (* x y)))))

(defn problem5 []
  (let [r (range 2 21)
        factored (partition-by identity (flatten (map prime-factors r)))
        grouped (group-by :num (map count-pieces factored))
        get-multi (fn [n]
          (let [a (last (sort-by :count n))] 
            (Math/pow (:num a) (:count a))))]
    (int (reduce * (map #(get-multi (second %)) grouped)))))

(defn problem6 
  "Find the difference between the sum of the squares of 
  the first one hundred natural numbers and the square of the sum."
  []
  (let [r (range 101)
        sum-squares (reduce + (map square r))
        square-sums (square (reduce + r))]
  (int (- square-sums sum-squares))))

(defn problem7 
  "what is the 10,001st prime number?"
  []
  (nth-prime 10001))

(defn problem8
  []
  (let [map-to-digits (fn [n] (map #(Character/digit % 10) n))
        nums "73167176531330624919225119674426574742355349194934
              96983520312774506326239578318016984801869478851843
              85861560789112949495459501737958331952853208805511
              12540698747158523863050715693290963295227443043557
              66896648950445244523161731856403098711121722383113
              62229893423380308135336276614282806444486645238749
              30358907296290491560440772390713810515859307960866
              70172427121883998797908792274921901699720888093776
              65727333001053367881220235421809751254540594752243
              52584907711670556013604839586446706324415722155397
              53697817977846174064955149290862569321978468622482
              83972241375657056057490261407972968652414535100474
              82166370484403199890008895243450658541227588666881
              16427171479924442928230863465674813919123162824586
              17866458359124566529476545682848912883142607690042
              24219022671055626321111109370544217506941658960408
              07198403850962455444362981230987879927244284909188
              84580156166097919133875499200524063689912560717606
              05886116467109405077541002256983155200055935729725
              71636269561882670428252483600823257530420752963450"
        nums (partition 5 1 (reduce str (re-seq #"\d+" nums)))
        digits (map map-to-digits nums)]
  (reduce max (map #(reduce * %) digits))))

; (println (str "problem1: " (problem1)))
; (println (str "problem2: " (problem2)))
; (println (str "problem3: " (problem3)))
; (println (str "problem4: " (problem4)))
; (println (str "problem5: " (problem5)))
; (println (str "problem6: " (problem6)))
; (println (str "problem7: " (problem7)))
; (println (str "problem8: " (problem8)))