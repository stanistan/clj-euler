
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
		(last (sort (for [x r y r :when (palindrome? (* x y))] (* x y))))))

(defn problem5 []
	(let [r (range 2 21)
				factored (partition-by identity (flatten (map prime-factors r)))
				grouped (group-by :num (map count-pieces factored))
				get-multi (fn [n]
					(let [a (last (sort-by :count n))] 
						(Math/pow (:num a) (:count a))))]
		(reduce * (map #(get-multi (second %)) grouped))))

; (println (str "problem1: " (problem1)))
; (println (str "problem2: " (problem2)))
; (println (str "problem3: " (problem3)))
; (println (str "problem4: " (problem4)))
; (println (str "problem5: " (problem5)))