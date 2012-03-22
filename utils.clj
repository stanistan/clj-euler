(defn fib [n]
  (if (<= n 1) 
    n
    (+ (fib (dec n)) (fib (- n 2)))))

(def fib (memoize fib))

(defn divides? [n d]
  (zero? (mod d n)))

(defn vcontains? [v thing]
  (not (nil? (some #{thing} v))))

(defn push-vec [v piece]
  (if (vcontains? v piece) 
    v
    (conj v piece)))

(def fib-seq 
  ((fn rfib [a b] 
     (lazy-seq (cons a (rfib b (+ a b)))))
   0 1))

(defn palindrome? [n]
  (let [e (reverse (str n))]
    (= e (reverse e))))

(defn push-if-contains [v piece]
  (if (vcontains? v piece) v (conj v piece)))

(defn prime-factors 
  ([n] (prime-factors n 2 []))
  ([n factor] (prime-factors n factor []))
  ([n factor factors] 
    (let [r (conj factors factor)]
      (if (= n factor) r
        (if (divides? factor n)
            (prime-factors (/ n factor) factor r)
            (prime-factors n (inc factor) factors))))))

(defn prime? [n]
  (if (<= n 1) false
      (empty? (take 1 (for [x (range 2 (Math/ceil (Math/sqrt n)))
        :when (divides? x n)] x)))))

(defn prime-range [start end]
  (filter prime? (range start end)))

(def prime-gen
  (let [primes (atom [])]
    (for [n (iterate inc 2)
         :when (not-any? #(zero? (rem n %))
                         (filter #(<= % (Math/sqrt n)) @primes))]
    (do (swap! primes conj n) n))))

(defn nth-prime [n]
  (last (take n prime-gen)))

(defn count-pieces [arr]
  { :num (first arr) 
    :count (count arr) })

(defn pow [b p]
  (Math/pow b p))

(defn square [n]
  (pow n 2))
