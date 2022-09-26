(ns dbinomial)

(defn exp
  "a ^ b"
  [a b]
  (reduce * (repeat b a)))

(defn factorial-div-factorial
  "factorial of a divided by factorial of b, assuming b < a" [a b]
  (reduce * (range (inc b) (inc a))))

(defn factorial
  "factorial of a" [a]
  (reduce * (range 1 (inc a))))

;; shouldn't produce overflow
(defn n-of-permutations [x n]
  (/ (factorial-div-factorial n x)
     (factorial (- n x))))

(defn dbinom [x n prob]
  (* (n-of-permutations x n)
     (exp prob x)
     (exp (- 1 prob) (- n x))))

(def grid-p
  "The grid for grid approximation"
  (map #(/ % 200) (range 0 201)))

(defn relative-likelihood 
  "Calculate the relative likelihood for a collection of p."
  [x n]
  (map #(dbinom x n %) grid-p))

(def relative-likelihood-memo
  (memoize relative-likelihood))


(defn bayesian-binary-update [found prior]
  (let [update-if-found grid-p    ; range from 0 to 1 inclusive with size elements
        update-if-not-found (map #(- 1 %) grid-p)]
    (map * prior (if found update-if-found update-if-not-found))))

(defn relative-likelihood-for-this-sequence [x n]
  (let [uniform-prior (repeat (count grid-p) 1)
        times-found x
        times-not-found (- n x)]
    (-> ; repeatedly update depending on times-found and times-not-found :
     (reduce (fn [last-step _] (bayesian-binary-update true last-step)) uniform-prior (repeat times-found 1))
     (#(reduce (fn [last-step _] (bayesian-binary-update false last-step)) % (repeat times-not-found 1))))))

(defn standardize
  "make average of values in coll r = 1"
  [r]
  (let [average (/ (apply + r) (count r))]
    (map #(/ % average) r)))

(defn count-land-or-water [samples]
  (let [n (count samples)
        land (count (filter (partial = :l) samples))
        water (count (filter (partial = :w) samples))]
    [n land water]))

(defn relative-likelihood-from-samples [samples]
  (let [[n _ water] (count-land-or-water samples)]
    (relative-likelihood water n)))

(defn posterior-distribution
  "If no prior given assume uniform prior."
  ([samples]
  (->>
   (relative-likelihood-from-samples samples)
   standardize))
  ([samples prior]
   (->>
    (relative-likelihood-from-samples samples)
    (map * prior)
    standardize)))

(defn round-number-to-grid
  [f]
  (/ (.round js/Math (* 200 f)) 200))


(defn r-likelihood-from-samples-for-this-sequence [samples]
  (let [[n _ water] (count-land-or-water samples)]
    (relative-likelihood-for-this-sequence water n)))

(defn sample-posterior 
  "Calculate posterior from :w or :l samples and then 
   take a random sample from posterior distribution"
  [samples]
  (let [[n _ water] (count-land-or-water samples)
        relative-likelihood-dis (relative-likelihood-from-samples samples)
        relative-likelihood-max (apply max relative-likelihood-dis)
        ]
    (first (filter (complement nil?)
                   (repeatedly
                    #(let [p (rand)
                           relative-likelihood-of-p (dbinom water n p)
                           accept-or-reject (rand relative-likelihood-max)]
                       (if (< accept-or-reject relative-likelihood-of-p)
                         p
                         nil)))))))
(defn  median [coll-of-numbers]
  (nth (sort coll-of-numbers)
       (js/Math.floor (/ (count coll-of-numbers) 2))))

(defn linear-loss
  "Given pos-dis posterior distribution find the expected loss for a grid of values of d."
  [pos-dis]
  (map
   (fn [d]
     (/ (apply + (map (fn [p, likelihood] (* (abs (- d p)) likelihood)) grid-p pos-dis)) (count grid-p)))
   grid-p))