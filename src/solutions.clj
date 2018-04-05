(defn anagram-find [coll]
  (letfn [(stored? [this in-here]
            (some #(% this) in-here))
          (get-anagrams [like-this from-here]
            (let [anagrams (set (filter 
                                   #(anagram? like-this %) 
                                   from-here))]
              (if (> (count anagrams) 1)
                anagrams)))
          (anagram? [this that]
           (let [this (sort (seq this))
                  that (sort (seq that))]
             (if (= this that) true false)))]
    (loop [current (first coll)
           remaining coll
           so-far #{}]
      (println current remaining so-far)
      (if (empty? remaining)
        so-far
        (if (or (stored? current so-far)
                (nil? (get-anagrams current remaining)))
          (recur (first (rest remaining))
                 (rest remaining)
                 so-far)
          (recur (first (rest remaining))
                 (rest remaining)
                 (conj so-far (get-anagrams current remaining))))))))

(defn totient-function [num]
  (letfn [(gcd [that-number]
            (loop [this-number num
                   that-number that-number]
              (if (zero? that-number)
                this-number
                (recur that-number 
                       (mod this-number that-number)))))]
    (count (reduce #(if (= (gcd %2) 1)
                      (conj %1 %2)
                      %1)
                    [1]
                    (take (- num 1) (iterate inc 2))))))

(defn palindromic-numbers-check [starting-number]
  (filter 
    #(= (Integer/parseInt (apply str (reverse (str %1)))) %)
    (iterate inc starting-number)))

(defn perfect-square [string-coll]
  (apply str (interpose "," 
    (reduce
      #(let [sqrt (int (Math/sqrt (Integer/parseInt %2)))]
        (println %2 sqrt)
        (if (= (* sqrt sqrt) (Integer/parseInt %2))
          (conj %1 %2)
          %1))
      []
      (clojure.string/split string-coll #",")))))

(defn tic-tac-toe-analyze [coll]
  (letfn [(winner-symbols 
            [coll] 
            (first 
              (filter 
                #(or (every? #{:o} %)
                     (every? #{:x} %))
                coll)))
          (index-chooser
            [coll]
            (keep-indexed
               #(if (= 0 (rem %1 4)) %2)
               (flatten coll)))]        
    (if-let [winner-symbol (first (winner-symbols coll))]
      winner-symbol
      (let [adj-coll (partition 3 (apply interleave coll))]
        (if-let [winner-symbol (first (winner-symbols adj-coll))]
          winner-symbol
          (let [adj-coll (partition 3 (concat (index-chooser coll)
                                              (index-chooser (reverse coll))))]
            (if-let [winner-symbol (first (winner-symbols adj-coll))]
              winner-symbol
              nil)))))))
              
(defn sequence-reduction
  ([f coll]
    (reduce
      (fn [cons-coll curr]
        (conj cons-coll (f (last cons-coll) curr)))
      (vector (first coll))
      (rest coll)))
  ([f x y]
    (reduce
      (fn [cons-coll curr]
        (conj cons-coll (f (last cons-coll) curr)))
      (vector x)
      y)))
      
(defn juxtaposition [& fs]
  (fn [& args]
    (reduce
      #(conj %1 (apply %2 args))
      []
      fs)))

(defn function-composition [& fs]
  (fn [& args]
    (loop [f (last fs)
           fs (drop-last fs)
           last-result (apply f args)]
      (if (empty? fs)
        last-result
        (recur (last fs) 
               (drop-last fs) 
               ((last fs) last-result))))))

(defn check [a-string]
  (loop [so-far []
         a-string (seq a-string)]
    (println (str "a-string: " (apply str a-string)
                  "\nso-far: " (apply str so-far)))
    (if (and (empty? a-string) (empty? so-far)) 
      true
      (case (first a-string)
        \( (recur (conj (into [] so-far) \()
                  (rest a-string))
        \{ (recur (conj (into [] so-far) \{)
                   (rest a-string))
        \[ (recur (conj (into [] so-far) \[)
                  (rest a-string))
        \) (if (= (last so-far) \()
             (recur (drop-last so-far)
                    (rest a-string))
             false)
        \} (if (= (last so-far) \{)
             (recur (drop-last so-far)
                     (rest a-string))
             false)
        \] (if (= (last so-far) \[)
             (recur (drop-last so-far)
                     (rest a-string))
             false)
        (if (and (empty? (rest a-string)) 
                 (not (empty? so-far)))
          false
          (recur so-far (rest a-string)))))))

(defn least-common-multiplier [& nums]
  (letfn [(gcd [x y]
            (if (zero? y) x
               (recur y (mod x y))))]
    (reduce #(/ (* %1 %2) (gcd %1 %2)) nums)))
  
(defn type-identification [coll]
  (let [str-coll (first (seq (str coll)))]
    (case str-coll
      (or \c \() :list
      \[ :vector
      \{ :map
      \# :set
      nil)))
    
(defn reverse-interleave [coll freq]
  (loop [coll coll
         so-far '()]
    (if (or (empty? (rest coll)) (= (count so-far) freq))
      (reverse so-far)      
      (recur (rest coll)
             (conj so-far (keep-indexed #(if (= 0 (mod %1 freq)) 
                                        %2) 
                                         coll))))))
                                       
(defn rotate-sequence [n coll]
  (let [from-pos-n (loop [is-pos? (pos? n)
                     current-n n]
                (if is-pos?
                  (if (<= current-n (count coll))
                    current-n
                    (- current-n (count coll)))
                  (let [next-n (+ (count coll) 
                                  current-n)]
                    (recur (pos? next-n) next-n))))]
    (concat (drop from-pos-n coll) (take from-pos-n coll))))
  
(defn replicate-sequence [coll n]
  (reduce #(concat %1 (repeat n %2)) '() coll))
  
(defn split-by-type [coll]
  (reduce (fn [cons-coll current] 
            (loop [inner-coll (first cons-coll)
                   outer-coll cons-coll]
              (println inner-coll)
              (if (empty? inner-coll) 
                (conj cons-coll (vector current))
                (if (some #(= (type current) (type %)) inner-coll)
                  (assoc cons-coll (.indexOf cons-coll inner-coll) 
                                   (conj inner-coll current))
                  (recur (first (rest outer-coll)) (rest outer-coll))))))
          []
          coll))

(defn longest-increasing-seq [coll]
  (reduce 
    #(if (and (> (count %2) 1) (> (count %2) (count %1))) %2 %1)
    []
    (reduce 
      (fn [cons-coll num]
        (let [inner-coll (last cons-coll)
              at-pos (if (nil? inner-coll)
                       0
                       (.indexOf cons-coll inner-coll))]
          (if (= num ((fnil inc num) (last inner-coll)))
            (assoc cons-coll at-pos (conj inner-coll num))
            (if (< (count inner-coll) 2)
              (assoc cons-coll at-pos (vector num))
              (conj cons-coll (vector num))))))
      []
      coll)))

(defn partition-at[freq coll]
  (loop [so-far [[]]
         rem-coll coll]
    (if (empty? rem-coll) 
      (if (< (count (last so-far)) freq) (drop-last so-far) so-far)
      (let [current (first rem-coll)
            last-of-so-far (last so-far)
            at-pos (.indexOf so-far last-of-so-far)]
        (if (< (count last-of-so-far) freq)
          (recur (assoc so-far at-pos (conj last-of-so-far current))
                 (rest rem-coll))
          (recur (conj so-far (vector current))
                 (rest rem-coll)))))))
               
(defn find-distinct-items [coll]
  (reduce
    (fn [cons-coll current]
      (if (some #(= % current) cons-coll)
        cons-coll
        (conj cons-coll current)))
    []
    coll))
  
(defn occurence-count [coll]
  (reduce
    (fn [cons-coll current]
      (if (contains? cons-coll current)
        (update-in cons-coll [current] inc)
        (assoc cons-coll current 1)))
    {}
coll))
