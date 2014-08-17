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
