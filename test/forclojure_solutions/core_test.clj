(ns forclojure-solutions.core-test
  (:require [clojure.test :refer :all]
            [forclojure-solutions.core :refer :all]))

;; 1: Nothing but the truth
(deftest test-nothing-but-the-truth
  (testing "1"
    (is (= true true))))

;; 2: Simple math
(deftest test-simple-math
  (testing "1"
    (is (= (- 10 (* 2 3)) 4))))

;; 24: Fibonacci Sequence
(defn first-n-fib [n]
  (loop [cur 1
         result []]
    (if (> cur n)
      result
      (recur (+ cur 1)
             (if (< cur 3)
               (conj result 1)
               (conj result (+
                             (last result)
                             (first (take-last 2 result))))
               )))))
(deftest test-fibonacci-sequence
  (testing "first 3"
    (is (= (first-n-fib 3) '(1 1 2))))
  (testing "first 6"
    (is (= (first-n-fib 6) '(1 1 2 3 5 8))))
  (testing "first-8"
    (is (= (first-n-fib 8) '(1 1 2 3 5 8 13 21)))))

;; 29: Get the Caps
(defn get-the-caps [s]
  (apply str (filter #(and (<= (int %) (int \Z)) (>= (int %) (int \A))) s)))
(deftest test-get-the-caps
  (testing "mixed set 1"
    (is (= (get-the-caps "HeLlO, WoRlD!") "HLOWRD")))
  (testing "nothing"
    (is (empty? (get-the-caps "nothing"))))
  (testing "mixed set 2"
    (is (= (get-the-caps "$#A(*&987Zf") "AZ"))))


;; 42: Factorial fun
(defn fact [n]
  (loop [cur n
         res n]
    (if (= 1 cur)
      res
      (recur (dec cur) (* res (dec cur))))))
(deftest test-factorial-fun
  (testing "1"
    (is (= (fact 1) 1)))
  (testing "3"
    (is (= (fact 3) 6)))
  (testing "5"
    (is (= (fact 5) 120)))
  (testing "8"
    (= (fact 8) 40320)))


;; 48: Intro to some
(deftest test-intro-to-some
  (testing "1"
    (is (= 6 (some #{2 7 6} [5 6 7 8]))))
  (testing "2"
    (is (= 6 (some #(when (even? %) %) [5 6 7 8])))))

;; 51: Advanced destructuring
(deftest test-advanced-destructuring
  (let [value [1 2 3 4 5]]
    (testing "1"
      (is (= [1 2 [3 4 5] [1 2 3 4 5]]
             (let [[a b & c :as d] value]
               [a b c d])
             )))))

;; 52: Destructuring
(deftest test-destructuring
  (testing "1"
    (is (= [2 4] (let [[a b c d e] [0 1 2 3 4]] [c e])))))


;; 66: Greatest common divisor
(deftest test-gcd
  (let [gcd (fn [a b]
              (let [range (range 1 (+ (min a b) 1))]
                (last (filter (fn [cur] (and
                                   (= 0 (mod a cur))
                                   (= 0 (mod b cur)))) range))
                ))]
    (testing "a"
      (is (= (gcd 2 4 ) 2)))
    (testing "b"
      (is (= (gcd 10 5) 5)))
    (testing "c"
      (is (= (gcd 5 7) 1)))
    (testing "d"
      (is (= (gcd 1023 858) 33)))
    ))

;; 83: A Half-Truth
(deftest test-a-half-truth
  (let [func (fn [& args]
               (let [num (count args)
                     numTrues (reduce (fn [res cur] (if (true? cur) (+ res 1) res))
                             0
                             args)]
                 (if (= num numTrues)
                   false
                   (< 0 numTrues))))]
    (testing "all false"
      (is (= false (func false false))))
    (testing "some true"
      (is (= true (func true false))))
    (testing "all true"
      (is (= false (func true))))
    )
  )

;; 90: Cartesian Product
(deftest test-cartestian-product
  (let [func (fn [a b]
               (set (for [x a y b] [x y])))]
    (testing "numbers"
      (is (= (func #{1 2 3} #{4 5})
             #{[1 4] [2 4] [3 4] [1 5] [2 5] [3 5]})))))

;; 107: Simple closures
(deftest test-simple-closures
  (let [func (fn [pow] (fn [b] (int (java.lang.Math/pow b pow))))]
    (testing "a"
      (is (= 256
             ((func 2) 16),
             ((func 8) 2))))
    (testing "b"
      (is (= [1 8 27 64] (map (func 3) [1 2 3 4]))))
    (testing "c"
      (is (= [1 2 4 8 16] (map #((func %) 2) [0 1 2 3 4]))))
    )
  )

;; 134: A nil key
(def a-nil-key #(nil? (get %2 %1 false)))
(deftest test-a-nil-key
  (testing "1"
    (is (true?  (a-nil-key :a {:a nil :b 2}))))
  (testing "2"
    (is (false?  (a-nil-key :b {:a nil :b 2}))))
  (testing "3"
    (is (false?  (a-nil-key :c {:a nil :b 2})))))
