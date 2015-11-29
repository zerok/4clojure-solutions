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


;; 52: Destructuring
(deftest test-destructuring
  (testing "1"
    (is (= [2 4] (let [[a b c d e] [0 1 2 3 4]] [c e])))))


;; 134: A nil key
(def a-nil-key #(nil? (get %2 %1 false)))
(deftest test-a-nil-key
  (testing "1"
    (is (true?  (a-nil-key :a {:a nil :b 2}))))
  (testing "2"
    (is (false?  (a-nil-key :b {:a nil :b 2}))))
  (testing "3"
    (is (false?  (a-nil-key :c {:a nil :b 2})))))
