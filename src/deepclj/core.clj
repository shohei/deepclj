(ns deepclj.core
  (:gen-class)
  (:use (incanter core stats charts io))) 
(defn AND [x1 x2]
  (let [x (matrix [x1 x2]) 
        w (matrix [0.5 0.5])
        b -0.7
        tmp (+ (sum (mult w x)) b)]
   (if (<= tmp 0)
       0
       1 )))

(defn NAND [x1 x2]
  (let [x (matrix [x1 x2])
        w (matrix [-0.5 -0.5])
        b 0.7
        tmp (+ (sum (mult w x)) b)]
    (if (<= tmp 0)
      0
      1 )))

(defn OR [x1 x2]
  (let [x (matrix [x1 x2])
        w (matrix [0.5 0.5])
        b -0.2
        tmp (+ (sum (mult w x)) b)]
   (if (<= tmp 0)
    0
    1)))

(defn XOR [x1 x2]
  (let [s1 (NAND x1 x2)
        s2 (OR x1 x2)
        y (AND s1 s2)]
    y))

(defn step_function [x]
  (if (> x 0)
    1
    0))

(defn test_step_function []
  (let [x (map #(+ -5 (* (bigdec 0.1) %1)) (range 100))]
    (map step_function x)))

(defn sigmoid [x]
  (/ 1 (+ 1 (exp (* -1 x)))))

(defn test_sigmoid []
  (let [x (map #(+ -5 (* (bigdec 0.1) %1)) (range 100))]
    (map sigmoid x)))

(defn test_view_sigmoid []
  (view (function-plot sigmoid -5 5)))


(defn -main [] 
  (println (test_view_sigmoid)))
