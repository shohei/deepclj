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


(defn relu [x]
  (max 0 x))


(defn identity_function [x]
  x)

(defn sigmoid-matrix [A]
  (trans (matrix (map sigmoid A))))

(defn test-nn [] 
  (let [X (trans (matrix [1.0 0.5]))
        W1 (matrix [[0.1 0.3 0.5] [0.2 0.4 0.6]])
        B1 (trans (matrix [0.1 0.2 0.3]))
        A1 (plus (mmult X W1) B1)
        Z1 (sigmoid-matrix A1)

        W2 (matrix [[0.1 0.4] [0.2 0.5] [0.3 0.6]])
        B2 (trans (matrix [0.1 0.2]))
        A2 (plus (mmult Z1 W2) B2)
        Z2 (sigmoid-matrix A2)

        W3 (matrix [[0.1 0.3] [0.2 0.4]])
        B3 (trans (matrix [0.1 0.2]))
        A3 (plus (mmult Z2 W3) B3)
        Y (identity_function A3)]
    Y))

(defn -main [] 
  (println (test_view_sigmoid)))
