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

(defmulti  pre-process class)
(defmethod pre-process java.lang.Long                [a] (cons a nil))
(defmethod pre-process java.lang.Double              [a] (cons a nil))
(defmethod pre-process clojure.lang.PersistentVector [a] (lazy-seq a))
(defmethod pre-process clatrix.core.Matrix           [a] (lazy-seq a))
(defmethod pre-process :default                      [a] a)

(defmulti  post-process (fn [a y] [(class a) (class y)])
(defmethod post-process [java.lang.Long _]                [a y] y)
(defmethod post-process [java.lang.Double _]              [a y] y)
(defmethod post-process [clojure.lang.PersistentVector _] [a y] y)
(defmethod post-process [clatrix.core.Matrix _]           [a y] y)
(defmethod post-process :default                          [a y] y)

(defn softmax [a']
  (let [a (pre-process a')
        c' (apply max a)
        c (repeat (length a) c')
        exp_a (exp (minus a c))
        sum_exp_a (sum exp_a)
        y' (div exp_a sum_exp_a)
        y (post-process a' y')]
    y)))

(defn softmax-test []
  (let [a (matrix (map #(- (* 1/10 %1) 5) (range 100)))]
    (softmax a)))

 (let [a (matrix (map #(- (* 1/10 %1) 5) (range 100)))]
  (view (time-series-plot a (softmax a))))

(view (function-plot sin -10 10))

(defn -main []
  (println "hello"))
