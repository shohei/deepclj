(ns deepclj.core
  (:gen-class)
  (:use incanter.core))

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

(defn -main [] 
  (println (AND 1 1))
  (println (NAND 1 0))
  (println (OR 1 0)))
