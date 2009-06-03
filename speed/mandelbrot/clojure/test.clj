(def runs 1)
(def max_iterations 99888)
(defn iter [ci cr]
  (let [max_iter (int max_iterations)
        ci (double ci)
        cr (double cr)]
    (loop [zi (double ci)   zr (double cr)   i (int 1)]
      (if (<= i max_iter)
        (let [zr2 (* zr zr)   zi2 (* zi zi)]
          (if (<= (+ zr2 zi2) (double 4.0))
            (recur (+ (* (* zr zi) (double 2.0)) ci)
                   (+ (- zr2 zi2) cr)
                   (inc i))
                  i))
        0))))
(defn mand [n]
  (doseq [y (range -39 39)]
    (when (= 1 n) (print "\n"))
    (doseq [x (range -39 39)]
      (let [i (iter (/ x 40.0) (- (/ y 40.0) 0.5))]
        (when (= 1 n) (print (if (= 0 i) "*" " ")))))))
(defn time-mand []
  (time
   (doseq [i (range 1 (inc runs))]
     (mand i))))
(time-mand)
