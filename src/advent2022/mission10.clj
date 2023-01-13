(ns advent2022.mission10
  (:require [clojure.java.io :as io]
            [utils.utils :as utils]
            [clojure.string :as string]
            [clojure.set :as set]
            [clojure.walk :as walk])
  (:gen-class))


(let [ops (let [parse-line (fn[line]
                             (let [[_ op v] (re-matches #"(.*) (-?\d*)" line)]
                               (cond v { :addx (utils/as-integer v) :op-cycles 2 }
                                     :else nil)))]
            (vec (map parse-line
                      (utils/get-lines "resources/input_10_full.txt"))))

      ;;testing
      ;;      ops [nil
      ;;           nil
      ;;           {:addx 23 :op-cycles 2 }]

      initial-cpu {:x 1
                   :cycle 1
                   :ops ops }

      noop-fn (fn [cpu]
                ;;(println (:cycle cpu) "noop")
                (-> cpu
                    (update :ops #(vec (rest %)))))

      addx-fn (fn
                [cpu]
                (let [op (first (:ops cpu))
                      v (:addx op)]
                  (if (= (dec (:op-cycles op)) 0)
                    ;; operation takes effect
                    (do
                      (-> cpu
                          (update :ops #(vec (rest %)))
                          (update :x
                                  (fn[x]
                                    ;;(println (:cycle cpu)"add"(:addx op) "to" x)
                                    (+ (:addx op) x))

                                  )))
                    ;;
                    (do
                      ;;(println (:cycle cpu)"add...")
                      (-> cpu
                          (update-in [:ops 0 :op-cycles] dec))))))

      do-cycle (fn do-cycle
                 [cpu]
                 (let [cpu-fn (fn[cpu]
                                (let [ops (:ops cpu)
                                      op (first ops)]
                                  (-> (cond
                                        (nil? op) (noop-fn cpu)
                                        (:addx op) (addx-fn cpu)
                                        :else (-> cpu ; noop (or no more instructions)
                                                  (update :ops #(vec (rest %)))))
                                      (update :cycle inc))))] 

                   ;;(println "do-cycle" cpu)
                   (lazy-seq (cons cpu (do-cycle (cpu-fn cpu))))))

      signal-strength (fn[cpu] (* (:x cpu)
                                  (:cycle cpu)))

      prep-data (fn[cpu]
                  (-> cpu
                      (select-keys [:cycle :x])
                      (assoc :ss (signal-strength cpu))))

      ;; 12470
      part1-solution (->> (do-cycle initial-cpu)
                          (drop (dec 20))
                          (take-nth 40)
                          (take 6) ; take the firt six cycle values: 20, 60, 100 140 180 220
                          (map prep-data)
                          (map :ss)
                          (apply +))

      crt {:width 40
           :height 6}; You count the pixels on the CRT: 40 wide and 6 high


      draw-data1 (for [y (range (:height crt))
                       x (range (:width crt))]
                   (char (+ (int \a)  x)))

      draw-lcd-fn (fn[crt
                      data] (doall (map println  
                                        (map #(apply str %) (partition (:width crt) data)))))

      scanline (fn scanline
                 ([x] (scanline 0 x))
                 ([scan-x x]
                  (let [sprite (set (map #(+ scan-x %) [-1 0 1]))
                        pxl (if (not (nil? (sprite x))) \# \.)]
                    (lazy-seq (cons pxl (scanline (inc scan-x) x))))))

      pixel-fn (fn[inp]
                 (let [[scanline x] inp
                       sprite (set (map #(+ scanline %) [-1 0 1]))] 
                   (if (not (nil? (sprite x)))
                     \#
                     \.)))

      ]

  (let [data-size (* (:height crt) (:width crt))
        xvals (vec (take data-size (map :x (do-cycle initial-cpu))))

        data (for [y (range (:height crt))
                   x (range (:width crt))]
               (pixel-fn
                [x
                 (xvals
                  (+ (* y (:width crt)) x))]))
        ]
    (draw-lcd-fn crt data))
  ;;  (draw-lcd-fn crt (take (* 40 6) (scanline 5)))

  )

"RBPARAGF"

(comment "
###..###..###...##..###...##...##..####.
#..#.#..#.#..#.#..#.#..#.#..#.#..#.#....
#..#.###..#..#.#..#.#..#.#..#.#....###..
###..#..#.###..####.###..####.#.##.#....
#.#..#..#.#....#..#.#.#..#..#.#..#.#....
#..#.###..#....#..#.#..#.#..#..###.#....
")
