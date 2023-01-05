(ns advent2022.mission8
  (:require [clojure.java.io :as io]
            [utils.utils :as utils]
            [clojure.string :as string]
            [clojure.set :as set]
            [clojure.walk :as walk])
  (:gen-class))

(defn parse-line
  [line]
  (vec
   (for [d line]
     (utils/as-integer (str d)))))

;; Note original 'any?' in clojure is always 'true' (constant pred.)
(defn any? [pred col] (not (not-any? pred col)))

(let [grid (vec (map parse-line (utils/get-lines "resources/8_input.txt")))
      side (count grid)
      column (fn
               [col]
               (vec (for [r (range side)]
                      ((grid r) col))))
      tree-situation (for [y (range side)
                           x (range side)]
                       {:height ((grid y) x) 
                        :x x
                        :y y
                        :lr (take x (grid y)) ; left to right
                        :rl (reverse (drop (inc x) (grid y))) ; right to left
                        :td (take y (column x)) ; top down 
                        :dt (reverse (drop (inc y) (column x)))

                        ;; Part II
                        :vr (drop (inc x) (grid y)); View Right
                        :vl (reverse (take x  (grid y))) ; View Left
                        :vd (drop (inc y) (column x)); View down
                        :vt (reverse (take y  (column x))) ; View Up (top)
                        })

      tree-check-one-dir (fn[trees
                             tree]
                           (every? #(> tree %) trees))

      tree-check-fn (fn
                      [tree]
                      (let [tree-checks [(:lr tree ) (:rl tree) (:td tree) (:dt tree)]]
                        ;; (every? #(tree-check-one-dir % tree) tree-checks)
                        (assoc tree :visible 
                               (any? #(tree-check-one-dir % (:height tree) ) tree-checks))))]

  ;; (count (filter :visible (map tree-check-fn tree-situation)))
  (map :vt tree-situation))

;; Part1 -> 1776


(comment
  "--- Day 8: Treetop Tree House ---
The expedition comes across a peculiar patch of tall trees all planted
carefully in a grid. The Elves explain that a previous expedition
planted these trees as a reforestation effort. Now, they're curious if
this would be a good location for a tree house.

First, determine whether there is enough tree cover here to keep a
tree house hidden. To do this, you need to count the number of trees
that are visible from outside the grid when looking directly along a
row or column.

The Elves have already launched a quadcopter to generate a map with
the height of each tree (your puzzle input). For example:


30373
25512  (5 5)
65332  (5 3)
33549  (5)
35390

16+5 = 21

Each tree is represented as a single
digit whose value is its height, where 0 is the shortest and 9 is the
tallest.

A tree is visible if all of the other trees between it and an edge of
the grid are shorter than it. Only consider trees in the same row or
column; that is, only look up, down, left, or right from any given
tree.

All of the trees around the edge of the grid are visible - since they
are already on the edge, there are no trees to block the view. In this
example, that only leaves the interior nine trees to consider:

The top-left 5 is visible from the left and top. (It isn't visible
from the right or bottom since other trees of height 5 are in the
way.)  The top-middle 5 is visible from the top and right.  The
top-right 1 is not visible from any direction; for it to be visible,
there would need to only be trees of height 0 between it and an edge.

The left-middle 5 is visible, but only from the right.
The center 3 is not visible from any direction; for it to be visible, there would
need to be only trees of at most height 2 between it and an edge.
The right-middle 3 is visible from the right.

In the bottom row, the
middle 5 is visible, but the 3 and 4 are not.  With 16 trees visible

on the edge and another 5 visible in the interior, a total of 21 trees
are visible in this arrangement.

Consider your map; how many trees are visible from outside the grid?")


