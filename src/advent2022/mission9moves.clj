(ns advent2022.mission9moves
  (:require [clojure.java.io :as io]
            [utils.utils :as utils]
            [clojure.string :as string]
            [clojure.set :as set]
            [clojure.walk :as walk])
  (:gen-class))

H..
..T

(def move-info 
  [{:org [0 -2] ; org is from H (centre 0 0) to T
    :vec [0 1] ; vector needed to move 'T' inorder to follow 'H'
    :comment "
..T..
.....
..H..
.....
....."}
   {:org [1 -2]
    :vec [-1 1]
    :comment "
...T.
.....
..H..
.....
....."}
   {:org [-2 -2] ; org is from H (centre 0 0) to T
    :vec [-1 1]
    :comment "
....T
.....
..H..
.....
....."}
   {:org [2 -1] ; org is from H (centre 0 0) to T
    :vec [-1 1] ; vector needed to move 'T' inorder to follow 'H'
    :comment "
.....
....T
..H..
.....
....."}
   {:org [2 0] ; org is from H (centre 0 0) to T
    :vec [-1 0] ; vector needed to move 'T' inorder to follow 'H'
    :comment "
.....
.....
..H.T
.....
....."}
   {:org [2 1] ; org is from H (centre 0 0) to T
    :vec [-1 -1] ; vector needed to move 'T' inorder to follow 'H'
    :comment "
.....
.....
..H..
....T
....."}
   {:org [2 2] ; org is from H (centre 0 0) to T
    :vec [-1 -1] ; vector needed to move 'T' inorder to follow 'H'
    :comment "
.....
.....
..H..
.....
....T"}
   {:org [1 2] ; org is from H (centre 0 0) to T
    :vec [-1 -1] ; vector needed to move 'T' inorder to follow 'H'
    :comment "
.....
.....
..H..
.....
...T."}
   {:org [0 2] ; org is from H (centre 0 0) to T
    :vec [0 -1] ; vector needed to move 'T' inorder to follow 'H'
    :comment "
.....
.....
..H..
.....
..T.."}
   {:org [-1 2] ; org is from H (centre 0 0) to T
    :vec [1 -1] 
    :comment "
.....
..H..
.....
.T..."}
   {:org [-2 2] ; org is from H (centre 0 0) to T
    :vec [1 -1] ; vector needed to move 'T' inorder to follow 'H'
    :comment "
.....
.....
..H..
.....
T...."}

   {:org [-2 1] ; org is from H (centre 0 0) to T
    :vec [1 -1] ; vector needed to move 'T' inorder to follow 'H'
    :comment "
.....
.....
..H..
T....
....."}
   {:org [-2 0] ; org is from H (centre 0 0) to T
    :vec [1 0] ; vector needed to move 'T' inorder to follow 'H'
    :comment "
.....
.....
T.H..
.....
....."}
   {:org [-2 -1] ; org is from H (centre 0 0) to T
    :vec [1 1] ; vector needed to move 'T' inorder to follow 'H'
    :comment "
.....
T....
..H..
.....
....."}
   {:org [-2 -2] ; org is from H (centre 0 0) to T
    :vec [1 1] ; vector needed to move 'T' inorder to follow 'H'
    :comment "
T....
.....
..H..
.....
....."}
   {:org [-1 -2] ; org is from H (centre 0 0) to T
    :vec [1 1] ; vector needed to move 'T' inorder to follow 'H'
    :comment "
.T...
.....
..H..
.....
....."}])



