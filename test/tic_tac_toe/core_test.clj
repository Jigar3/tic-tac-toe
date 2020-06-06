(ns tic-tac-toe.core-test
  (:require [clojure.test :refer :all]
            [tic-tac-toe.core :refer :all]))

(deftest a-test
  (testing "FIXME, I fail."
    (is (= 0 0))))

(def all-nil-matrix  [[nil nil nil] [nil nil nil] [nil nil nil]])
(def matrix-with-horizontal-pattern [["X" "X" "X"] ["O" "X" "O"] ["O" "O" "X"]])
(def matrix-with-vertical-pattern [["X" "O" "O"] ["X" "X" "O"] ["X" "O" "X"]])
(def matrix-with-diagonal-pattern [["X" "O" "O"] ["O" "X" "X"] ["X" "O" "X"]])
(def matrix-with-reverse-diagonal-pattern [["O" "X" "X"] ["O" "X" "O"] ["X" "O" "O"]])
(def matrix-with-no-pattern [["X" "O" "X"] ["O" "X" "X"] ["O" "X" "O"]])
(def matrix-with-no-pattern-with-nil [["X" nil nil] [nil nil nil] [nil nil nil]])

(deftest init-matrix-test
  (testing "Initializing an empty matrix"
    (is (= all-nil-matrix (init-matrix 3)))))

(deftest mark-matrix-test
  (testing "Marking a matrix")
  (let [matrix (init-matrix size) updated-matrix (mark-matrix matrix player_1 0 2)]
    (is (= [[nil nil "X"] [nil nil nil] [nil nil nil]] updated-matrix)))
  (let [matrix [[nil "O" "X"] [nil nil nil] [nil nil nil]]
        updated-matrix (mark-matrix matrix player_1 0 1)]
    (is (= nil updated-matrix))))

(deftest check-horizontal-test
  (testing "Checking to find a winner in horizontal pattern")
  (is (= player_1 (check-horizontal matrix-with-horizontal-pattern player_1)))
  (is (= nil (check-horizontal matrix-with-no-pattern player_1)))
  (is (= nil (check-horizontal matrix-with-no-pattern-with-nil player_1))))

(deftest check-vertical-test
  (testing "Checking to find a winner in vertical pattern")
  (is (= player_1 (check-vertical matrix-with-vertical-pattern player_1)))
  (is (= nil (check-vertical matrix-with-no-pattern player_1)))
  (is (= nil (check-vertical matrix-with-no-pattern-with-nil player_1))))

(deftest check-diagonal-test
  (testing "Checking to find a winner in diagonal pattern")
  (is (= player_1 (check-diagonal matrix-with-diagonal-pattern player_1)))
  (is (= nil (check-diagonal matrix-with-no-pattern player_1)))
  (is (= nil (check-diagonal matrix-with-no-pattern-with-nil player_1))))

(deftest check-reverse-diagonal-test
  (testing "Checking to find a winner in reverse-diagonal pattern")
  (testing "Checking to find a winner in reverse diagonal pattern")
  (is (= player_1 (check-reverse-diagonal matrix-with-reverse-diagonal-pattern player_1)))
  (is (= nil (check-reverse-diagonal matrix-with-no-pattern player_1)))
  (is (= nil (check-reverse-diagonal matrix-with-no-pattern-with-nil player_1))))

(deftest check-matrix-test
  (testing "Checking to find a winner in the matrix")
  (is (= player_1 (check-matrix matrix-with-horizontal-pattern player_1)))
  (is (= player_1 (check-matrix matrix-with-vertical-pattern player_1)))
  (is (= player_1 (check-matrix matrix-with-diagonal-pattern player_1)))
  (is (= player_1 (check-matrix matrix-with-reverse-diagonal-pattern player_1)))
  (is (= nil (check-matrix matrix-with-no-pattern player_1)))
  (is (= nil (check-matrix matrix-with-no-pattern-with-nil player_1))))

(deftest in-range-test
  (testing "Checking if input in range")
  (is (= true (in-range 2)))
  (is (= false (in-range 3)))
  (is (= true (in-range 0)))
  (is (= false (in-range -1))))

(deftest check-get-coordinates
  (testing "To get the coordinates entered by user")
  (is (= [1 2] (get-coordinates "1 2")))
  (is (= [0 1] (get-coordinates "0 1")))
  (is (= nil (get-coordinates "0 3"))))

(deftest check-filled-matrix-test
  (testing "To see if the game got finished")
  (is (= false (check-filled-matrix matrix-with-no-pattern-with-nil)))
  (is (= true (check-filled-matrix matrix-with-horizontal-pattern))))

(deftest generate-possible-cells-test
  (testing "To generate all possible cells")
  (is (= [[0 0] [0 1] [0 2] [1 0] [1 1] [1 2] [2 0] [2 1] [2 2]] (generate-possible-cells 3)))
  (is (= [[0 0] [0 1] [1 0] [1 1]] (generate-possible-cells 2))))

(deftest remove-from-vector-test
  (testing "A vector gets removed from a vector of vectors")
  (is (= [] (remove-from-vector [[1 2]] [1 2])))
  (is (= [[0 1]] (remove-from-vector [[1 2] [0 1]] [1 2]))))
