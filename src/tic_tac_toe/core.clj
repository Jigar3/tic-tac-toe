(ns tic-tac-toe.core
  (:gen-class))

(def ^:const player_1 "X")
(def ^:const player_2 "O")
(def ^:const size 3)

(defn init-matrix
  [size]
  (into [] (vec (repeat size (vec (repeat size nil))))))

(defn mark-matrix
  [matrix player x y]
  (if (= nil (nth (nth matrix x) y))
    (assoc matrix x (assoc (nth matrix x) y player))
    nil))

(defn check-horizontal
  [matrix player]
  (let [initial-filter (filter #(and (= (count %) 1)) (map set matrix))]
    (if (= 0 (count initial-filter))
      nil
      (if (= player (get (nth initial-filter 0) player))
        player
        nil))))

(defn check-vertical
  [matrix player]
  (let [inverted-vector (apply mapv vector matrix)]
    (check-horizontal inverted-vector player)))

(defn check-diagonal
  [matrix player]
  (let [initial-filter (set (keep-indexed #(if (= 0 (mod %1 4)) (if (= nil %2)
                                                                  "_"
                                                                  %2)) (flatten matrix)))]
    (if (not (= 1 (count initial-filter)))
      nil
      (if (= player (get initial-filter player))
        player
        nil))))

(defn check-reverse-diagonal
  [matrix player]
  (check-diagonal (reverse matrix) player))

(defn check-matrix
  [matrix player]
  (if (or (check-horizontal matrix player) (check-vertical matrix player)
          (check-diagonal matrix player) (check-reverse-diagonal matrix player))
    player
    nil))

(defn print-matrix
  [matrix]
  (map println matrix))

(defn in-range
  [x]
  (and (> x -1) (< x 3)))

(defn check-filled-matrix
  [matrix]
  (let [result (some nil? (flatten matrix))]
    (if (nil? result)
      true
      false)))

(defn get-coordinates
  [coordinates]
  (let [initial-result (map #(Integer/parseInt %) (clojure.string/split coordinates #" "))]
    (if (and (= 2 (count initial-result))
            (in-range (nth initial-result 0))
            (in-range (nth initial-result 1)))
    initial-result
    nil)
    ))

(defn play
  []
  (loop [game (init-matrix 3), player player_1]
    (let [[x y] (get-coordinates (read-line)),  new-matrix (mark-matrix game player x y)]
      (println (map #(str % "\n") new-matrix))
      (if (= player (check-matrix new-matrix player))
        (println (str "Player " player " won"))
        (if (check-filled-matrix new-matrix)
          (println "Draw")
          (recur new-matrix (if (= player player_1)
                            player_2
                            player_1)))))))

(defn -main
  []
  (play))
        
;; 00 01 02
;; 10 11 12
;; 20 21 22

;; Draw Game
;; 11
;; 12
;; 01
;; 21
;; 02
;; 20
;; 22
;; 00
;; 10

;; Player "O" wins
;; 00
;; 20
;; 11
;; 22
;; 02
;; 21

