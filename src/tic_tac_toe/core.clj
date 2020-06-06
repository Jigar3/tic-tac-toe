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
  (let [initial-filter (set (keep-indexed #(if (= 0 (mod %1 (inc size))) (if (= nil %2)
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
  (and (> x -1) (< x size)))

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
      nil)))

(defn generate-possible-cells
  [size]
  (for [a (range size) b (range size)] [a b]))

(defn remove-from-vector
  [vector-list to-remove]
  (filter #(not= to-remove %) vector-list))

(defn get-random-from-vector
  [vector-list]
  (nth vector-list (rand-int (count vector-list))))

(defn play-move
  [matrix player]
  (println (map #(str % "\n") matrix))
  (if (= player (check-matrix matrix player))
    :player-won
    (if (check-filled-matrix matrix)
      :draw
      :continue)))

(defn play-computer
  []
  (loop [game (init-matrix 3), filled-vector (generate-possible-cells 3)]
    (let [[x y] (get-coordinates (read-line))
          new-matrix (mark-matrix game player_1 x y)
          new-vector (remove-from-vector filled-vector [x y])]
      (case (play-move new-matrix player_1)
        :player-won (println (str "Player " player_1 " won"))
        :draw (println "Draw")
        :continue (let [[new_x new_y] (get-random-from-vector new-vector)
                        newer-matrix (mark-matrix new-matrix player_2 new_x new_y)
                        new-filled-vector (remove-from-vector new-vector [new_x new_y])]
                    (case (play-move newer-matrix player_2)
                      :player-won (println (str "Player " player_2 " won"))
                      :draw (println "Draw")
                      :continue (recur newer-matrix new-filled-vector)))))))

(defn play-human
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
  (play-computer))