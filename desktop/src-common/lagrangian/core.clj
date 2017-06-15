(ns lagrangian.core
  (:require [play-clj.core :refer :all]
            [play-clj.ui :refer :all]
            [lagrangian.vector :refer [distance add scale unit-from-angle]]
            [lagrangian.verlet :refer [integrate]]
            [lagrangian.forces :refer [gravity]]))

(def physics-time-step 0.01)
(def player-turn-speed 100)
(def player-acceleration 20)

(def key-left (key-code :a))
(def key-right (key-code :d))
(def key-engine (key-code :space))

(defn position-to-xy [{[x y] :position :as element}]
  (assoc element :x x :y y))

(defn player-ship [[x y] [vx vy]]
  (assoc
    (shape :filled :set-color (color :magenta) :circle 0 0 4 :line 0 0 0 10)
    :id :player-ship
    :body true :x x :y y :position [x y] :velocity [vx vy]
    :angle 0 :angle-velocity 0 :self-acceleration 0))

(defn planet [[x y] gm]
  (assoc
    (shape :filled :set-color (color :cyan) :circle 0 0 16)
    :id :planet
    :body :planet :x x :y y :position [x y] :velocity [0 0] :gm gm))

(defn hud-label [text] (assoc (label text (color :white)) :id :hud))

(defn has-id [id entity] (= id (:id entity)))

(defn get-by-id [id entities]
  (find-first #(has-id id %) entities))

(defn format-player-x [entities]
  (let [player (get-by-id :player-ship entities) planet (get-by-id :planet entities)] 
    (str
      "fps: " (game :fps) "\n"
      "distance:" (distance (:position player) (:position planet)) "\n"
      "position:" (:position player) "\n"
      "velocity:" (:velocity player) "\n"
      "angle:" (:angle player) "\n"
      "acceleration:" (:self-acceleration player))))

(defn acceleration-on-player-ship [entities ship]
  (add
    (gravity entities ship)
    (scale (:self-acceleration ship) (unit-from-angle (:angle ship)))))


(defn update-angle [dt entity]
  (assoc entity :angle (+ (:angle entity) (* dt (:angle-velocity entity)))))

(defn update-entity [entities entity]
  (case (:id entity)
    :player-ship
    (let
      [calc-a (partial acceleration-on-player-ship entities)
       dt physics-time-step]
      (position-to-xy (integrate dt calc-a (update-angle dt entity))))
    :planet
    (position-to-xy entity)
    :hud (hud-label (format-player-x entities))
      entity))

(defn if-player-set-acceleration [accel entity]
  (if (has-id :player-ship entity)
    (assoc entity :self-acceleration accel)
    entity))

(defn if-player-set-angle-speed [speed entity]
  (if (has-id :player-ship entity)
    (assoc entity :angle-velocity speed)
    entity))

(defscreen main-screen
  :on-show
  (fn [screen entities]
    (add-timer! screen :update-game-state physics-time-step physics-time-step)
    (update! screen :renderer (stage))
    [
     (planet [400.0 300.0] 100000.0)
     (planet [200.0 300.0] 100000.0)
     (player-ship [400.0 200.0] [20.0 0.0])
     (hud-label "hello world")])

  :on-render
  (fn [screen entities]
    (clear!)
    (render! screen entities))

  :on-timer
  (fn [screen entities]
    (case (:id screen)
      :update-game-state (map #(update-entity entities %) entities)
      entities))

  :on-key-down
  (fn [screen entities]
    (cond
      (= (:key screen) key-left)
      (map (partial if-player-set-angle-speed player-turn-speed) entities)
      (= (:key screen) key-right)
      (map (partial if-player-set-angle-speed (- player-turn-speed)) entities)
      (= (:key screen) key-engine)
      (map (partial if-player-set-acceleration player-acceleration) entities)
      true
      entities))
  
  :on-key-up
  (fn [screen entities]
    (cond
      (= (:key screen) key-left)
      (map (partial if-player-set-angle-speed 0) entities)
      (= (:key screen) key-right)
      (map (partial if-player-set-angle-speed 0) entities)
      (= (:key screen) key-engine)
      (map (partial if-player-set-acceleration 0) entities)
      true
      entities)))

(defgame lagrangian-game
  :on-create
  (fn [this]
    (set-screen! this main-screen)))
