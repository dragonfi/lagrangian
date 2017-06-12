(ns lagrangian.core
  (:require [play-clj.core :refer :all]
            [play-clj.ui :refer :all]
            [lagrangian.vec2d :refer [distance add-xy unit-from-angle scalar-mult]]
            [lagrangian.verlet :refer [integrate]]
            [lagrangian.forces :refer [gravity]]))

(def physics-time-step 0.01)
(def player-turn-speed 100)
(def player-acceleration 20)

(def key-left (key-code :a))
(def key-right (key-code :d))
(def key-engine (key-code :space))

(defn player-ship [[x y] [vx vy]]
  (assoc
    (shape :filled :set-color (color :magenta) :circle 0 0 4 :line 0 0 0 10)
    :id :player-ship :body true :x x :y y :vx vx :vy vy :angle 0 :angle-velocity 0 :acceleration 0))
(defn planet [[x y] gm]
  (assoc
    (shape :filled :set-color (color :cyan) :circle 0 0 16)
    :id :planet :x x :y y :body :planet :gm gm))
(defn hud-label [text] (assoc (label text (color :white)) :id :hud))

(defn has-id [id entity] (= id (:id entity)))

(defn get-by-id [id entities]
  (find-first #(has-id id %) entities))

(defn format-player-x [entities]
  (let [player (get-by-id :player-ship entities) planet (get-by-id :planet entities)] 
    (str
      "fps: " (game :fps) "\n"
      "distance:" (distance player planet) "\n"
      "x:" (:x player) "\n"
      "y:" (:y player) "\n"
      "vx:" (:vx player) "\n"
      "vy:" (:vy player) "\n"
      "angle:" (:angle player) "\n"
      "acceleration:" (:acceleration player))))

(defn acceleration-on-player-ship [entities ship]
  (add-xy
    (gravity entities ship)
    (scalar-mult (:acceleration ship) (unit-from-angle (:angle ship)))))
       

(defn update-angle [dt entity]
  (assoc entity :angle (+ (:angle entity) (* dt (:angle-velocity entity)))))

(defn update-entity [entities entity]
  (case (:id entity)
    :player-ship 
    (integrate
      physics-time-step
      (update-angle physics-time-step entity)
      (partial acceleration-on-player-ship entities))
    :hud (hud-label (format-player-x entities))
    entity))

(defn update-label-entity [text id entity]
  (if (has-id id entity) (assoc entity :text text) entity))

(defn update-label [text id entities]
  map update-label-entity entities)
  
(defn update-hud [entities]
  (let [player (get-by-id :player-ship entities)
        status-text (str "x: " (:x player) " y: " (:y player))]
    (update-label status-text :hud entities)))

(defn if-player-set-acceleration [accel entity]
  (if (has-id :player-ship entity)
    (assoc entity :acceleration accel)
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
