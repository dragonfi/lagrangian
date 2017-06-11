(ns lagrangian.core
  (:require [play-clj.core :refer :all]
            [play-clj.ui :refer :all]
            [lagrangian.vec2d :refer :all]))

(def physics-time-step 0.01)

(defn player-ship [x y]
  (assoc (image "ship.png") :id :player-ship :body true :x x :y y :vx 10.0 :vy 0.0 :origin-x 8 :origin-y 8))
(defn planet [x y]
  (assoc (image "planet.png") :id :planet :width 64 :height 64 :origin-x 32 :origin-y 32 :x x :y y :body :planet :gm 10.0))
(defn hud-label [text] (assoc (label text (color :white)) :id :hud))

(defn has-id [id entity] (= id (:id entity)))

(defn get-by-id [id entities]
  (find-first #(has-id id %) entities))

(defn format-player-x [entities]
  (let [player (get-by-id :player-ship entities) planet (get-by-id :planet entities)] 
    (str "fps: " (game :fps) "\ndistance:" (distance player planet))))

(defn update-entity [entities entity]
  (case (:id entity)
    :player-ship (integrate physics-time-step entity (partial gravity entities))
    :hud (hud-label (format-player-x entities))
    entity))

(defn update-label-entity [text id entity]
  (println id (:id entity))
  (if (has-id id entity) (assoc entity :text text) entity))

(defn update-label [text id entities]
  map update-label-entity entities)
  
(defn update-hud [entities]
  (let [player (get-by-id :player-ship entities)
        status-text (str "x: " (:x player) " y: " (:y player))]
    (update-label status-text :hud entities)))

(defscreen main-screen
  :on-show
  (fn [screen entities]
    (add-timer! screen :update-game-state physics-time-step physics-time-step)
    (update! screen :renderer (stage))
    [ 
     (planet 200.0 200.0)
     (player-ship 180.0 180.0)
     (hud-label "hello world")])

  :on-render
  (fn [screen entities]
    (clear!)
    (render! screen entities))

  :on-timer
  (fn [screen entities]
    (case (:id screen)
      :update-game-state (map #(update-entity entities %) entities)
      entities)))

(defgame lagrangian-game
  :on-create
  (fn [this]
    (set-screen! this main-screen)))
