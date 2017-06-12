(ns lagrangian.forces
  (:require [lagrangian.vec2d :refer [distance add-xy]]))

(defn- planet? [entity] (= :planet (:body entity)))

(defn- planet-gravity [planet location]
  "acceleration = - GM * r_unit / distance / distance
   acceleration = - GM * r_12 / distance / distance / distance"
  (let [
        d (distance planet location)
        scalar-part (/ (:gm planet) (* d d d))]
    {:x (* scalar-part (- (:x planet) (:x location)))
     :y (* scalar-part (- (:y planet) (:y location)))}))

(defn- entity-gravity [entity location]
  (if (planet? entity)
    (planet-gravity entity location)
    {:x 0.0 :y 0.0}))

(defn gravity [entities location]
  (reduce add-xy (map #(entity-gravity % location) entities)))
