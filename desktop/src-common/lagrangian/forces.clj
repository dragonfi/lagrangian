(ns lagrangian.forces
  (:require [lagrangian.vector :refer [add sub scale distance]]))

(defn- planet? [entity] (= :planet (:body entity)))

(defn- planet-gravity [planet location]
  "acceleration = - GM * r_unit / distance / distance
   acceleration = - GM * r_12 / distance / distance / distance"
  (let
    [
      d (distance (:position planet) (:position location))
      scalar-part (/ (:gm planet) (* d d d))
      vector-part (sub (:position planet) (:position location))]
    (scale scalar-part vector-part)))

(defn- entity-gravity [entity location]
  (if (planet? entity)
    (planet-gravity entity location)
    [0.0 0.0]))

(defn gravity [entities location]
  (apply add (map #(entity-gravity % location) entities)))
