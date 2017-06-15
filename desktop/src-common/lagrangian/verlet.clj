(ns lagrangian.verlet
  (:require [lagrangian.vector :refer [add scale]]))

(defn- body? [element]
  (and (:position element) (:velocity element)))

(defn- integrate-v [dt a new-a v]
  (+ v (* 0.5 (+ a new-a) dt)))

(defn- integrate-position [dt a element]
  "first step of integration (r + v dt + 0.5 a dt dt)"
  (let
    [ r (:position element)
      v (:velocity element)]
    (assoc element :position
      (add r (scale dt v) (scale (* 0.5 dt dt) a)))))

(defn- integrate-velocity [dt old-a calc-a element]
  "second step of integration (v + 0.5 (a + new_a) dt), must call integrate-position first"
  (let
    [ v (:velocity element)
      new-a (calc-a element)]
    (assoc element :velocity
      (add v (scale (* 0.5 dt) (add old-a new-a))))))

(defn integrate [dt calc-a element] 
  "one step of verlet integration, uses :body :x :y :vx :vy of element"
  (if (body? element)
    (let [a (calc-a element)]
      (integrate-velocity dt a calc-a (integrate-position dt a element)))
    element))
