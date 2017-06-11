(ns lagrangian.vec2d)

(defn seq-abs [seq] (Math/sqrt (apply + (map #(* % %) seq))))

(defn seq-distance [seq1 seq2] (seq-abs (map - seq1 seq2)))

(defn distance [{ax :x ay :y} {bx :x by :y}] (seq-distance [ax ay] [bx by]))

(defn add-xy [{ax :x ay :y} {by :x bx :y}]
  {:x (+ ax by) :y (+ ay by)})


(defn integrate-x [dt a v x]
  (+ x (* v dt) (* 0.5 a dt dt)))

(defn integrate-v [dt a new-a v]
  (+ v (* 0.5 (+ a new-a) dt)))

(defn integrate-position [dt a element]
  "first step of integration"
  (assoc element
    :x (integrate-x dt (:x a) (:vx element) (:x element))
    :y (integrate-x dt (:y a) (:vy element) (:y element))))

(defn integrate-velocity [dt old-a calc-a element]
  "second step of integration, must call integrate-position first"
  (let [new-a (calc-a element)]
    (assoc element
      :vx (integrate-v dt (:x old-a) (:x new-a) (:vx element))
      :vy (integrate-v dt (:y old-a) (:y new-a) (:vy element)))))

(defn integrate [dt element calc-a] 
  "one step of verlet integration, uses :body :x :y :vx :vy of element"
  (if (:body element)
    (let [a (calc-a element)]
      (integrate-velocity dt a calc-a (integrate-position dt a element)))
    element))


(defn planet? [entity] (= :planet (:body entity)))

(defn planet-gravity [planet location]
  "acceleration = - GM * r_unit / distance / distance
   acceleration = - GM * r_12 / distance / distance / distance"
  (let [
        d (distance planet location)
        scalar-part (/ (:gm planet) (* d d d))]
    {:x (* scalar-part (- (:x planet) (:x location)))
     :y (* scalar-part (- (:y planet) (:y location)))}))

(defn entity-gravity [entity location]
  (if (planet? entity)
    (planet-gravity entity location)
    {:x 0.0 :y 0.0}))

(defn gravity [entities location]
  (reduce add-xy (map #(entity-gravity % location) entities)))

