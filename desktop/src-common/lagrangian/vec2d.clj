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

(defn integrate [dt element calc-a] 
  (if (:body element)
    (let [a (calc-a element)
          new-position (assoc element
                         :x (integrate-x dt (:x a) (:vx element) (:x element))
                         :y (integrate-x dt (:y a) (:vy element) (:y element)))
          new-a (calc-a new-position)]
      (assoc new-position
        :vx (integrate-v dt (:x a) (:x new-a) (:vx element))
        :vy (integrate-v dt (:y a) (:y new-a) (:vy element))))
    element))

(defn planet? [entity] (= :planet (:body entity)))

(defn planet-gravity [planet location]
  (let [scalar-part (/ (:gm planet) (distance planet location))]
    {:x (* scalar-part (- (:x planet) (:x location)))
     :y (* scalar-part (- (:y planet) (:y location)))}))

(defn entity-gravity [entity location]
  (if (planet? entity)
    (planet-gravity entity location)
    {:x 0.0 :y 0.0}))

(defn gravity [entities location]
  (reduce add-xy (map #(entity-gravity % location) entities)))

