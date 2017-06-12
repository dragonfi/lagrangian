(ns lagrangian.vec2d)

(defn seq-abs [seq] (Math/sqrt (apply + (map #(* % %) seq))))

(defn seq-distance [seq1 seq2] (seq-abs (map - seq1 seq2)))

(defn distance [{ax :x ay :y} {bx :x by :y}] (seq-distance [ax ay] [bx by]))

(defn scalar-mult [scalar {x :x y :y}] {:x (* scalar x) :y (* scalar y)})

(defn deg-to-rad [deg]
  (* deg (/ Math/PI 180)))

(defn unit-from-angle [angle]
  (let [rad (deg-to-rad angle)]
    {:x (- (Math/sin rad)) :y (Math/cos rad)}))

(defn add-xy [{ax :x ay :y} {bx :x by :y}]
  {:x (+ ax bx) :y (+ ay by)})
