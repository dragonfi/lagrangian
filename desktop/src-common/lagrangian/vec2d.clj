(ns lagrangian.vec2d)

(defn seq-abs [seq] (Math/sqrt (apply + (map #(* % %) seq))))

(defn seq-distance [seq1 seq2] (seq-abs (map - seq1 seq2)))

(defn distance [{ax :x ay :y} {bx :x by :y}] (seq-distance [ax ay] [bx by]))

(defn add-xy [{ax :x ay :y} {by :x bx :y}]
  {:x (+ ax by) :y (+ ay by)})
