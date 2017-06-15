(ns lagrangian.vector)

(defn abs [seq] (Math/sqrt (apply + (map #(* % %) seq))))

(defn sub [seq1 & seqs] (vec (apply (partial map - seq1) seqs)))

(defn add [seq1 & seqs] (vec (apply (partial map + seq1) seqs)))

(defn distance [seq1 seq2] (abs (sub seq1 seq2)))

(defn scale [scalar seq] (map #(* scalar %) seq))

(defn deg-to-rad [deg] (* deg (/ Math/PI 180)))

(defn unit-from-angle [angle]
  (let [rad (deg-to-rad angle)]
    [(- (Math/sin rad)) (Math/cos rad)]))

