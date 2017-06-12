(ns lagrangian.verlet)

(defn- body? [element]
  (every? element [:position :velocity]))

(defn- map-position-to-xy [body]
  (let [r (body :position)]
    (assoc body :x (r 0) :y (r 1))))

(defn- integrate-x [dt a v x]
  (+ x (* v dt) (* 0.5 a dt dt)))

(defn- integrate-v [dt a new-a v]
  (+ v (* 0.5 (+ a new-a) dt)))

(defn- integrate-position [dt a element]
  "first step of integration"
  (assoc element
    :x (integrate-x dt (:x a) (:vx element) (:x element))
    :y (integrate-x dt (:y a) (:vy element) (:y element))))

(defn- integrate-velocity [dt old-a calc-a element]
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
