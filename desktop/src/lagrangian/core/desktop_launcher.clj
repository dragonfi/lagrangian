(ns lagrangian.core.desktop-launcher
  (:require [lagrangian.core :refer :all])
  (:import [com.badlogic.gdx.backends.lwjgl LwjglApplication]
           [org.lwjgl.input Keyboard])
  (:gen-class))

(defn -main
  []
  (LwjglApplication. lagrangian-game "lagrangian" 800 600)
  (Keyboard/enableRepeatEvents true))
