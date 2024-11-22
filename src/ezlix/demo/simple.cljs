(ns ezlix.demo.simple
  (:require [helix.core :refer [defnc $ <>]]
            [helix.hooks :as hooks]
            [helix.dom :as d]
            ["react-dom/client" :as rdom]
            [stylefy.core :as stylefy]
            [stylefy.generic-dom :as gdom]
            [ezlix.core :as h :refer-macros [c]]))

(stylefy/init {:dom (gdom/init)})


(defnc greeting
  "A component which greets a user."
  [{:keys [name]}]
  ;; use helix.dom to create DOM elements
  (d/div "Hello, " (d/strong name) "!"))

(defnc app []
  (let [[state set-state] (hooks/use-state {:name "Helix User"})]
    (d/div
     (d/h1 "Welcomeu!")
      ;; create elements out of components
     ($ greeting {:name (:name state)})
     (c :input#my-input.chouette.pouet
        {:style {:bg {:color "red"}
                 :color "white"
                 :hover {:bg {:color :green}}}

         :value (:name state)
         :on-change #(set-state assoc :name (.. % -target -value))}))))

;; start your app with your favorite React renderer
(defonce root (rdom/createRoot (js/document.getElementById "app")))

(defn ^:dev/after-load render []
  (.render root ($ app)))

(defn ^:export init []
  (render))
