(ns ezlix.demo.simple
  (:require [uix.core :refer [defui $]]
            [uix.dom]
            [stylefy.core :as stylefy]
            [stylefy.generic-dom :as gdom]
            [ezlix.component :refer [c]]
            [ezlix.state :as s :refer [signal sub dbf effect event]]
            ))

(def tree
  {:init (dbf [_ _]
              {:name "foo" :x 1 :y 2})
   :name (sub [db _] (get db :name))
   :xy (signal [{x [:get :x] y [:get :y]} _]
               (+ x y))
   :reset (event [_ _]
                 {:pp ["reset"]
                  :db {:name "nono" :x 12 :y 30}})})

(defui app []

  (let [[<< >>]
        (s/use-frame* :pouetpouet
                      tree
                      {:name "pouet"}
                      [:init])]
    (c {:style {:flex [:column]}}
       (c {:style {:text :xl}}
          (<< [:name]))
       (c :button
          {:on-click (fn [_] (println "inc x") (>> [:put [:x] 34]))}
          "inc x")
       (c :button
          {:on-click (fn [_] (>> [:reset]))}
          "reset")
       (c :input#my-input.chouette.pouet
          {:style {:bg {:color "red"}
                   :color "white"
                   :hover {:bg {:color :green}}}

           :default-value (<< [:get :name])
           :on-change #(>> [:put :name (.. % -target -value)])})
       (c {:style {:flex [:row {:gap 2}]}}
          (c (<< [:get :x]))
          (c (<< [:get :y]))
          (c (<< [:xy])))
       (c {:style {:color [:gray {:a 0.1}]}}
          (vec (range 36)))

       (c {:style {:width (str 10 "px")}}
          "io")
       #_(c {:children (vec (range 36))}))))

;; start your app with your favorite React renderer
(defonce root (uix.dom/create-root (js/document.getElementById "app")))

(defn ^:dev/after-load render []
  (uix.dom/render-root ($ app) root))


(defn ^:export init []
  (stylefy/init {:dom (gdom/init)})
  (render))
