(ns ezlix.demo.simple
  (:require [helix.core :refer [defnc $ <>]]
            [helix.hooks :as hooks]
            [helix.dom :as d]
            ["react-dom/client" :as rdom]
            [stylefy.core :as stylefy]
            [stylefy.generic-dom :as gdom]
            [ezlix.core :as h :refer-macros [c]]
            [ezlix.state :as s :refer [<< >> sub dbf]]))

(stylefy/init {:dom (gdom/init)})


(defnc app []
  (c {:style {:flex [:column]}}
     (c {:style {:text :xl}}
        (<< :foo))
     (c :button
        {:on-click (fn [_] (>> :foo.inc))}
        "inc foo")
     (c :input#my-input.chouette.pouet
        {:style {:bg {:color "red"}
                 :color "white"
                 :hover {:bg {:color :green}}}

         :value (<< [:get :bar])
         :on-change #(>> [:put :bar (.. % -target -value)])})
     (c (<< [:get :bar])
        (<< :foo))))

(s/register
 {:init (dbf [_ _] {:foo 1 :bar "qux"})
  :foo [(sub [db _] (get db :foo))
        {:inc (dbf [db _] (update db :foo inc))}]})

;; start your app with your favorite React renderer
(defonce root (rdom/createRoot (js/document.getElementById "app")))

(defn ^:dev/after-load render []
  (.render root ($ app)))

(defn ^:export init []
  (>> [:init])
  (render))
