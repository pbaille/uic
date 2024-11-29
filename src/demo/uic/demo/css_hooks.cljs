(ns uic.demo.css-hooks
  (:require [uix.core :refer [defui $]]
            [uix.dom]
            [stylefy.core :as stylefy]
            [stylefy.generic-dom :as gdom]
            [uic.component :refer [c]]
            [uic.state :as s :refer [signal sub dbf effect event]]
            [uic.css :as css]))

(defui app []
  ($ :div
     {:style (css/css {:bg {:color :blue}
                       :hover {:bg {:color :green}}})}
     ($ :div.child
        "hello")
     "world"))

;; start your app with your favorite React renderer
(def root (uix.dom/create-root (js/document.getElementById "app")))

(defn ^:dev/after-load render []
  (uix.dom/render-root (css/wrap-css css/DEFAULT_HOOKS ($ app))
                       root))


(defn ^:export init []
  (stylefy/init {:dom (gdom/init)})
  (render))

(comment
  (init))
