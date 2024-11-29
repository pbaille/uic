(ns uic.demo.core
  (:require [uix.core :refer [defui $]]
            [uix.dom]
            [stylefy.generic-dom :as gdom]
            [stylefy.core :as stylefy]
            [uic.demo.article :as article]
            ["highlight.js/lib/core" :as hljs]
            ["highlight.js/lib/languages/clojure" :as clojure]))

(defonce root (uix.dom/create-root (js/document.getElementById "app")))

(defn ^:dev/after-load render []
  (println "reload")
  (uix.dom/render-root ($ article/component) root)
  (hljs/highlightAll))

(defn ^:export init []
  (hljs/registerLanguage "clojure" clojure)
  (stylefy/init {:dom (gdom/init)})
  (render))