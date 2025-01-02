(ns uic.demo.simple
  (:require [uix.core :refer [defui $]]
            [uix.dom]
            [stylefy.core :as stylefy]
            [stylefy.generic-dom :as gdom]
            [uic.component :refer [sc c]]
            [uic.state :as s :refer [signal sub dbf effect event]]))

(def tree
  {:init (dbf [_ _]
              {:name "foo" :x 1 :y 2})
   :name (sub [db _] (get db :name))
   :xy (signal [{x [:get :x] y [:get :y]} _]
               (+ x y))
   :reset (event [_ _]
                 {:pp ["reset"]
                  :db {:name "nono" :x 12 :y 30}})})

(let [[subscribe dispatch]
      (s/init-frame {:id :pouetpouet
                     :tree tree
                     :db {:name "pouet"}
                     :init [:init]})]
  (def >> dispatch)
  (def << subscribe))

(defui app []

  (sc {:flex [:column]}
      (sc {:text :xl}
          (<< [:name]))
      (c :button
         {:on-click (fn [_] (>> [:upd :x inc]))}
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
      (sc {:flex [:row {:gap 2}]}
          (c (<< [:get :x]))
          (c (<< [:get :y]))
          (c (<< [:xy])))))

(defui app-hooked []

  (let [[<< >>]
        (s/use-frame* :pouetpouet
                      tree
                      {:name "pouet"}
                      [:init])]
    (sc {:flex [:column]}
        (sc {:text :xl}
            (<< [:name]))
        (c :button
           {:on-click (fn [_] (>> [:upd :x inc]))}
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
        (sc {:flex [:row {:gap 2}]}
            (c (<< [:get :x]))
            (c (<< [:get :y]))
            (c (<< [:xy]))))))



;; start your app with your favorite React renderer
(comment
  (defonce root (uix.dom/create-root (js/document.getElementById "app")))

  (defn ^:dev/after-load render []
    (uix.dom/render-root ($ app) root))


  (defn ^:export init []
    (stylefy/init {:dom (gdom/init)})
    (render)))
