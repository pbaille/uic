(ns ezlix.demo.simple
  (:require [helix.core :refer [defnc $]]
            ["react-dom/client" :as rdom]
            [stylefy.core :as stylefy]
            [stylefy.generic-dom :as gdom]
            [ezlix.component :refer [c c*]]
            [ezlix.state :as s :refer [signal sub dbf effect event]]))

(defnc app []

  (let [[<< >>]
        (s/use-frame :pouetpouet
                     {:foo 1 :bar "qux" :x 1 :y 2}
                     {:foo [(sub [db _] (get db :foo))
                            {:inc (dbf [db _] (update db :foo inc))}]
                      :composite (signal [{x [:get :x] y [:get :y]} _]
                                         (+ x y))
                      :pouet {:pong (event [_ _]
                                           {:pp ["ping"]
                                            :db {:foo 10 :bar "poukav" :x 12 :y 30}})}})]
    (c {:style {:flex [:column]}}
       (c {:style {:text :xl}}
          (<< [:foo]))
       (c :button
          {:on-click (fn [_] (>> [:foo.inc]))}
          "inc foo")
       (c :button
          {:on-click (fn [_] (>> [:pouet.pong]))}
          "pong")
       (c :input#my-input.chouette.pouet
          {:style {:bg {:color "red"}
                   :color "white"
                   :hover {:bg {:color :green}}}

           :value (<< [:get :bar])
           :on-change #(>> [:put :bar (.. % -target -value)])})
       (c {:style {:flex [:row {:gap 2}]}}
          (c (<< [:get :bar]))
          (c (<< [:get :x]))
          (c (<< [:foo]))
          (c (<< [:composite])))
       (c {:style {:flex [:row {:gap 2}]}
           :children [(c (<< [:get :bar]))
                      (c (<< [:get :x]))
                      (c (<< [:foo]))
                      (c (<< [:composite]))]})

       (c* {:style {:flex [:row {:gap 2}]}}
           [(c (<< [:get :bar]))
            (c (<< [:get :x]))
            (c (<< [:foo]))
            (c (<< [:composite]))])

       (c* {:style {:color [:gray {:a 0.1}]}}
           (vec (range 36)))

       (c {:style {:width (str 10 "px")}}
          "io")
       #_(c {:children (vec (range 36))}))))

#_(s/register
 {:init (dbf [_ _] {:foo 1 :bar "qux"})
  :foo [(sub [db _] (get db :foo))
        {:inc (dbf [db _] (update db :foo inc))}]
  :pouet {:pong (event [_ _]
                       (do (println "here"){:pp "ping"}))}
  :subframe (event [(s/sub-frame [:pouet])]
                   [_ [_ e]]
                   {:dispatch [:pong]})})

;; start your app with your favorite React renderer
(defonce root (rdom/createRoot (js/document.getElementById "app")))

(defn ^:dev/after-load render []
  (.render root ($ app)))


(defn ^:export init []
  (stylefy/init {:dom (gdom/init)})
  (render))
