(ns uic.demo.article
  (:require
   [uix.core :as uix]
   [uic.component :refer [c sc]]
   [uic.styles.mixins :as s]
   [uic.styles.colors :as c]
   [uic.demo.utils :refer-macros [card card* expr code]]))

(defn icon-button
  [class & [styles attrs]]
  (c {:style {:color :midgrey0
              :text :md
              :p 0.5
              :hover {:color :tomato}
              :& [styles]}
      :key class
      :& attrs}
     (c :i
        {:class (str "fa fa-" (name class))}
        "pouet")))

(defn section
  [{:keys [styles title content]}]
  (let [[open set-open] (uix/use-state true)
        if-open (fn [a b] (if open a b))
        border {:width 3 :color :lightgrey0}]
    (sc :.section
        {:m {:top 2}
         :border {:top border :left border}
         :& styles}
        (c :.title
           {:style {:flex [:inline :center {:items :baseline}]
                    :hover {:.collapse-button {:display :inline}}}
            :on-click #(set-open not)}
           (name title)
           (icon-button (if-open :minus-square :plus-square)
                        {:color :lightgrey2}))
        (sc :.content
            {:flex [:nowrap :column {:gap 2}]
             :p {:left 2}
             :display (if-open :flex :none)}
            content))))

(letfn [(builder [size]
          (fn [title & content]
            (section {:title title :content content
                      :styles {:.title {:text [size :bold] :p 2}}})))]
  (def S1 (builder :3xl))
  (def S2 (builder :2xl))
  (def S3 (builder :xl))
  (def S4 (builder :lg)))

(defn show-size [size]
  (c :.container
     {:style {:bg {:color :light-grey}
              :rounded 1
              :size 100
              :p 1}}
     (c {:style {:size size
                 :flex :center
                 :bg {:color :salmon}
                 :color :white
                 :rounded 1}})))

(uix/defui component []

  (c {:style {:m 3 :p 3
              :text :lg
              :color :dark-slategray}}

     (S1 :uic

         "uic is a small library that lets you create and compose simple components."

         "We can create a component like this:"

         (card* "Hello")

         "It can take styles:"

         (card* {:style {:color :gray}} "Hello")

         "Attributes:"

         (card* {:style {:color :gray}
                 :on-click (fn [_] (js/alert "hello!"))}
                "Click me")

         "Pretty normal so far, but how about this ?"

         (card* :button
                {:style {:rounded 2 :p 1
                         :color :white
                         :bg {:color :primary}
                         :hover {:bg {:color :tomato}}}}
                "Hover Me")

         "uic uses the stylefy library under the hood, and is letting you use pseudo classes and subselectors."

         (card* {:style {:color :grey
                         :.sub {:color :tomato
                                :text :bold
                                :hover {:bg {:color :gold}}}}}
                "I can inject styles to my sub components!"
                (mapv (fn [t] (c :div.sub t)) (range 3)))

         "When dealing with component that only have a single :style prop, we can use the `sc` form (styled-component)"
         (card (sc {:flex :center
                    :text [:xl :bold :italic]}
                   "Hello styled component"))

         (S2 :Styles

             (str "You may have noticed that styles we are using in our components are not regular css ones, some are nested, some do not exists. "
                  "Under the hood uic uses a collection of clojure functions to produce styles, along with stylefy that do the heavy lifting.")

             "Those functions are locatted in the `uic.styles.mixins` namespace, in this section we will introduce some of them."

             (S3 "Size"
                 "Integers are interpreted as pixels."
                 (expr (s/size 50 50))
                 "Floats are interpreted as percentages."
                 (expr (s/size 0.5 0.75))
                 #_(show-size [50 50])
                 "Arity one is handy if x size and y size are equals."
                 (expr (s/size 50))
                 "Any hiccup recognized value is valid."
                 (expr (s/size :3rem))
                 "There is some predefined aliases."
                 (expr (s/size :full :half))
                 (expr (s/size :third))
                 "You can specify min and max values to."
                 (expr (s/size {:min 100 :max :half} {:min 100 :val :full}))
                 "You can set width and height independently too."
                 (expr (s/width :full))
                 (expr (s/height "2rem"))
                 (expr (s/width {:min 50 :max :half}))

                 (S4 "Demo"
                     (card (show-size [:full :half]))
                     (card (show-size [:half :full]))
                     (card (show-size :half))
                     (card (show-size :full))
                     (card (show-size [:double (/ 2 3)]))))

             (S3 "Spaces"
                 "Paddings, margins and gaps"
                 (code (defn pad-square [pad]
                         (sc {:p pad :size 100 :bg {:color :light-grey} :rounded 1}
                             (sc {:size :full :bg {:color :salmon} :rounded 1})))
                       (defn flex-row [& xs]
                         (sc {:flex [:row {:gap 1}]}
                             xs)))
                 (card (flex-row (pad-square 1)
                                 (pad-square 2)
                                 (pad-square 4)))
                 (code (defn square [margin pad]
                         (sc {:p margin :size 100 :bg {:color :salmon} :rounded 1}
                             (sc {:size :full :p pad :bg {:color :light-grey} :rounded 1}
                                 (sc {:size :full :bg {:color :light-skyblue} :rounded 1})))))
                 (card (flex-row (square 1 2)
                                 (square 2 1)
                                 (square 4 0))))

             (S3 "Text"
                 (card (sc (s/flex :column {:gap 1})
                           (mapv (fn [size]
                                   (c {:key size :style [(s/text size) (s/flex :row :nogrow {:gap 2})]}
                                      (sc :span [(s/width 100) (s/bg-color :light-grey) (s/text :center)] (name size))
                                      (sc :span (s/margin {:left 3}) (s/font-sizes size))))
                                 s/sorted-font-sizes)))
                 (expr (mapv s/text s/sorted-font-sizes)))

             (S3 :Colors
                 (expr (s/color :tomato-light))
                 (expr (s/color :light-skyblue))
                 "CSS colors are available:"
                 (card (sc {:flex [:wrap]}
                           (map (fn [name] (c {:style {:p 2 :display :inline-block
                                                       :flex-basis "2.15%" :bg {:color name}}
                                               :on-click (fn [] (js/alert name))}))
                                uic.styles.colors/color-presets)))
                 "The awesome `thi.ng.color` library is let you manipulate colors in many ways."
                 (card (sc {:flex [:row :nowrap]}
                           (sc {:p 2 :bg {:color :blue}})
                           (sc {:p 2 :bg {:color (c/mix :red :blue 0.5)}})
                           (sc {:p 2 :bg {:color :red}})))
                 (card (sc {:flex [:center :wrap]}
                           (map (fn [col] (sc {:p 2 :display :inline-block
                                               :flex-basis "10%" :bg {:color col}}))
                                (uic.styles.colors/shades :gray 30)))))))))
