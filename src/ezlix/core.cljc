(ns ezlix.core
  (:require [helix.core :refer [defnc $ <>]]
            [stylefy.core :as stylefy]
            [ezlix.styles :as styles]
            [ezlix.utils :as u]))

#?(:clj (defmacro c
          "easy component interface on top of helix"
          [tag props & children]
          (let [spread (if-let [styles (:style props)]
                         (macroexpand-1 `(styles/props ~styles ~(:& props {})))
                         props)]
            (u/prob ::c.expansion
             `($ ~(name tag)
                 ~(merge (dissoc props :style :&)
                         (when spread {:& spread}))
                 ~@children)))))


(comment
  (macroexpand-1 '(c :input {:style {:bg {:color "blue"}}
                             :on-click (fn [_] ())}))
  (println "io"))
