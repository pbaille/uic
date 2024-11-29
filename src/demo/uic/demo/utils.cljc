(ns uic.demo.utils
  (:require [uic.component :refer [c]]
            #?(:cljs [cljs.pprint :as pp])
            #?@(:clj [[zprint.core :as zp]
                      [clojure.string :as str]
                      [clojure.pprint :as pp]
                      [uic.utils :as u]])))

(defn pretty-str [x]
  (with-out-str (binding [pp/*print-right-margin* 50]
                  (pp/pprint x))))

(def code-block-ss
  {:p 1
   :border [2 :#f7f7f7]})

(defn code-block [x & xs]
  (c :pre
     {:style [code-block-ss {:bg {:color "#fafafa"}}]}
     (c :code.language-clojure
        x xs)))

(defn code-output [x]
  (c {:style code-block-ss} x))

(defn repl-output [x]
  (c :pre.repl-output
     {:style code-block-ss}
     (c :code.language-clojure
        x)))

#?(:clj
   (defmacro card [& content]
     (do u/prob :card
             `(c :.code-card
                 (code-block ~(str/join "\n" (mapv zp/zprint-str content)))
                 (code-output (list ~@content))))))

#?(:clj
   (defmacro card* [& xs]

     (do u/prob :card*
             `(c :.code-card
                 (code-block ~(zp/zprint-str (cons 'c xs)))
                 (code-output (c ~@xs))))))

#?(:clj
   (defmacro expr [x]
     (do u/prob :expr
             `(c :.code-card
                 (code-block ~(zp/zprint-str x))
                 (repl-output (pretty-str ~x))))))

#?(:clj
   (defmacro code [& xs]
     (do u/prob :code
             `(c :.code-card
                 (code-block ~@(interpose "\n" (map zp/zprint-str xs)))
                 (and ~@xs (repl-output ":ok"))))))
