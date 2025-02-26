(ns uic.state
  (:require [clojure.string :as str]
            [uix.core]
            #?@(:cljs [[refx.alpha :as rf]
                       [cljs.pprint :as pp]]))
  #?(:cljs (:require-macros [uic.state :refer [sub dbf event effect]])))

;; I will try to implement a nice way to declare re-frame subs/events/fxs

;; re-frame reserved keys

:rf-sub
:rf-upd
:rf-event
:rf-effect

(defrecord RFHandler
           [name type doc interceptors handler])

(do :help

    (defn path->keyword [path]
      (keyword (str/join "." (map name path))))

    (defn all-paths [x]
      (cond
        (instance? RFHandler x) (list (list x))
        (map? x) (mapcat (fn [[k v]] (map #(cons k %) (all-paths v))) x)
        (vector? x) (mapcat all-paths x)
        :else (list (list x)))))

#?(:clj (do

          (defn parse-rf-handler

            [[type fst & nxt]]

            (let [[name fst & nxt]
                  (if (symbol? fst)
                    (cons fst nxt)
                    (concat [nil fst] nxt))

                  [doc & nxt]
                  (if (string? fst)
                    (cons fst nxt)
                    (concat ["" fst] nxt))

                  [interceptors argv return]
                  (case (count nxt)
                    2 (cons [] nxt)
                    3 nxt)]

              {:type type
               :doc doc
               :interceptors interceptors
               :handler `(fn ~@(when name [name]) ~argv ~return)}))

          (defmacro rfn [& xs]
            (list `map->RFHandler (parse-rf-handler xs)))

          (defmacro sub [& xs]
            `(rfn :rf-sub ~@xs))

          (defmacro signal
            [[deps event-pattern] & body]
            `(map->RFHandler {:type :rf-sub
                              :handler (fn [~(vec (keys deps)) ~event-pattern] ~@body)
                              :inputs ~(vec (vals deps))}))

          (defmacro dbf [& xs]
            `(rfn :rf-dbf ~@xs))

          (defmacro event [& xs]
            `(rfn :rf-event ~@xs))

          (defmacro effect [& xs]
            `(rfn :rf-effect ~@xs))))

#?(:cljs (do :registration

             (def default-tree
               {:get (uic.state/sub [db [_ x & xs]]
                                      (cond (not x) db
                                            xs (get-in db (cons x xs))
                                            (keyword? x) (get db x)
                                            (sequential? x) (get-in db x)))

                :put (uic.state/dbf self [db [_ p v & pvs]]
                                      (let [db (cond (sequential? p) (if (seq p) (assoc-in db p v) v)
                                                     (keyword? p) (assoc db p v))]
                                        (if pvs
                                          (self db (into [nil] pvs))
                                          db)))

                :upd (uic.state/dbf self [db [_ p f & pfs]]
                                      (let [db (cond (sequential? p) (if (seq p) (update-in db p f) (f db))
                                                     (keyword? p) (update db p f))]
                                        (if pfs
                                          (self db (into [nil] pfs))
                                          db)))

                :do (uic.state/event [_ [_ & xs]] {:dispatch-n xs})

                :fx (uic.state/event [_ [_ fxs]] fxs)

                :pp [(uic.state/event [_ [_ & xs]] {:pp xs})
                     (uic.state/effect [_ xs] (doseq [x xs] (pp/pprint x)))]})

             (defn register [frame tree]
               (doseq [p (all-paths tree)]
                 (let [raw-handler? (fn? (last p))
                       [path {:keys [interceptors handler type inputs]}]
                       (if raw-handler?
                         [(drop-last 2 p) {:type (last (butlast p)) :handler (last p)}]
                         [(butlast p) (last p)])
                       key (path->keyword path)]
                   (case type
                     :rf-dbf (rf/reg-event-db frame key interceptors handler)
                     :rf-event (rf/reg-event-fx frame key interceptors handler)
                     :rf-effect (rf/reg-fx frame key (partial handler frame))
                     :rf-sub (if-not inputs
                               (rf/reg-sub frame key handler)
                               (rf/reg-sub frame
                                           key
                                           (fn [_ _] (mapv (partial rf/sub frame) inputs))
                                           handler))))))))

#?(:cljs (do :aliases

             (defn >> [frame event] (rf/dispatch frame event))))

;; prelude
#?(:cljs (do :init-and-hook

             (defn init-frame
               [{:keys [tree init db]}]
               (let [frame (rf/new-frame)]
                 (register frame (merge default-tree tree))
                 (rf/dispatch-sync frame [:fx {:db db}])
                 (when init (rf/dispatch-sync frame init))
                 [(rf/subscription-hook frame)
                  (partial rf/dispatch frame)]))

             (defn use-frame* [id tree db & [init]]
               (let [[subscribe dispatch]
                     (uix.core/use-memo
                      (fn [] (let [frame (rf/new-frame)]
                               (println "frame memo exec")
                               (register frame (merge default-tree tree))
                               (rf/dispatch-sync frame [:fx {:db db}])
                               (when init (rf/dispatch-sync frame init))
                               [(rf/subscription-hook frame)
                                (partial rf/dispatch frame)]))
                      [id db tree init])]
                 (println "use-frame* ret " [subscribe dispatch])
                 [subscribe dispatch]))))
