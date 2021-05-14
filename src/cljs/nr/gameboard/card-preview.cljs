(ns nr.gameboard.card-preview
  (:require [cljs.core.async :refer [put!]]
            [nr.appstate :refer [app-state]]))

(defn- get-card-data-title [e]
  (let [target (.. e -target)
        title (.getAttribute target "data-card-title")]
    (not-empty title)))

(defn card-preview-mouse-over
  [e channel]
  (.preventDefault e)
  (when-let [title (get-card-data-title e)]
    (when-let [card (get (:all-cards-and-flips @app-state) title)]
      (put! channel card)))
  nil)

(defn card-preview-mouse-out [e channel]
  (.preventDefault e)
  (when (get-card-data-title e)
    (put! channel false))
  nil)

(defn card-highlight-mouse-over [e value channel]
  (.preventDefault e)
  (when (:cid value)
    (put! channel (select-keys value [:cid])))
  nil)

(defn card-highlight-mouse-out [e value channel]
  (.preventDefault e)
  (when (:cid value)
    (put! channel false))
  nil)

