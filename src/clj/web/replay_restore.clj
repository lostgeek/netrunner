(ns web.replay-restore
  (:require
   [clojure.data.json :refer [read-json]]
   [game.core.say :refer [system-msg]]
   [game.core.finding :refer [find-card]]
   [game.core.moving :refer [move]]
   [game.replay :as replay]))

(defn replay-deps [game]
  {:app-state (atom {})
   :game-state (atom {})
   :last-state (atom {})
   :replay-status (atom {:autoplay false :speed 1600})
   :replay-timeline (atom [])
   :replay-side (atom :spectator)
   :load-notes (atom nil)
   :get-remote-annotations (atom nil)})

(defn check-for-correct-ids [game replay-state]
  (let [state (:state game)]
    (when (not= (get-in @state [:corp :identity :normalizedtitle])
                (get-in @replay-state [:corp :identity :normalizedtitle]))
      (throw (Exception. "Selected Corp ID does not match replay.")))
    (when (not= (get-in @state [:runner :identity :normalizedtitle])
                (get-in @replay-state [:runner :identity :normalizedtitle]))
      (throw (Exception. "Selected Runner ID does not match replay.")))))

(defn move-all-cards-to-decks [game side]
  (let [state (:state game)]
    (doseq [card (get-in @state [side :hand])]
      (move state :corp card :deck {:suppress-event true :force true}))))

(defn move-cards-to-hand [game replay-state side]
  (let [state (:state game)
        target-cards (get-in @replay-state [side :hand])]
    (doseq [target-card target-cards]
      (let [card (find-card (:title target-card) (get-in @state [side :deck]))]
        (move state side card :hand {:suppress-event true :force true})))))

(defn setup-state-from-replay [game replay-deps]
  (let [replay-state (:game-state replay-deps)]
    (check-for-correct-ids game replay-state)
    (move-all-cards-to-decks game :corp)
    (move-all-cards-to-decks game :runner)
    (move-cards-to-hand game replay-state :corp)))

(defn handle-replay-state
  [game {:keys [replay]} replay-timestamp]
  (when replay
    (let [history (read-json replay true)
          replay-state (replay-deps game)]
      (reset! (:game-state replay-state) (replay/replay-init-state-from-history history (:gameid game)))
      (replay/replay-jump-to! replay-state replay-timestamp)
      (setup-state-from-replay game replay-state)
      (system-msg (:state game) :public "[!] Replay restored")
      game)))
