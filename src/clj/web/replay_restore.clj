(ns web.replay-restore
  (:require
   [clojure.data.json :refer [read-json]]
   [game.core.card :refer [get-card]]
   [game.core.say :refer [system-msg]]
   [game.core.finding :refer [find-card]]
   [game.core.hosting :refer [host]]
   [game.core.moving :refer [move]]
   [game.core.update :refer [update!]]
   [game.replay :as replay]))

(defn replay-deps [_game]
  {:app-state (atom {})
   :game-state (atom {})
   :last-state (atom {})
   :replay-status (atom {:autoplay false :speed 1600})
   :replay-timeline (atom [])
   :replay-side (atom :spectator)
   :load-notes (fn [] nil)
   :get-remote-annotations (fn [_] nil)})

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
      (move state side card :deck {:suppress-event true :force true}))))

(defn move-cards-to-path [game replay-state side path]
  (let [state (:state game)
        target-cards (get-in @replay-state (cons side path))]
    (doseq [target-card target-cards]
      (when-let [card (find-card (:title target-card) (get-in @state [side :deck]))]
        (move state side card path {:suppress-event true :force true})))))

(def zones {:runner [:hand :deck :discard :scored :rfg :play-area :current]
            :corp [:hand :deck :discard :scored :rfg :play-area :current]})

(defn normalize-all-cards-to-decks [game]
  (doseq [side [:corp :runner]]
    (move-all-cards-to-decks game side)))

(defn restore-flat-zones [game replay-state]
  (doseq [side [:corp :runner]
          zone (side zones)]
    (move-cards-to-path game replay-state side [zone])))

(defn restore-installed-zones
  [game replay-state]
  (doseq [server (keys (get-in @replay-state [:corp :servers]))]
    (move-cards-to-path game replay-state :corp [:servers server :content])
    (move-cards-to-path game replay-state :corp [:servers server :ices]))
  (doseq [rig-zone [:program :hardware :resource :facedown]]
    (move-cards-to-path game replay-state :runner [:rig rig-zone])))

(defn apply-visible-card-state [state side live-card replay-card]
  (let [updated (cond-> live-card
                  (contains? replay-card :rezzed) (assoc :rezzed (:rezzed replay-card))
                  (not (contains? replay-card :rezzed)) (dissoc :rezzed)
                  (contains? replay-card :facedown) (assoc :facedown (:facedown replay-card))
                  (not (contains? replay-card :facedown)) (dissoc :facedown)
                  (contains? replay-card :counter) (assoc :counter (:counter replay-card))
                  (not (contains? replay-card :counter)) (dissoc :counter)
                  (contains? replay-card :advance-counter) (assoc :advance-counter (:advance-counter replay-card))
                  (not (contains? replay-card :advance-counter)) (dissoc :advance-counter))]
    (update! state side updated)))

(defn restore-hosted-tree [state side live-host replay-host]
  (doseq [replay-child (:hosted replay-host)]
    (when-let [live-child (find-card (:title replay-child) (get-in @state [side :deck]))]
      (let [hosted-card (host state side (get-card state live-host) live-child {:facedown (:facedown replay-child)})]
        (when hosted-card
          (apply-visible-card-state state side hosted-card replay-child)
          (restore-hosted-tree state side hosted-card replay-child))))))

(defn restore-card-and-hosted [state side path replay-card]
  (when-let [live-card (find-card (:title replay-card) (get-in @state (cons side path)))]
    (apply-visible-card-state state side live-card replay-card)
    (restore-hosted-tree state side live-card replay-card)))

(defn replay-installed-paths [replay-state side]
  (if (= side :corp)
    (mapcat (fn [server] [[:servers server :content] [:servers server :ices]])
            (keys (get-in @replay-state [:corp :servers])))
    [[:rig :program] [:rig :hardware] [:rig :resource] [:rig :facedown]]))

(defn restore-hosted-cards
  [game replay-state]
  (let [state (:state game)]
    (doseq [side [:corp :runner]
            path (replay-installed-paths replay-state side)
            replay-card (get-in @replay-state (cons side path))]
      (restore-card-and-hosted state side path replay-card))))

(defn restore-player-state
  [_game _replay-state])

(defn setup-state-from-replay [game replay-deps]
  (let [replay-state (:game-state replay-deps)]
    (check-for-correct-ids game replay-state)
    (normalize-all-cards-to-decks game)
    (restore-flat-zones game replay-state)
    (restore-installed-zones game replay-state)
    (restore-hosted-cards game replay-state)
    (restore-player-state game replay-state)))

(defn handle-replay-state
  [game {:keys [replay]} replay-timestamp]
  (when replay
    (let [history (read-json replay true)
          replay-state (replay-deps game)]
      (reset! (:game-state replay-state) (replay/replay-init-state-from-history history (:gameid game)))
      (replay/populate-replay-timeline! replay-state @(:game-state replay-state))
      (replay/replay-jump-to! replay-state replay-timestamp)
      (setup-state-from-replay game replay-state)
      (system-msg (:state game) :public "[!] Replay restored")
      game)))
