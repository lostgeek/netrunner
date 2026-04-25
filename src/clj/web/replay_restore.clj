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

(defn move-cards-to-path
  ([game replay-state side path]
   (move-cards-to-path game replay-state side path nil))
  ([game replay-state side path cid-map]
   (let [state (:state game)
         target-cards (get-in @replay-state (cons side path))]
     (doseq [target-card target-cards]
       (when-let [card (find-card (:title target-card) (get-in @state [side :deck]))]
         (let [moved-card (move state side card path {:suppress-event true :force true})]
           (when (and cid-map (:cid target-card) moved-card)
            (swap! cid-map assoc (:cid target-card) moved-card))))))))

(def zones {:runner [:hand :deck :discard :scored :rfg :play-area :current]
            :corp [:hand :deck :discard :scored :rfg :play-area :current]})

(defn replay-card-side [card]
  (let [side (:side card)]
    (cond
      (keyword? side) side
      (string? side) (keyword (.toLowerCase side))
      :else nil)))

(defn normalize-all-cards-to-decks [game]
  (doseq [side [:corp :runner]]
    (move-all-cards-to-decks game side)))

(defn restore-flat-zones [game replay-state cid-map]
  (doseq [side [:corp :runner]
          zone (side zones)]
    (move-cards-to-path game replay-state side [zone] cid-map)))

(defn restore-installed-zones
  [game replay-state cid-map]
  (doseq [server (keys (get-in @replay-state [:corp :servers]))]
    (move-cards-to-path game replay-state :corp [:servers server :content] cid-map)
    (move-cards-to-path game replay-state :corp [:servers server :ices] cid-map))
  (doseq [rig-zone [:program :hardware :resource :facedown]]
    (move-cards-to-path game replay-state :runner [:rig rig-zone] cid-map)))

(defn apply-visible-card-state [state side live-card replay-card]
  (when live-card
    (let [updated (cond-> live-card
                    (contains? replay-card :rezzed) (assoc :rezzed (:rezzed replay-card))
                    (not (contains? replay-card :rezzed)) (dissoc :rezzed)
                    (contains? replay-card :facedown) (assoc :facedown (:facedown replay-card))
                    (not (contains? replay-card :facedown)) (dissoc :facedown)
                    (contains? replay-card :counter) (assoc :counter (:counter replay-card))
                    (not (contains? replay-card :counter)) (dissoc :counter)
                    (contains? replay-card :advance-counter) (assoc :advance-counter (:advance-counter replay-card))
                    (not (contains? replay-card :advance-counter)) (dissoc :advance-counter))]
      (update! state side updated))))

(defn restore-hosted-tree
  ([state side live-host replay-host]
   (restore-hosted-tree state side live-host replay-host nil))
  ([state side live-host replay-host cid-map]
   (doseq [replay-child (:hosted replay-host)]
     (let [child-side (or (replay-card-side replay-child) side)]
       (when-let [live-child (find-card (:title replay-child) (get-in @state [child-side :deck]))]
         (when-let [live-host-card (get-card state live-host)]
           (let [hosted-card (host state child-side live-host-card live-child {:facedown (:facedown replay-child)})]
             (when hosted-card
               (when (and cid-map (:cid replay-child))
                 (swap! cid-map assoc (:cid replay-child) hosted-card))
               (apply-visible-card-state state child-side hosted-card replay-child)
               (restore-hosted-tree state child-side hosted-card replay-child cid-map)))))))))

(defn replay-card->live-card [state replay-card cid-map]
  (let [live-ref (get @cid-map (:cid replay-card))
        live-card (get-card state live-ref)]
    (when live-card
      {:side (replay-card-side live-card)
       :card live-card})))

(defn restore-card-and-hosted [state replay-card cid-map]
  (when-let [{:keys [side card]} (replay-card->live-card state replay-card cid-map)]
    (when card
      (apply-visible-card-state state side card replay-card)
      (restore-hosted-tree state side card replay-card cid-map))))

(defn replay-installed-paths [replay-state side]
  (if (= side :corp)
    (mapcat (fn [server] [[:servers server :content] [:servers server :ices]])
            (keys (get-in @replay-state [:corp :servers])))
    [[:rig :program] [:rig :hardware] [:rig :resource] [:rig :facedown]]))

(defn restore-hosted-cards
  [game replay-state cid-map]
  (let [state (:state game)]
    (doseq [side [:corp :runner]
            path (replay-installed-paths replay-state side)
            replay-card (get-in @replay-state (cons side path))]
      (restore-card-and-hosted state replay-card cid-map))))

(defn restore-player-state
  [_game _replay-state])

(defn setup-state-from-replay [game replay-deps]
  (let [replay-state (:game-state replay-deps)
        cid-map (atom {})]
    (check-for-correct-ids game replay-state)
    (normalize-all-cards-to-decks game)
    (restore-flat-zones game replay-state cid-map)
    (restore-installed-zones game replay-state cid-map)
    (restore-hosted-cards game replay-state cid-map)
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
