(ns nr.angelarena
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [clojure.string :refer [lower-case capitalize]]
            [jinteki.utils :refer [superuser?]]
            [nr.ajax :refer [GET]]
            [nr.appstate :refer [app-state]]
            [nr.cardbrowser :refer [image-url]]
            [nr.deckbuilder :refer [deck-entry deck-name deck-date]]
            [nr.game-row :refer [game-row]]
            [nr.translations :refer [tr tr-side tr-format]]
            [nr.utils :refer [slug->format cond-button tristate-button]]
            [nr.ws :as ws]
            [reagent.core :as r]
            [reagent-modals.modals :as reagent-modals]))

(defonce arena-supported-formats [:standard :startup])

(defonce runs (r/atom nil))
(defonce chosen-format (r/atom (first arena-supported-formats)))

(defn- fetch-runs []
  (go (let [{:keys [status json]} (<! (GET "/profile/angelarena/runs"))]
        (when (= 200 status)
          (reset! runs (js->clj json))))))

(defn- time-delta-string [delta]                          ;XXX: switch to clj-time
  (let [days (Math/floor (/ delta (* 1000 60 60 24)))
        delta (mod delta (* 1000 60 60 24))
        hours (Math/floor (/ delta (* 1000 60 60)))
        delta (mod delta (* 1000 60 60))
        minutes (Math/floor (/ delta (* 1000 60)))
        delta (mod delta (* 1000 60))
        seconds (Math/floor (/ delta 1000))]
    (cond
      (pos? days) (str days " days, " hours " hours")
      (pos? hours) (str hours " hours, " minutes " minutes")
      :else (str minutes " minutes, " seconds " seconds"))))

(defn- deck-view [side s decks]
  (r/with-let [deck (first (filter #(= (str (:_id %))
                                       (get-in @runs [@chosen-format side :deck-id]))
                                   @decks))
               run-info (get-in @runs [@chosen-format side])
               time-since-start (- (js/Date.now) (js/Date.parse (:run-started run-info)))
               allowed-days (+ 3 (:wins run-info) (:losses run-info))]
    [:div.deck
     [:img {:src (image-url (:identity deck))
            :alt (get-in deck [:identity :title] "")}]
     [:h4 (deck-name deck)]
     [:div.result.float-right (str (:wins run-info) " wins")]
     [:div (get-in deck [:identity :title])]
     [:div.result.float-right (str (:losses run-info) " losses")]
     [:div.time (str "Time left: " (time-delta-string (- (* 1000 60 60 24 allowed-days)
                                                         time-since-start)))]]))

(defn- deck-button-bar [side s]
  (r/with-let [abandon (r/atom false)]
    [:div.button-bar
     [tristate-button
      (tr [:angelarena.queueing "Queueing..."])
      (tr [:angelarena.queue-for-match "Queue for match"])
      (= side (:queueing @s))
      (and (:queueing @s)
           (not= side (:queueing @s)))
      #(if (:queueing @s)
         (do (ws/ws-send! [:angelarena/dequeue {:side side}])
             (swap! s dissoc :queueing))
         (do (ws/ws-send! [:angelarena/queue {:side side}])
             (swap! s assoc :queueing side)))]
     (if @abandon
       [:span "Are you sure? "
        [:button.small {:on-click #(do (ws/ws-send! [:angelarena/abandon-run {:deck-id (get-in @runs [@chosen-format side :deck-id])}])
                                       (fetch-runs))} "yes"]
        [:button.small {:on-click #(reset! abandon false)} "no"]]
       [:button {:on-click #(reset! abandon true)} "Abandon run"])]))

(defn deckselect-modal [user {:keys [side gameid games decks format]}]
  [:div
   [:h3 (tr [:lobby.select-title "Select your deck"])]
   [:div.deck-collection.lobby-deck-selector
    (let [same-side? (fn [deck] (= (capitalize (name side))
                                   (get-in deck [:identity :side])))]
      [:div
       (doall
         (for [deck (->> @decks
                         (filter same-side?)
                         (sort-by :date >))]
           ^{:key (:_id deck)}
           [:div.deckline {:on-click #(do (ws/ws-send! [:angelarena/start-run
                                                        {:deck-id (:_id deck)}])
                                          (reagent-modals/close-modal!)
                                          (fetch-runs))}
            [:img {:src (image-url (:identity deck))
                   :alt (get-in deck [:identity :title] "")}]
            [:h4 (:name deck)]
            [:div.float-right (-> (:date deck) js/Date. js/moment (.format "MMM Do YYYY"))]
            [:p (get-in deck [:identity :title])]]))])]])

(defn- new-run-button-bar [side decks s games gameid sets user]
  [:div.button-bar
   [cond-button (tr [:angelarena.start-new-run "Start new run"])
    (not (:queueing @s))
    #(reagent-modals/modal!
       [deckselect-modal user {:games games :gameid gameid
                               :sets sets :decks decks
                               :side side :format nil}
        runs])]])

(defn game-panel [decks s games gameid sets user]
  (if-not @runs
    (do
      (fetch-runs)
      [:div.game-panel.angelarena
       [:h3 "Requesting run data..."]])
    [:div.game-panel.angelarena
     [:h3 "Format"]
     [:div.format-bar
      (doall
        (for [form arena-supported-formats]
          ^{:key form}
          [:span.tab {:on-click #(reset! chosen-format form)
                      :class [(when (= @chosen-format form) "current")]}
           (get slug->format (name form))]))]
     [:h3 "Current corp run"]
     (if (get-in @runs [@chosen-format :corp])
       [:div
        [deck-view :corp s decks]
        [deck-button-bar :corp s gameid games user]]
       [new-run-button-bar :corp decks s games gameid sets user])

     [:h3 "Current runner run"]
     (if (get-in @runs [@chosen-format :runner])
       [:div
        [deck-view :runner s decks]
        [deck-button-bar :runner s gameid games user]]
       [new-run-button-bar :runner decks s games gameid sets user])

     [:h3 "Latest runs"]]))

(defn- blocked-from-game
  "Remove games for which the user is blocked by one of the players"
  [user game]
  (or (superuser? user)
      (let [players (get game :players [])
            blocked-users (flatten (map #(get-in % [:user :options :blocked-users] []) players))]
        (= -1 (.indexOf blocked-users (:username user))))))

(defn- blocking-from-game
  "Remove games with players we are blocking"
  [blocked-users game]
  (let [players (get game :players [])
        player-names (map #(get-in % [:user :username] "") players)
        intersect (clojure.set/intersection (set blocked-users) (set player-names))]
    (empty? intersect)))

(defn filter-blocked-games
  [user games]
  (if (= "tournament" (:room (first games)))
    games
    (let [blocked-games (filter #(blocked-from-game user %) games)
          blocked-users (get-in user [:options :blocked-users] [])]
      (filter #(blocking-from-game blocked-users %) blocked-games))))

(defn game-list [user {:keys [room games gameid password-game editing]}]
  (let [roomgames (r/track (fn [] (filter #(= (:room %) room) @games)))
        filtered-games (r/track #(filter-blocked-games @user @roomgames))]
    [:div.game-list
     (if (empty? @filtered-games)
       [:h4 (tr [:lobby.no-games "No games"])]
       (doall
         (for [game @filtered-games]
           ^{:key (:gameid game)}
           [game-row (assoc game :current-game @gameid :password-game password-game :editing editing)])))]))


(defn- start-game
  [s side]
  (swap! s assoc :editing false)
  (swap! app-state dissoc :editing-game)
  (ws/ws-send! [:lobby/create
                {:title "Angel Arena Match"
                 :password ""
                 :allow-spectator true
                 :save-replay true
                 :spectatorhands false
                 :side side
                 :format "standard" ; XXX: Make flexible
                 :room "angelarena"
                 :timer nil
                 :api-access true
                 :replay false
                 :prompt false
                 :editing false
                 :protected false
                 :create-game-deck nil
                 :timed false}]))
