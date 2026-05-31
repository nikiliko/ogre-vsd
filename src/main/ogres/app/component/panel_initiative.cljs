(ns ogres.app.component.panel-initiative
  (:require [clojure.string :refer [join capitalize blank?]]
            [ogres.app.component :refer [icon image]]
            [ogres.app.hooks :as hooks]
            [uix.core :as uix :refer [defui $]]))

;; ── Against the Darkmaster — Tactical Round Sequence phases ─────────────────
(def ^:private phase-names
  ["Assessment" "Declaration" "Move Phase"
   "Spell Phase A" "Range Phase A" "Melee Phase"
   "Range Phase B" "Spell Phase B" "Other Actions"])

(def ^:private action-options
  ["Move" "Melee Attack" "Range Attack"
   "Spell: Touch" "Spell: Range" "Spell: Space" "Wait"])

;; tokens with these declared actions are highlighted in the corresponding phase
(def ^:private phase-active-actions
  {2 #{"Move"}
   3 #{"Spell: Touch" "Spell: Range" "Spell: Space"}
   4 #{"Range Attack"}
   5 #{"Melee Attack"}
   8 #{"Wait"}})

(def ^:private query
  [:user/host
   {:user/camera
    [{:camera/scene
      [:db/id
       :initiative/rounds
       :initiative/phase
       :initiative/turn
       :initiative/played
       {:scene/initiative
        [:db/id
         :object/hidden
         :token/label
         :token/flags
         :initiative/suffix
         :initiative/health
         :initiative/declared-action
         :initiative/defence
         :camera/_selected
         {:token/image
          [:token-image/url
           :image/public
           {:image/thumbnail
            [:image/hash]}]}]}]}]}])

(def ^:private query-actions
  [{:user/camera
    [{:camera/scene
      [{:scene/initiative
        [:db/id :token/flags]}
       [:initiative/rounds :default 0]
       [:initiative/phase  :default 0]
       :initiative/played]}]}])

(defn ^:private token-order
  "Stable insertion order — no initiative rolls in AtD."
  [a b]
  (compare (:db/id a) (:db/id b)))

;; ── Declared-action selector ─────────────────────────────────────────────────

(defui ^:private form-action
  [{:keys [value on-change phase]}]
  (let [[editing set-editing form] (hooks/use-modal)
        editable (= phase 1)]
    ($ :.initiative-token-action
      {:data-present (some? value)
       :data-value   (or value "")}
      ($ :button.initiative-token-action-label
        {:disabled (not editable)
         :title    (if editable "Click to set declared action" "")
         :on-click
         (fn [event]
           (.stopPropagation event)
           (when editable (set-editing not)))}
        (or value "—"))
      (if editing
        ($ :.initiative-token-action-menu
          {:ref form}
          (for [action action-options]
            ($ :button
              {:key        action
               :type       "button"
               :data-active (= action value)
               :on-click
               (fn []
                 (on-change action)
                 (set-editing false))}
              action)))))))

;; ── Defence tracker ──────────────────────────────────────────────────────────

(defui ^:private form-defence
  [{:keys [value on-change]}]
  (let [[editing set-editing form] (hooks/use-modal)
        input (uix/use-ref)]
    ($ :.initiative-token-defence
      {:data-present (some? value)}
      ($ :.initiative-token-defence-frame
        ($ icon {:name "fist" :size 40}))
      ($ :button.initiative-token-defence-label
        {:on-click (fn [event] (.stopPropagation event) (set-editing not))}
        (or value "DEF"))
      (if editing
        ($ :form.initiative-token-form
          {:ref form
           :data-type "defence"
           :on-submit
           (fn []
             (on-change (fn [_ v] v) (.-value @input))
             (set-editing not))}
          ($ :input.text.text-ghost
            {:type        "number"
             :name        "defence"
             :ref         input
             :auto-focus  true
             :placeholder "Defence"
             :aria-label  "Defence"})
          (for [[key label f] [["-" "Subtract from" -]
                               ["+" "Add to" +]
                               ["=" "Set as" (fn [_ v] v)]]]
            ($ :button
              {:key key :type "button" :aria-label label
               :on-click
               (fn []
                 (on-change f (.-value @input))
                 (set-editing not))} key)))))))

;; ── HP tracker (unchanged from original) ────────────────────────────────────

(defui ^:private form-hp
  [{:keys [value on-change]}]
  (let [[editing set-editing form] (hooks/use-modal)
        input (uix/use-ref)]
    ($ :.initiative-token-health
      {:data-present (some? value)}
      ($ :.initiative-token-health-frame
        ($ icon {:name "heart-fill" :size 40}))
      ($ :button.initiative-token-health-label
        {:on-click (fn [event] (.stopPropagation event) (set-editing not))}
        (or value "HP"))
      (if editing
        ($ :form.initiative-token-form
          {:ref form
           :data-type "health"
           :on-submit
           (fn []
             (on-change (fn [_ v] v) (.-value @input))
             (set-editing not))}
          ($ :input.text.text-ghost
            {:type        "number"
             :name        "hitpoints"
             :ref         input
             :auto-focus  true
             :placeholder "Hitpoints"
             :aria-label  "Hitpoints"})
          (for [[key label f] [["-" "Subtract from" -]
                               ["+" "Add to" +]
                               ["=" "Set as" (fn [_ v] v)]]]
            ($ :button
              {:key key :type "button" :aria-label label
               :on-click
               (fn []
                 (on-change f (.-value @input))
                 (set-editing not))} key)))))))

;; ── Token row ────────────────────────────────────────────────────────────────

(defui ^:private token
  [{:keys [context entity]}]
  (let [dispatch (hooks/use-dispatch)
        {host :user/host
         {{curr  :initiative/turn
           phase :initiative/phase
           rnds  :initiative/rounds
           went  :initiative/played}
          :camera/scene} :user/camera} context
        {id       :db/id
         label    :token/label
         flags    :token/flags
         suffix   :initiative/suffix
         declared :initiative/declared-action
         {{hash :image/hash} :image/thumbnail} :token/image} entity
        playing (= (:db/id curr) (:db/id entity))
        played  (boolean (some #{{:db/id id}} went))
        hidden  (and (not host) (:object/hidden entity))
        active  (contains? (get phase-active-actions (or phase 0) #{}) declared)]
    ($ :li.initiative-token
      {:data-playing playing
       :data-played  played
       :data-hidden  hidden
       :data-active  active
       :data-type    "token"}
      ($ :button.initiative-token-turn
        {:disabled (or (nil? rnds) (zero? rnds))
         :on-click
         (fn []
           (if played
             (dispatch :initiative/unmark id)
             (dispatch :initiative/mark id)))}
        ($ icon {:name "arrow-right-short"}))
      ($ :.initiative-token-frame
        {:on-click  #(dispatch :objects/select id)
         :data-player (contains? flags :player)
         :data-hidden hidden}
        (cond hidden \?
              (some? hash)
              ($ image {:hash hash}
                (fn [url]
                  ($ :.initiative-token-image
                    {:style {:background-image (str "url(" url ")")}})))
              :else
              ($ :.initiative-token-pattern
                ($ icon {:name "dnd" :size 36}))))
      (if suffix
        ($ :.initiative-token-suffix (char (+ suffix 64))))
      ($ :.initiative-token-info
        (if (not (blank? label))
          ($ :.initiative-token-label label))
        ($ form-action
          {:value     declared
           :phase     (or phase 0)
           :on-change (fn [action]
                        (dispatch :initiative/change-declared-action id action))})
        (if (seq flags)
          ($ :.initiative-token-flags
            (join ", " (mapv (comp capitalize name) flags))))
        (if-let [url (:token-image/url (:token/image entity))]
          (if (or host (:image/public (:token/image entity)))
            (if-let [parsed (js/URL.parse url)]
              ($ :a.initiative-token-url
                {:href (.-href parsed) :target "_blank"}
                (str (.-hostname parsed)
                     (if (not= (.-pathname parsed) "/")
                       (.-pathname parsed))))
              ($ :.initiative-token-url url)))))
      ($ form-defence
        {:value     (:initiative/defence entity)
         :on-change (fn [f v]
                      (dispatch :initiative/change-defence id f v))})
      (if (or host (contains? flags :player))
        ($ form-hp
          {:value     (:initiative/health entity)
           :on-change (fn [f v]
                        (dispatch :initiative/change-health id f v))})))))

;; ── Placeholder row (shown before combat starts) ────────────────────────────

(defui ^:private token-placeholder []
  ($ :li.initiative-token {:data-type "placeholder"}
    ($ :.initiative-token-turn
      ($ icon {:name "arrow-right-short"}))
    ($ :.initiative-token-frame
      ($ :.initiative-token-pattern))
    ($ :.initiative-token-info)
    ($ :.initiative-token-defence
      ($ :.initiative-token-defence-frame
        ($ icon {:name "fist" :size 40}))
      ($ :.initiative-token-defence-label))
    ($ :.initiative-token-health
      ($ :.initiative-token-health-frame
        ($ icon {:name "heart-fill" :size 40}))
      ($ :.initiative-token-health-label))))

;; ── Main panel ───────────────────────────────────────────────────────────────

(defui ^:memo panel []
  (let [dispatch (hooks/use-dispatch)
        result   (hooks/use-query query)
        {{{tokens :scene/initiative
           rounds :initiative/rounds
           phase  :initiative/phase} :camera/scene}
         :user/camera} result]
    ($ :.initiative
      ($ :header
        ($ :h2 (if (and (>= (or rounds 0) 1) (some? phase))
                 (get phase-names phase "Initiative")
                 "Initiative"))
        (if (>= (or rounds 0) 1)
          ($ :h3 "Round " rounds)))
      (cond
        (and (not (seq tokens)) (nil? rounds))
        ($ :ol.initiative-list.initiative-list-placeholder
          (for [indx (range 6)]
            (if (= indx 1)
              ($ :.initiative-prompt {:key indx :style {:text-align "center"}}
                "Begin the Tactical Round by selecting one or more tokens and
                 clicking the hourglass button.")
              ($ token-placeholder {:key indx}))))

        (and (not (seq tokens)) (>= (or rounds 0) 1))
        ($ :.prompt
          ($ :br)
          "The round is running but no tokens are in initiative."
          ($ :br) ($ :br)
          ($ :button.button.button-neutral
            {:on-click #(dispatch :initiative/leave)} "Leave initiative"))

        (seq tokens)
        ($ :ol.initiative-list
          (for [entity (sort token-order tokens)]
            ($ token {:key (:db/id entity) :entity entity :context result})))))))

;; ── Action bar (Next Phase, Leave, etc.) ─────────────────────────────────────

(defui ^:memo actions []
  (let [dispatch (hooks/use-dispatch)
        result   (hooks/use-query query-actions)
        {{{rounds :initiative/rounds
           phase  :initiative/phase
           tokens :scene/initiative}
          :camera/scene}
         :user/camera} result
        on-quit (uix/use-callback #(dispatch :initiative/leave) [dispatch])
        on-next (uix/use-callback #(dispatch :initiative/next)  [dispatch])
        cur-phase (or phase 0)]
    ($ :<>
      ($ :button.button.button-neutral
        {:disabled (empty? tokens) :on-click on-quit} "Leave")
      (cond
        (not (seq tokens))
        ($ :button.button.button-neutral {:disabled true} "Next Phase")

        (nil? rounds)
        ($ :button.button.button-primary {:on-click on-next} "Start Round")

        (= cur-phase 8)
        ($ :button.button.button-primary {:on-click on-next} "New Round")

        :else
        ($ :button.button.button-neutral {:on-click on-next}
           (str "→ " (get phase-names (inc cur-phase) "Next")))))))
