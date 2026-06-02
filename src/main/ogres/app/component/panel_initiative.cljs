(ns ogres.app.component.panel-initiative
  (:require [clojure.string :refer [join capitalize blank?]]
            [ogres.app.component :refer [icon image]]
            [ogres.app.hooks :as hooks]
            [uix.core :as uix :refer [defui $]]))

;; ── Tactical Round Sequence ──────────────────────────────────────────────────

(def ^:private phase-names
  ["Assessment" "Declaration" "Move Phase"
   "Spell Phase A" "Range Phase A" "Melee Phase"
   "Range Phase B" "Spell Phase B" "Other Actions"])

(def ^:private action-options
  ["Move" "Spell A" "Range A" "Melee" "Range B" "Spell B" "Wait"])

(def ^:private phase-active-actions
  {2 #{"Move"}
   3 #{"Spell A"}
   4 #{"Range A"}
   5 #{"Melee"}
   6 #{"Range B"}
   7 #{"Spell B"}
   8 #{"Wait"}})

(def ^:private action-phase-order
  {"Move" 0 "Spell A" 1 "Range A" 2 "Melee" 3
   "Range B" 4 "Spell B" 5 "Wait" 6})

(def ^:private action-section-name
  {"Spell A" "Spell Phase A" "Range A" "Range Phase A"
   "Melee"   "Melee Phase"   "Range B" "Range Phase B"
   "Spell B" "Spell Phase B" "Wait"    "Other Actions"})

(def ^:private action-svg
  {"Move"    "/icons/move.svg"
   "Spell A" "/icons/spell.svg"  "Spell B" "/icons/spell.svg"
   "Range A" "/icons/ranged.svg" "Range B" "/icons/ranged.svg"
   "Melee"   "/icons/melee.svg"  "Wait"    "/icons/other_actions.svg"})

(def ^:private action-ab-badge
  {"Spell A" "A" "Range A" "A" "Spell B" "B" "Range B" "B"})

;; ── Queries ───────────────────────────────────────────────────────────────────

(def ^:private query
  [:user/host
   {:user/camera
    [{:camera/scene
      [:db/id
       :initiative/rounds
       :initiative/phase
       :initiative/turn
       :initiative/played
       {:scene/tokens
        [:db/id
         :token/label
         :token/flags
         {:token/image
          [{:image/thumbnail
            [:image/hash]}]}]}
       {:scene/initiative
        [:db/id
         :object/hidden
         :token/label
         :token/flags
         :initiative/suffix
         :initiative/health
         :initiative/bleeding
         :initiative/declared-action
         :initiative/also-move
         :initiative/readied
         :initiative/action-modified
         :initiative/other-action
         :initiative/prev-declared-action
         :initiative/prev-also-move
         :initiative/prev-readied
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

(defn ^:private token-order [a b]
  (let [pa (get action-phase-order (:initiative/declared-action a) 99)
        pb (get action-phase-order (:initiative/declared-action b) 99)]
    (if (= pa pb) (compare (:db/id a) (:db/id b)) (compare pa pb))))

(defn ^:private token-active? [cur-phase entity]
  (let [declared  (:initiative/declared-action entity)
        also-move (:initiative/also-move entity)]
    (or (contains? (get phase-active-actions cur-phase #{}) declared)
        (boolean (and (= cur-phase 2) also-move)))))

;; ── Declared-action selector ─────────────────────────────────────────────────

(def ^:private other-action-opts
  ["Use a Skill" "Concentrate" "Preparing a Spell" "Aiming" "Other"])

(defui ^:private form-action
  [{:keys [value also-move on-change phase modified other-action]}]
  (let [[editing set-editing form] (hooks/use-modal)
        [mode set-mode]            (uix/use-state :main)
        [other-text set-other-text] (uix/use-state "")
        editable (= phase 1)
        wait?    (= value "Wait")
        display  (cond
                   (and wait? (some? other-action)) other-action
                   wait?         "Other Act."
                   (some? value) value
                   :else         "—")]
    ($ :.initiative-token-action
      {:data-present (or (some? value) also-move)
       :data-value   (or value "")}
      (if (and also-move (= phase 2))
        ($ :.initiative-token-action-move "Move"))
      (if (or value (not also-move))
        ($ :button.initiative-token-action-label
          {:disabled (not editable)
           :title    (if editable "Click to set declared action" "")
           :on-click
           (fn [event]
             (.stopPropagation event)
             (when editable
               (set-mode (if wait? :sub :main))
               (set-other-text "")
               (set-editing not)))}
          ($ :<>
            display
            (if modified ($ :.initiative-token-action-modified " *")))))
      (if (and also-move (not= phase 2))
        ($ :.initiative-token-action-move "Move"))
      (if editing
        ($ :.initiative-token-action-menu
          {:ref form}
          (case mode
            :main
            (for [action action-options]
              ($ :button
                {:key        action
                 :type       "button"
                 :data-active (= action value)
                 :on-click
                 (fn []
                   (if (= action "Wait")
                     (set-mode :sub)
                     (do (on-change action nil)
                         (set-editing false))))}
                (if (= action "Wait") "Other Act." action)))
            :sub
            ($ :<>
              ($ :button.initiative-action-back
                {:type "button" :on-click #(set-mode :main)}
                "← Back")
              (for [opt other-action-opts]
                ($ :button
                  {:key        opt
                   :type       "button"
                   :data-active (= opt other-action)
                   :on-click
                   (fn []
                     (if (= opt "Other")
                       (set-mode :other-text)
                       (do (on-change "Wait" opt)
                           (set-editing false))))}
                  opt)))
            :other-text
            ($ :form.initiative-action-other-form
              {:on-submit
               (fn [ev]
                 (.preventDefault ev)
                 (on-change "Wait" (if (blank? other-text) "Other" other-text))
                 (set-editing false)
                 (set-other-text ""))}
              ($ :input
                {:type        "text"
                 :value       other-text
                 :auto-focus  true
                 :placeholder "Describe action..."
                 :on-change   #(set-other-text (.. % -target -value))})
              ($ :button {:type "submit"} "OK"))))))))

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
        value)
      (if editing
        ($ :form.initiative-token-form
          {:ref form
           :data-type "defence"
           :on-submit
           (fn []
             (on-change (fn [_ v] v) (.-value @input))
             (set-editing not))}
          ($ :input.text.text-ghost
            {:type "number" :name "defence" :ref input
             :auto-focus true :placeholder "Value" :aria-label "Defence"})
          ($ :.initiative-token-form-ops
            (for [[label f] [["− Reduce" -]
                             ["+ Add"    +]
                             ["= Set to" (fn [_ v] v)]]]
              ($ :button {:key label :type "button"
                          :on-click (fn [] (on-change f (.-value @input)) (set-editing not))}
                 label))))))))



;; ── HP tracker ───────────────────────────────────────────────────────────────

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
            {:type "number" :name "hitpoints" :ref input
             :auto-focus true :placeholder "Value" :aria-label "Hitpoints"})
          ($ :.initiative-token-form-ops
            (for [[label f] [["− Damage" -]
                             ["+ Heal"   +]
                             ["= Set to" (fn [_ v] v)]]]
              ($ :button {:key label :type "button"
                          :on-click (fn [] (on-change f (.-value @input)) (set-editing not))}
                 label))))))))


;; ── Bleeding tracker ─────────────────────────────────────────────────────────

(defui ^:private form-bleeding
  [{:keys [value on-change]}]
  (let [[editing set-editing form] (hooks/use-modal)
        input (uix/use-ref)]
    ($ :.initiative-token-bleeding
      {:data-present (some? value)}
      ($ :.initiative-token-bleeding-frame
        ($ :img {:src "/icons/bleed.svg" :alt "Bleeding" :aria-hidden true}))
      ($ :button.initiative-token-bleeding-label
        {:on-click (fn [event] (.stopPropagation event) (set-editing not))}
        (or value "BLD"))
      (if editing
        ($ :form.initiative-token-form
          {:ref form
           :data-type "bleeding"
           :on-submit
           (fn []
             (on-change (fn [_ v] v) (.-value @input))
             (set-editing not))}
          ($ :input.text.text-ghost
            {:type "number" :name "bleeding" :ref input :min "0"
             :auto-focus true :placeholder "Value" :aria-label "Bleeding"})
          ($ :.initiative-token-form-ops
            (for [[label f] [["− Reduce" -]
                             ["+ Add"    +]
                             ["= Set to" (fn [_ v] v)]]]
              ($ :button {:key label :type "button"
                          :on-click (fn [] (on-change f (.-value @input)) (set-editing not))}
                 label))))))))


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
        {id           :db/id
         label        :token/label
         flags        :token/flags
         suffix       :initiative/suffix
         declared     :initiative/declared-action
         other-action :initiative/other-action
         also-move    :initiative/also-move
         bleeding  :initiative/bleeding
         {{hash :image/hash} :image/thumbnail} :token/image} entity
        playing   (= (:db/id curr) (:db/id entity))
        played    (boolean (some #{{:db/id id}} went))
        hidden    (and (not host) (:object/hidden entity))
        cur-phase (or phase 0)
        active    (or (<= cur-phase 1) (token-active? cur-phase entity))]
    ($ :li.initiative-token
      {:data-playing playing
       :data-played  played
       :data-hidden  hidden
       :data-active  active
       :data-type    "token"}
      ($ :button.initiative-token-turn
        {:disabled (or (nil? rnds) (zero? rnds) (= cur-phase 1))
         :on-click
         (fn []
           (if played
             (dispatch :initiative/unmark id)
             (dispatch :initiative/mark id)))}
        ($ icon {:name "arrow-right-short"}))
      ($ :.initiative-token-frame
        {:on-click    #(dispatch :objects/select id)
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
          {:value        declared
           :also-move    also-move
           :phase        cur-phase
           :modified     (:initiative/action-modified entity)
           :other-action other-action
           :on-change    (fn [action detail]
                           (dispatch :initiative/change-declared-action id action)
                           (dispatch :initiative/set-other-action id detail))})
        (if (seq flags)
          ($ :.initiative-token-flags
            (for [flag flags]
              ($ :.initiative-token-flag {:key (name flag)}
                (capitalize (name flag))))))
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
         :on-change (fn [f v] (dispatch :initiative/change-defence id f v))})
      (if (or host (contains? flags :player))
        ($ form-hp
          {:value     (:initiative/health entity)
           :on-change (fn [f v] (dispatch :initiative/change-health id f v))}))
      ($ form-bleeding
        {:value     bleeding
         :on-change (fn [f v] (dispatch :initiative/change-bleeding id f v))}))))

;; ── Placeholder row ───────────────────────────────────────────────────────────

(defui ^:private token-placeholder []
  ($ :li.initiative-token {:data-type "placeholder"}
    ($ :.initiative-token-turn ($ icon {:name "arrow-right-short"}))
    ($ :.initiative-token-frame ($ :.initiative-token-pattern))
    ($ :.initiative-token-info)
    ($ :.initiative-token-defence
      ($ :.initiative-token-defence-frame ($ icon {:name "fist" :size 40}))
      ($ :.initiative-token-defence-label))
    ($ :.initiative-token-health
      ($ :.initiative-token-health-frame ($ icon {:name "heart-fill" :size 40}))
      ($ :.initiative-token-health-label))))

;; ── Declaration phase — chip components ──────────────────────────────────────

(defui ^:private token-move-chip
  [{:keys [entity]}]
  (let [{label :token/label
         {{hash :image/hash} :image/thumbnail} :token/image} entity]
    ($ :.initiative-move-chip
      ($ :.initiative-move-chip-frame
        (if (some? hash)
          ($ image {:hash hash}
            (fn [url]
              ($ :.initiative-move-chip-image
                {:style {:background-image (str "url(" url ")")}})))
          ($ :.initiative-move-chip-pattern
            ($ icon {:name "dnd" :size 20}))))
      (if (not (blank? label))
        ($ :.initiative-move-chip-label label)))))

(defui ^:private token-undeclared-chip
  [{:keys [entity]}]
  (let [{label :token/label
         {{hash :image/hash} :image/thumbnail} :token/image} entity]
    ($ :.initiative-undeclared-chip {:title (or label "?")}
      (if (some? hash)
        ($ image {:hash hash}
          (fn [url]
            ($ :.initiative-undeclared-chip-image
              {:style {:background-image (str "url(" url ")")}})))
        ($ :.initiative-undeclared-chip-pattern
          ($ icon {:name "dnd" :size 16}))))))

(defui ^:private token-prev-chip
  [{:keys [entity clickable]}]
  (let [dispatch (hooks/use-dispatch)
        {id             :db/id
         label          :token/label
         prev-declared  :initiative/prev-declared-action
         prev-also-move :initiative/prev-also-move
         prev-readied   :initiative/prev-readied
         {{hash :image/hash} :image/thumbnail} :token/image} entity
        svg-src  (get action-svg prev-declared)
        ab-badge (get action-ab-badge prev-declared)]
    ($ :.initiative-prev-chip
      {:data-clickable clickable
       :title (str (or label "?") " — " (or prev-declared "?"))
       :on-click
       (when clickable
         (fn []
           (dispatch :initiative/toggle [id] true)
           (when prev-declared
             (dispatch :initiative/change-declared-action id prev-declared))
           (when prev-also-move
             (dispatch :initiative/set-also-move id prev-also-move))
           (when prev-readied
             (dispatch :initiative/set-readied id prev-readied))))}
      ($ :.initiative-prev-chip-frame
        (if (some? hash)
          ($ image {:hash hash}
            (fn [url]
              ($ :.initiative-prev-chip-image
                {:style {:background-image (str "url(" url ")")}})))
          ($ :.initiative-prev-chip-pattern
            ($ icon {:name "dnd" :size 14}))))
      ($ :.initiative-prev-chip-action
        (if ab-badge ($ :.initiative-prev-chip-ab ab-badge))
        (if svg-src
          ($ :img {:src svg-src :alt prev-declared :aria-hidden true})
          ($ icon {:name "hourglass-split"}))))))

;; ── Pre-combat roster ────────────────────────────────────────────────────────

(defui ^:private roster-chip
  [{:keys [entity on-drag-start dragging]}]
  (let [{label :token/label
         {{hash :image/hash} :image/thumbnail} :token/image} entity]
    ($ :.initiative-roster-chip
      {:title         (or label "?")
       :draggable     true
       :data-dragging dragging
       :on-drag-start (fn [e]
                        (.setData (.-dataTransfer e) "text/plain" "")
                        (on-drag-start (:db/id entity)))
       :on-drag-end   (fn [] (on-drag-start nil))}
      (if (some? hash)
        ($ image {:hash hash}
          (fn [url]
            ($ :.initiative-roster-chip-image
              {:style {:background-image (str "url(" url ")")}})))
        ($ :.initiative-roster-chip-pattern
          ($ icon {:name "dnd" :size 16}))))))

(defui ^:private panel-roster
  [{:keys [all-tokens]}]
  (let [dispatch   (hooks/use-dispatch)
        [excluded  set-excluded] (uix/use-state #{})
        [dragging  set-dragging] (uix/use-state nil)
        [drop-zone set-drop-zone] (uix/use-state nil)
        in-tokens  (remove #(excluded (:db/id %)) all-tokens)
        out-tokens (filter  #(excluded (:db/id %)) all-tokens)]
    ($ :.initiative-roster
      ($ :.initiative-roster-section
        {:data-active   (= drop-zone :in)
         :on-drag-over  #(.preventDefault %)
         :on-drag-enter #(do (.preventDefault %) (set-drop-zone :in))
         :on-drop       (fn [e]
                          (.preventDefault e)
                          (set-drop-zone nil)
                          (when dragging
                            (set-excluded #(disj % dragging))
                            (set-dragging nil)))}
        ($ :.initiative-roster-section-header
          (str "In Combat" (when (seq in-tokens) (str " (" (count in-tokens) ")"))))
        (if (seq in-tokens)
          ($ :.initiative-roster-chips
            (for [entity in-tokens]
              ($ roster-chip {:key           (:db/id entity)
                              :entity        entity
                              :dragging      (= dragging (:db/id entity))
                              :on-drag-start set-dragging})))
          ($ :.initiative-roster-empty "Drop tokens here")))
      ($ :.initiative-roster-section.initiative-roster-section-out
        {:data-active   (= drop-zone :out)
         :on-drag-over  #(.preventDefault %)
         :on-drag-enter #(do (.preventDefault %) (set-drop-zone :out))
         :on-drop       (fn [e]
                          (.preventDefault e)
                          (set-drop-zone nil)
                          (when dragging
                            (set-excluded #(conj % dragging))
                            (set-dragging nil)))}
        ($ :.initiative-roster-section-header "Not In Combat")
        (if (seq out-tokens)
          ($ :.initiative-roster-chips
            (for [entity out-tokens]
              ($ roster-chip {:key           (:db/id entity)
                              :entity        entity
                              :dragging      (= dragging (:db/id entity))
                              :on-drag-start set-dragging})))
          ($ :.initiative-roster-empty "Drop tokens here")))
      ($ :.initiative-enter-combat
        ($ :button.button.button-primary
          {:on-click
           (fn []
             (let [ids (mapv :db/id in-tokens)]
               (when (seq ids)
                 (dispatch :initiative/toggle ids true)))
             (dispatch :initiative/next))}
          "Enter Combat")))))

;; ── Declaration phase — sections ─────────────────────────────────────────────

(defui ^:private section-move
  [{:keys [tokens]}]
  ($ :.initiative-section
    ($ :.initiative-section-header "Move")
    ($ :.initiative-move-chips
      (for [entity tokens]
        ($ token-move-chip {:key (:db/id entity) :entity entity})))
    ($ :.initiative-section-note
      "Conflicting Roll may be needed to establish move order")))

(defui ^:private section-phase
  [{:keys [phase-name tokens context]}]
  (let [any-modified? (some :initiative/action-modified tokens)]
    ($ :.initiative-section
      ($ :.initiative-section-header phase-name)
      ($ :ol
        (for [entity (sort token-order tokens)]
          ($ token {:key (:db/id entity) :entity entity :context context})))
      (when any-modified?
        ($ :.initiative-section-modified-note
          "* this action has been modified from the original declaration")))))

(defui ^:private section-readied
  [{:keys [tokens context]}]
  ($ :.initiative-section
    ($ :.initiative-section-header "Readied Actions")
    ($ :ol
      (for [entity tokens]
        ($ token {:key (:db/id entity) :entity entity :context context})))))

(defui ^:private section-undeclared
  [{:keys [tokens]}]
  ($ :.initiative-section.initiative-section-muted
    ($ :.initiative-section-header "Undeclared")
    ($ :.initiative-undeclared-chips
      (for [entity tokens]
        ($ token-undeclared-chip {:key (:db/id entity) :entity entity})))))

(defui ^:private section-previous-round
  [{:keys [tokens clickable]}]
  (let [prev-tokens (filter :initiative/prev-declared-action tokens)]
    (when (seq prev-tokens)
      ($ :.initiative-section.initiative-section-prev
        ($ :.initiative-section-header "Previous Round")
        (if clickable
          ($ :.initiative-section-msg
            "Click your token to repeat last round's action"))
        ($ :.initiative-prev-chips
          (for [entity prev-tokens]
            ($ token-prev-chip {:key (:db/id entity) :entity entity :clickable clickable})))))))

;; ── Declaration phase panel ───────────────────────────────────────────────────

(defui ^:private panel-declaration
  [{:keys [tokens rounds context]}]
  (let [move-tokens    (filter #(or (= (:initiative/declared-action %) "Move")
                                    (:initiative/also-move %)) tokens)
        readied-tokens (filter :initiative/readied tokens)
        phase-tokens   (filter #(and (some? (:initiative/declared-action %))
                                     (not= (:initiative/declared-action %) "Move")
                                     (not (:initiative/readied %))) tokens)
        undeclared     (filter #(nil? (:initiative/declared-action %)) tokens)
        by-action      (group-by :initiative/declared-action phase-tokens)
        combat-order   ["Spell A" "Range A" "Melee" "Range B" "Spell B" "Wait"]]
    ($ :div.initiative-declaration
      (if (seq move-tokens)
        ($ section-move {:tokens move-tokens}))
      (for [action combat-order
            :let   [group (get by-action action)]
            :when  (seq group)]
        ($ section-phase {:key        action
                          :phase-name (get action-section-name action action)
                          :tokens     group
                          :context    context}))
      (if (seq readied-tokens)
        ($ section-readied {:tokens readied-tokens :context context}))
      (if (seq undeclared)
        ($ section-undeclared {:tokens undeclared}))
      (if (>= (or rounds 0) 2)
        ($ section-previous-round {:tokens tokens :clickable true})))))

;; ── Main panel ───────────────────────────────────────────────────────────────

(defui ^:memo panel []
  (let [dispatch  (hooks/use-dispatch)
        result    (hooks/use-query query)
        {{{tokens       :scene/initiative
           scene-tokens :scene/tokens
           rounds :initiative/rounds
           phase  :initiative/phase} :camera/scene}
         :user/camera} result
        cur-phase (or phase 0)]
    ($ :.initiative
      ($ :header
        ($ :h2 (if (and (>= (or rounds 0) 1) (some? phase))
                 (get phase-names phase "Initiative")
                 "Initiative"))
        (if (>= (or rounds 0) 1)
          ($ :h3 "Round " rounds)))
      (cond
        ;; No tokens, no rounds started yet — roster setup
        (and (not (seq tokens)) (nil? rounds))
        ($ panel-roster {:all-tokens scene-tokens})

        ;; Round running but no tokens
        (and (not (seq tokens)) (>= (or rounds 0) 1))
        ($ :.prompt
          ($ :br)
          "The round is running but no tokens are in initiative."
          ($ :br) ($ :br)
          ($ :button.button.button-neutral
            {:on-click #(dispatch :initiative/leave)} "Leave initiative"))

        ;; Declaration phase
        (= cur-phase 1)
        ($ panel-declaration
          {:tokens  (sort token-order tokens)
           :rounds  rounds
           :context result})

        ;; Assessment after first round — show tokens (no badges) + Previous Round
        (and (= cur-phase 0) (>= (or rounds 0) 2))
        ($ :div
          ($ :ol.initiative-list
            (for [entity (sort token-order tokens)]
              ($ token {:key (:db/id entity) :entity entity :context result})))
          ($ section-previous-round {:tokens tokens :clickable false}))

        ;; Phases 2-8 — split into acting / waiting
        (and (seq tokens) (>= cur-phase 2))
        (let [sorted  (sort token-order tokens)
              acting  (filter (partial token-active? cur-phase) sorted)
              waiting (remove (partial token-active? cur-phase) sorted)]
          ($ :div
            ($ :ol.initiative-list
              (for [entity acting]
                ($ token {:key (:db/id entity) :entity entity :context result})))
            (when (seq waiting)
              ($ :.initiative-section.initiative-section-muted
                ($ :.initiative-section-header "Not in this phase")
                ($ :ol.initiative-list
                  (for [entity waiting]
                    ($ token {:key (:db/id entity) :entity entity :context result})))))))

        ;; Assessment phase with tokens (rounds = 0 or 1)
        (seq tokens)
        ($ :ol.initiative-list
          (for [entity (sort token-order tokens)]
            ($ token {:key (:db/id entity) :entity entity :context result})))))))

;; ── Actions bar ──────────────────────────────────────────────────────────────

(defui ^:memo actions []
  (let [dispatch  (hooks/use-dispatch)
        result    (hooks/use-query query-actions)
        {{{rounds :initiative/rounds
           phase  :initiative/phase
           tokens :scene/initiative}
          :camera/scene}
         :user/camera} result
        on-quit   (uix/use-callback #(dispatch :initiative/leave) [dispatch])
        on-next   (uix/use-callback #(dispatch :initiative/next)  [dispatch])
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
