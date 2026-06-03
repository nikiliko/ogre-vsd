(ns ogres.app.component.scene-context-menu
  (:require [clojure.string :refer [capitalize]]
            [ogres.app.component :refer [icon]]
            [ogres.app.component.scene-pattern :refer [pattern]]
            [ogres.app.context :as context]
            [ogres.app.hooks :as hooks]
            [ogres.app.util :as util]
            [uix.core :as uix :refer [defui $]]))

(defn ^:private token-size [x]
  (cond (<= x 3)  "Tiny"
        (<  x 5)  "Small"
        (<= x 5)  "Medium"
        (<= x 10) "Large"
        (<= x 15) "Huge"
        (>  x 15) "Gargantuan"
        :else     "Unknown"))

(def ^:private token-conditions
  [{:value :dying         :icon "skull"}
   {:value :engaged       :icon "fist"}
   {:value :frightened    :icon "black-cat"}
   {:value :held          :icon "lock-fill"}
   {:value :incapacitated :icon "slash-lg"}
   {:value :prone         :icon "tripwire"}
   {:value :stunned       :icon "emoji-dizzy"}
   {:value :surprised     :icon "exclamation-triangle-fill"}
   {:value :weary             :icon "moon-fill"}
   {:value :positional-bonus :icon "crosshair"}])

(def ^:private initiative-combat-actions
  [{:value "Spell A" :svg "/icons/spell.svg"  :label "Spell Phase A" :abbr "spl" :badge "A"}
   {:value "Range A" :svg "/icons/ranged.svg" :label "Range Phase A" :abbr "rng" :badge "A"}
   {:value "Melee"   :svg "/icons/melee.svg"  :label "Melee Phase"   :abbr "mel" :badge nil}
   {:value "Range B" :svg "/icons/ranged.svg" :label "Range Phase B" :abbr "rng" :badge "B"}
   {:value "Spell B" :svg "/icons/spell.svg"  :label "Spell Phase B" :abbr "spl" :badge "B"}
   {:value "Wait"    :svg "/icons/other_actions.svg" :label "Other Actions" :abbr "oth" :badge nil}])

(def ^:private shape-colors
  ["red"   "orange"  "amber"  "yellow" "lime"
   "green" "emerald" "teal"   "cyan"   "sky"
   "blue"  "indigo"  "violet" "purple" "fuchsia"])

(def ^:private shape-patterns
  [{:value :solid   :label "Fill"}
   {:value :empty   :label "Empty"}
   {:value :lines   :label "Lines"}
   {:value :crosses :label "Crosses"}
   {:value :caps    :label "Caps"}])

(defn ^:private prevent-default [event]
  (.preventDefault event))

(defn ^:private stop-propagation [event]
  (.stopPropagation event))

(defui ^:private action-hide
  [{:keys [value disabled on-change]
    :or   {value false disabled false on-change prevent-default}}]
  ($ :label.context-menu-action
    {:data-tooltip (if value "Reveal" "Hide")}
    ($ :input
      {:type "checkbox"
       :name "hidden"
       :checked value
       :disabled disabled
       :aria-disabled disabled
       :on-change
       (fn [event]
         (on-change (.-checked (.-target event))))})
    ($ icon {:name (if value "eye-slash-fill" "eye-fill")})))

(defui ^:private action-lock
  [{:keys [value disabled on-change]
    :or   {value false disabled false on-change prevent-default}}]
  ($ :label.context-menu-action
    {:data-tooltip (if value "Unlock" "Lock")}
    ($ :input
      {:type "checkbox"
       :name "hidden"
       :checked value
       :disabled disabled
       :aria-disabled disabled
       :on-change
       (fn [event]
         (on-change (.-checked (.-target event))))})
    ($ icon {:name (if value "lock" "unlock")})))

(defui ^:private action-remove
  [{:keys [disabled on-click]
    :or   {disabled false on-click prevent-default}}]
  ($ :button
    {:type "button"
     :disabled disabled
     :data-tooltip "Remove"
     :style {:margin-left "auto"}
     :on-click (fn [] (on-click))}
    ($ icon {:name "trash3-fill"})))

(defui ^:private context-menu-fn
  [{:keys [render-toolbar render-aside children]
    :or   {render-toolbar (constantly nil)
           render-aside   (constantly nil)}}]
  (let [[selected set-selected] (uix/use-state nil)
        props {:selected  selected
               :on-change (fn [form]
                            (if (= selected form)
                              (set-selected nil)
                              (set-selected form)))}]
    ($ :.context-menu {:on-pointer-down stop-propagation}
      ($ :.context-menu-main {:data-expanded (some? selected)}
        ($ :.context-menu-toolbar
          (render-toolbar props))
        (if selected
          ($ :.context-menu-form
            {:class (str "context-menu-form-" (name selected))}
            (children props))))
      ($ :.context-menu-aside
        ($ :.context-menu-toolbar
          (render-aside props))))))

(defui ^:private checkbox
  [{:keys [checked children]}]
  (let [input (uix/use-ref)
        indtr (= checked :indeterminate)]
    (uix/use-effect
     (fn [] (set! (.-indeterminate @input) indtr)) [indtr])
    (children input)))

(defui ^:private token-form-label
  [{:keys [values on-change on-close]
    :or   {values    (constantly (list))
           on-change identity
           on-close  identity}}]
  (let [[dirty set-dirty] (uix/use-state false)
        input (uix/use-ref)
        label (let [vs (values :token/label)]
                (if (= (count vs) 1) (first vs) ""))]
    (uix/use-effect
     (fn [] (.select @input)) [])
    ($ :form
      {:on-submit
       (fn [event]
         (.preventDefault event)
         (let [value (.. event -target -elements -label -value)]
           (on-change :token/change-label value)
           (on-close)))
       :on-blur
       (fn [event]
         (when dirty
           (on-change :token/change-label (.-value (.-target event))))
         (on-close))}
      ($ :input.text.text-ghost
        {:type "text"
         :name "label"
         :placeholder "Change token label"
         :default-value label
         :ref input
         :auto-focus true
         :on-change
         (fn []
           (set-dirty true))}))))

(defui ^:private token-form-details
  [{:keys [on-change values]
    :or   {values (constantly (list)) on-change identity}}]
  (let [value-fn (fn [attr] (first (into (sorted-set-by >) (values attr))))]
    ($ :<>
      (let [value (value-fn :token/size)]
        ($ :<>
          ($ :label "Size")
          ($ :button
            {:type "button"
             :auto-focus true
             :on-click #(on-change :token/change-size (max (- value 5) 5))
             :aria-label "Decrease token size by 5 feet"}
            "-")
          ($ :data {:value value}
            (str value  "ft. " (token-size value)))
          ($ :button
            {:type "button"
             :on-click #(on-change :token/change-size (min (+ value 5) 50))
             :aria-label "Increase token size by 5 feet"} "+")))
      (let [value (value-fn :token/light)]
        ($ :<>
          ($ :label "Light")
          ($ :button
            {:type "button"
             :on-click #(on-change :token/change-light (max (- value 5) 0))
             :aria-label "Decrease light radius by 5 feet"}
            "-")
          ($ :data {:value value}
            (if (> value 0) (str value "ft. radius") "None"))
          ($ :button
            {:type "button"
             :on-click #(on-change :token/change-light (min (+ value 5) 120))
             :aria-label "Increase light radius by 5 feet"}
            "+")))
      (let [value (value-fn :token/aura-radius)]
        ($ :<>
          ($ :label "Aura")
          ($ :button
            {:type "button"
             :on-click #(on-change :token/change-aura (max (- value 5) 0))
             :aria-label "Decrease aura size by 5 feet"}
            "-")
          ($ :data {:value value}
            (if (> value 0) (str value "ft. radius") "None"))
          ($ :button
            {:type "button"
             :on-click #(on-change :token/change-aura (min (+ value 5) 120))
             :aria-label "Increase aura size by 5 feet"}
            "+"))))))

(defui ^:private token-form-conditions
  [props]
  (let [fqs (frequencies (reduce into [] ((:values props) :token/flags [])))
        ids ((:values props) :db/id)
        [_ set-hovered] (uix/use-context context/condition-hover)]
    (for [{value :value icon-name :icon} token-conditions
          :let [focus (= value (:value (first token-conditions)))
                state (cond (= (get fqs value 0) 0) false
                            (= (get fqs value 0) (count ids)) true
                            :else :indeterminate)]]
      ($ checkbox {:key value :checked state}
        (fn [input]
          ($ :label
            {:aria-label      (name value)
             :data-tooltip    (capitalize (name value))
             :on-mouse-enter  #(set-hovered value)
             :on-mouse-leave  #(set-hovered nil)}
            ($ :input
              {:ref input
               :type "checkbox"
               :name (str "token-condition-" (name value))
               :checked (if (= state :indeterminate) false state)
               :auto-focus focus
               :on-change
               (fn [event]
                 (let [checked (.. event -target -checked)]
                   ((:on-change props) :token/change-flag value checked)))})
            ($ icon {:name icon-name})))))))

(def ^:private query-phase
  [{:user/camera [{:camera/scene [[:initiative/phase :default nil]
                                  {:scene/initiative [:db/id]}]}]}])

(defui ^:private initiative-picker
  [{:keys [idxs]}]
  (let [phase-result  (hooks/use-query query-phase)
        scene         (get-in phase-result [:user/camera :camera/scene])
        cur-phase     (or (:initiative/phase scene) 0)
        init-ids      (into #{} (map :db/id) (:scene/initiative scene))
        in-initiative (and (seq idxs) (every? init-ids idxs))
        declaration?  (= cur-phase 1)
        assessment?   (= cur-phase 0)
        dispatch    (hooks/use-dispatch)
        [open set-open dropdown] (hooks/use-modal)
        [move-sel    set-move]    (uix/use-state false)
        [readied-sel set-readied] (uix/use-state false)
        move-ref    (uix/use-ref false)
        readied-ref (uix/use-ref false)
        commit
        (fn [action-value also-move?]
          (dispatch :initiative/toggle idxs true)
          (doseq [id idxs]
            (dispatch :initiative/change-declared-action id action-value)
            (dispatch :initiative/set-also-move id also-move?)
            (dispatch :initiative/set-readied id (.-current readied-ref))))
        reset-local
        (fn []
          (set! (.-current move-ref) false)
          (set-move false)
          (set! (.-current readied-ref) false)
          (set-readied false))]
    (uix/use-effect
     (fn []
       (when (not open) (reset-local)))
     [open])
    ($ :.initiative-picker
      ($ :button
        {:type          "button"
         :data-selected in-initiative
         :data-phase    (cond assessment?   "assessment"
                             declaration?  "declaration"
                             in-initiative "combat"
                             :else         nil)
         :data-tooltip  (cond
                          assessment?
                          "Declaration phase only. Double-click to force."
                          (and in-initiative (not declaration?))
                          "Actions already declared — double-click to modify"
                          :else "Initiative")
         :on-click
         (fn [event]
           (.stopPropagation event)
           (when declaration?
             (set-open not)))
         :on-double-click
         (fn [event]
           (.stopPropagation event)
           (when (not declaration?)
             (set-open not)))}
        ($ :img {:src "/icons/initiative.svg" :alt "Initiative" :aria-hidden true
                 :style {:width "18px" :height "18px"
                         :opacity (cond assessment?                            0.4
                                        (and in-initiative (not declaration?)) 0.65
                                        :else                                  1)
                         :filter (cond
                                   declaration?
                                   "invert(0.3) sepia(1) saturate(10) hue-rotate(320deg)"
                                   assessment?
                                   "invert(0.6) grayscale(1)"
                                   (and in-initiative (not declaration?))
                                   "invert(0.3) sepia(1) saturate(6) hue-rotate(10deg)"
                                   :else "invert(0.6)")}}))
      (if open
        ($ :.initiative-picker-dropdown
          {:ref dropdown}
          ($ :.initiative-picker-row
            ;; ── Move ──
            ($ :button
              {:type       "button"
               :title      (if move-sel "Click again to confirm Move only" "Move Phase")
               :data-state (cond (and move-sel readied-sel) "combined"
                                 move-sel "move"
                                 readied-sel "combined"
                                 :else nil)
               :on-click
               (fn [event]
                 (.stopPropagation event)
                 (if (.-current move-ref)
                   (do (commit "Move" false)
                       (reset-local)
                       (set-open false))
                   (do (set! (.-current move-ref) true)
                       (set-move true))))}
              ($ :img {:src "/icons/move.svg" :alt "Move" :aria-hidden true})
              ($ :.initiative-picker-abbr "mov"))
            ($ :.initiative-picker-divider)
            ;; ── Combat actions ──
            (for [{svg-src :svg value :value label :label abbr :abbr badge :badge}
                  initiative-combat-actions]
              ($ :button
                {:key        value
                 :type       "button"
                 :title      label
                 :data-state (cond (and move-sel readied-sel) "combined"
                                   move-sel "combined"
                                   readied-sel "combined"
                                   :else nil)
                 :on-click
                 (fn []
                   (commit value (.-current move-ref))
                   (reset-local)
                   (set-open false))}
                (if badge ($ :.initiative-picker-badge badge))
                ($ :img {:src svg-src :alt label :aria-hidden true})
                ($ :.initiative-picker-abbr abbr)))
            ;; ── Readied separator + button ──
            ($ :.initiative-picker-divider-thick)
            ($ :button
              {:type       "button"
               :title      (if readied-sel "Select the action to ready" "Readied Action")
               :data-state (if readied-sel "readied" nil)
               :on-click
               (fn [event]
                 (.stopPropagation event)
                 (let [new-val (not (.-current readied-ref))]
                   (set! (.-current readied-ref) new-val)
                   (set-readied new-val)))}
              ($ icon {:name "hourglass-split"})
              ($ :.initiative-picker-abbr "rdy")))
          (if readied-sel
            ($ :.initiative-picker-hint "Select the action to ready")))))))

(defui ^:private context-menu-token [props]
  (let [dispatch (hooks/use-dispatch)
        data     (:data props)
        idxs     (into [] (map :db/id) data)
]
    ($ context-menu-fn
      {:render-toolbar
       (fn [{:keys [selected on-change]}]
         ($ :<>
           (for [[form icon-name tooltip]
                 [[:label "fonts" "Label"]
                  [:details "sliders" "Options"]
                  [:conditions "arrow-through-heart-fill" "Conditions"]]]
             ($ :button
               {:key form
                :type "button"
                :data-selected (= selected form)
                :data-tooltip tooltip
                :on-click #(on-change form)}
               ($ icon {:name icon-name})))
           ($ initiative-picker
             {:idxs idxs})
           (let [on (every? (comp boolean :player :token/flags) data)]
             ($ :button
               {:type "button"
                :data-tooltip "Player"
                :data-selected on
                :on-click #(dispatch :token/change-flag idxs :player (not on))}
               ($ icon {:name "people-fill"})))
           (let [on (every? (comp boolean :dead :token/flags) data)]
             ($ :button
               {:type "button"
                :data-tooltip "Dead"
                :data-selected on
                :on-click #(dispatch :token/change-dead idxs (not on))}
               ($ icon {:name "skull"})))
           (let [xfr (comp :token-image/url :token/image)
                 url (js/URL.parse (xfr (first data)))]
             (if (and (some? url)
                      (util/uniform-by xfr data)
                      (or (:host props)
                          (every? (comp :image/public :token/image) data)))
               ($ :a {:href (.-href url) :target "_blank" :data-tooltip "Open link"}
                 ($ icon {:name "box-arrow-up-right"}))))))
       :render-aside
       (fn []
         ($ :<>
           ($ action-hide
             {:value (every? :object/hidden data)
              :disabled (not (:host props))
              :on-change
              (fn []
                (dispatch :objects/toggle-hidden-selected))})
           ($ action-remove
             {:on-click
              (fn []
                (dispatch :objects/remove-selected))})))}
      (fn [{:keys [selected on-change]}]
        (let [props {:on-close  #(on-change nil)
                     :on-change #(apply dispatch %1 idxs %&)
                     :values    (fn vs
                                  ([f] (vs f #{}))
                                  ([f init] (into init (map f) data)))}]
          (case selected
            :label      ($ token-form-label props)
            :details    ($ token-form-details props)
            :conditions ($ token-form-conditions props)))))))

(defui ^:private shape-form-style
  [{:keys [on-change values]}]
  ($ :.context-menu-form-styles
    (for [{:keys [value label]} shape-patterns]
      (let [id (str "template-pattern-" (name value))]
        ($ :label {:key value :aria-label label}
          ($ :input
            {:type "radio"
             :name "shape-pattern"
             :value value
             :checked (= value (first (values :shape/pattern)))
             :on-change
             (fn [event]
               (let [value (.. event -target -value)]
                 (on-change :objects/update :shape/pattern (keyword value))))})
          ($ :svg
            ($ :defs ($ pattern {:id id :name value}))
            ($ :rect {:width "100%" :height "100%" :fill (str "url(#" id ")")})))))
    (for [value shape-colors]
      ($ :label {:key value :aria-label value :data-color value}
        ($ :input
          {:type "radio"
           :name "shape-color"
           :value value
           :checked (= value (first (values :shape/color)))
           :on-change
           (fn [event]
             (on-change :objects/update :shape/color (.. event -target -value)))})))))

(defui ^:private context-menu-shape [props]
  (let [dispatch (hooks/use-dispatch)
        data     (:data props)]
    ($ context-menu-fn
      {:render-toolbar
       (fn [{:keys [selected on-change]}]
         ($ :button
           {:type "button"
            :data-selected (= selected :style)
            :data-tooltip "Style"
            :on-click #(on-change :style)}
           ($ icon {:name "palette-fill"})))
       :render-aside
       (fn []
         ($ :<>
           ($ action-lock
             {:value (every? :object/locked data)
              :disabled (not (:host props))
              :on-change
              (fn []
                (dispatch :objects/toggle-locked-selected))})
           ($ action-remove
             {:on-click
              (fn []
                (dispatch :objects/remove-selected))})))}
      (fn [{:keys [selected]}]
        (if (= selected :style)
          ($ shape-form-style
            {:values
             (fn vs
               ([f] (vs f #{}))
               ([f init] (into init (map f) data)))
             :on-change
             (fn [event & args]
               (apply dispatch event (map :db/id data) args))}))))))

(defui context-menu-prop [props]
  (let [dispatch (hooks/use-dispatch)
        data     (:data props)]
    ($ context-menu-fn
      {:render-toolbar
       (fn []
         ($ :button
           {:type "button"
            :data-tooltip "Reset size/rotation"
            :on-click
            (fn []
              (dispatch :objects/reset-transform-selected))}
           ($ icon {:name "arrows-angle-expand"})))
       :render-aside
       (fn []
         ($ :<>
           ($ action-hide
             {:value (every? :object/hidden data)
              :disabled (not (:host props))
              :on-change
              (fn []
                (dispatch :objects/toggle-hidden-selected))})
           ($ action-lock
             {:value (every? :object/locked data)
              :disabled (not (:host props))
              :on-change
              (fn []
                (dispatch :objects/toggle-locked-selected))})
           ($ action-remove
             {:on-click
              (fn []
                (dispatch :objects/remove-selected))})))}
      (fn [{:keys []}]))))

(defui context-menu [props]
  (if (util/uniform-by (comp namespace :object/type) (:data props))
    (case (namespace (:object/type (first (:data props))))
      "prop"  ($ context-menu-prop  props)
      "shape" ($ context-menu-shape props)
      "token" ($ context-menu-token props)
      nil)))
