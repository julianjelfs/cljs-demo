(ns cl-todo.core
  (:require
    [reagent.core :as reagent]
    ))

(defonce model
  (reagent/atom {:todos
                 [{:text "Write some code" :complete false}
                  , {:text "Get some milk" :complete true }]}))

(defn addTodo []
  (swap! model
         (fn [m]
           (let [todos (:todos m)
                 next (:nextTodo m)
                 todo {:text next :complete false }
                 updated (conj todos todo)]
             (assoc m :todos updated :nextTodo nil)))))

(defn toggleAtIndex [todos index]
  (update-in todos [index :complete] not))

(defn toggleTodo [index]
  (swap! model #(assoc % :todos (toggleAtIndex (:todos %) index))))

(defn removeAll []
  (swap! model assoc :todos []))

(defn updateNextTodo [txt]
  (swap! model assoc :nextTodo txt))

(defn renderTodo [index todo]
  [:div.todo {:key index :on-click #(toggleTodo index)}
   [:div.todo-text (if (:complete todo) {:class "complete"} {}) (:text todo)]
   [:div.todo-toggle (if (:complete todo) "✓" "✗")] ])

(defn renderButtons []
  [:div.buttons
   [:input {:type "button" :value "Add" :disabled (empty? (:nextTodo @model))
            :on-click #(addTodo)}]
   [:input {:type "button"
            :value "Remove All"
            :on-click #(removeAll)}] ] )

(defn page []
  [:div
   [:h1 "My To-do List"]
   [:form { :on-submit #(do (addTodo) (.preventDefault %))}
    [:input {:type "text"
             :value (:nextTodo @model)
             :placeholder "What do you have to do?"
             :on-change #(updateNextTodo (-> % .-target .-value))} ]]
    (renderButtons)
    [:div.todos (map-indexed renderTodo (:todos @model))]
    [:p (str @model)]])


;;everything below here is just boilerplate

  (defn dev-setup []
    (when ^boolean js/goog.DEBUG
      (enable-console-print!)
      (println "dev mode")
      ))

  (defn reload []
    (reagent/render [page model]
                    (.getElementById js/document "app")))

  (defn ^:export main []
    (dev-setup)
    (reload))
