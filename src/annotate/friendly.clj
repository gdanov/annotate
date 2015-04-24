(ns annotate.friendly
  "Translate a map of errors returned by check into a map of friendly
  error message."
  (:use [annotate.types :only [key-not-found]]))

(defrecord Label [err-msg not-found-msg])

(defn label
  "Useful for validating form fields where you need to return a friendly
  error message."
  ([] (Label. "Invalid input" "Not found"))
  ([err-msg]
     (Label. err-msg "Not found"))
  ([err-msg not-found-msg]
     (Label. err-msg not-found-msg)))

(defn friendly
  "Takes an error map and a map of fields to labels. Replaces values in
  the error map with the appropriate labels."
  [errors labels]
  (when errors
    (if labels
      (reduce (fn [acc [k v]]
                (->> (if (map? v)
                       (friendly v (get labels k))
                       (if-let [label (get labels k)]
                         (if (= v key-not-found)
                           (.-not-found-msg label)
                           (.-err-msg label))
                         v))
                     (assoc acc k)))
              {}
              errors)
      errors)))

(ns-unmap *ns* '->Label)
(ns-unmap *ns* 'map->Label)
