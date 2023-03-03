(ns zen.fhir-light.loader.group
  (:require [zen.fhir-light.loader.parse-id :as loader.parse-id]
            [zen.fhir.utils :as utils]))


(def strdef-keys-types
  {:zf/meta #{:resourceType :url
              :type :kind :derivation :abstract
              :experimental :status
              :fhirVersion  :version :date  :context :contextInvariant}

   :zf/description #{:name :title :description :purpose :useContext
                     :publisher :contact :jurisdiction :copyright
                     :keyword :identifier :mapping}})


(def elements-keys-types
  {:zf/loc          #{:id :path}

   :zf/value        #{:type :binding :contentReference :maxLength :base}
   :zf/container    #{:max :min :sliceIsConstraining :sliceName :slicing :base}
   :zf/outer        #{:max :min :condition :base}
   :zf/context      #{:constraint}

   :zf/meta         #{:isModifier :isSummary :mustSupport :representation}
   :zf/description  #{:alias :code :comment :definition :example :isModifierReason
                      :label :mapping :orderMeaning :requirements :short}})


(def elements-poly-keys-types
  {:zf/value #{:fixed :maxValue :minValue :pattern}
   :zf/meta  #{:default}})


(defn group-keys [element keys-types poly-keys-types]
  (utils/strip-when
    empty?
    (merge-with merge
                (update-vals keys-types
                             #(select-keys element %))
                (update-vals poly-keys-types
                             #(into {}
                                    (mapcat (fn [pk] (utils/poly-find-all element pk)))
                                    %)))))


(defn enrich-loc [grouped-element]
  (let [id        (get-in grouped-element [:zf/loc :id])
        parsed-id (loader.parse-id/parse-id id)]
    (assoc-in grouped-element [:zf/loc :zf/id] parsed-id)))
