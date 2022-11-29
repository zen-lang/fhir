(ns zen.fhir.inter-utils
  (:require [com.rpl.specter :as sp]
            [clojure.string :as str]))


(defn packages-deps-nses [zen-nses fhir-inter]
  (let [packages (into #{}
                       (sp/select [sp/MAP-VALS sp/MAP-VALS :zen.fhir/package-ns]
                                  fhir-inter))

        namespaces&deps (update-vals zen-nses #(some % [:import 'import]))

        ns-package-fn (fn [zen-ns]
                        (or (->> packages
                                 (filter #(str/starts-with? (name zen-ns)
                                                            (name %)))
                                 first)
                            zen-ns))

        package-namespaces&deps (update-vals (group-by (comp ns-package-fn key) namespaces&deps)
                                             #(into {} %))

        package-namespaces&package-deps
        (update-vals
          package-namespaces&deps
          (fn [package-ns]
            (update-vals package-ns (fn [ns-deps] (into #{} (map ns-package-fn) ns-deps)))))

        packages-deps
        (into {}
              (map (fn [[package namespaces&package-deps]]
                     [package
                      (disj (into #{} cat (vals namespaces&package-deps))
                            package)]))
              package-namespaces&package-deps)]

    packages-deps))
