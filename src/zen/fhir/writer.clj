(ns zen.fhir.writer
  (:require [clojure.string :as str]
            [zen.fhir.inter-utils]
            [cheshire.core :as json]
            [clojure.java.io]
            [clojure.pprint]
            [clojure.walk]
            [zen.package]))


(defn order-zen-ns [zen-ns-map]
  (let [zen-ns             (get zen-ns-map 'ns)
        zen-import         (get zen-ns-map 'import)
        rest-zen-ns-map    (dissoc zen-ns-map 'ns 'import)
        ordered-zen-ns-map (cond->> (sort-by key rest-zen-ns-map)
                             (some? zen-import) (cons ['import zen-import])
                             (some? zen-ns)     (cons ['ns zen-ns])
                             :always            flatten
                             :always            (apply array-map))]
    ordered-zen-ns-map))


(defn format-zen-ns [zen-ns-map]
  (clojure.pprint/write (order-zen-ns zen-ns-map) :stream nil))


(defn spit-zen-schemas [ztx zrc-dir & [{:keys [package]}]]
  (doseq [[zen-ns ns-content] (get-in @ztx [:fhir.zen/ns])
          :let [nss  (name zen-ns)
                file (str zrc-dir "/" (str/replace nss #"\." "/") ".edn")
                package-name (first (str/split nss #"\." 2))]
          :when (or (nil? package) (= package package-name))]
    (clojure.java.io/make-parents file)
    (spit file (format-zen-ns ns-content)))
  :done)


(defn spit-ndjson-gz-bundle! [dir filename resources]
  (let [f    (clojure.java.io/file (str dir \/ filename ".ndjson.gz"))
        outs (java.util.zip.GZIPOutputStream. (clojure.java.io/output-stream f) true)]
    (with-open [w (java.io.BufferedWriter. (java.io.OutputStreamWriter. outs))]
      (doseq [resource resources]
        (.write w (cheshire.core/generate-string resource))
        (.write w "\n")
        (.flush w)))))


(defn spit-terminology-bundle [ztx package-dir {package-ns :package}]
  (let [fhir-inter (:fhir/inter @ztx)
        resources (->> (select-keys fhir-inter ["ValueSet" "Concept" "CodeSystem"])
                       vals
                       (mapcat vals))
        package-resources (map :zen.fhir/resource (filter #(= package-ns (name (:zen.fhir/package-ns %))) resources))
        filename (str package-ns "-terminology-bundle")]
    (clojure.java.io/make-parents (str package-dir \/ filename))
    (spit-ndjson-gz-bundle! package-dir filename package-resources)))


(defn collect-packages ;; TODO: Shouldn't be a function, result should be stored in ztx
  "Finds all zen packages in ztx"
  [ztx]
  (->> (vals (:fhir/inter @ztx))
       (mapcat vals)
       (keep #(some-> % :zen.fhir/package-ns name))
       set))


(defn spit-zen-modules [ztx zrc-dir & [package-name]]
  (let [packages (-> (cond->> (collect-packages ztx)
                       (some? package-name)
                       (filter #{(name package-name)})))]
    (doseq [package packages]
      (spit-zen-schemas ztx zrc-dir {:package package})
      (spit-terminology-bundle ztx zrc-dir {:package package}))
    :done))


(defn spit-zen-npm-modules [ztx zrc-node-modules-dir ver & [package-name]]
  (let [packages (cond->> (collect-packages ztx)
                   (some? package-name)
                   (filter #{(name package-name)}))
        packages-deps (zen.fhir.inter-utils/packages-deps-nses (:fhir/inter @ztx))]
    (doseq [package packages
            :let [package-dir (str zrc-node-modules-dir \/ package \/)
                  package-file-path (str package-dir "/package.json")
                  package-deps (into {}
                                     (map #(-> {(str "@zen-lang/" %) ver}))
                                     (get packages-deps (symbol package)))
                  package-file {:name    (str "@zen-lang/" package)
                                :version ver
                                :author  "Health-Samurai" ;; TODO: parameterize this
                                :license "MIT"
                                :dependencies package-deps}]]

      (spit-zen-schemas ztx package-dir {:package package})
      (spit-terminology-bundle ztx package-dir {:package package})
      (spit package-file-path (json/generate-string package-file {:pretty true})))
    :done))


(defn spit-zen-packages [ztx {:keys [out-dir package git-url-format]}]
  (let [packages (cond->> (collect-packages ztx)
                   (some? package)
                   (filter #{(name package)}))
        packages-deps (zen.fhir.inter-utils/packages-deps-nses (:fhir/inter @ztx))]
    (doseq [package packages
            :let [package-dir (str out-dir \/ package \/)
                  package-git-url (format git-url-format package)
                  package-file-path (str package-dir "/zen-package.edn")
                  package-deps (into {'zen.fhir (str out-dir "/zen.fhir")}
                                     (map (fn [dep] [(symbol dep) (format git-url-format dep)]))
                                     (get packages-deps (symbol package)))
                  package-file {:deps package-deps}]]

      (if (zero? (:exit (zen.package/sh! "git" "clone" package-git-url package-dir)))
        :clone
        (do
          (zen.package/mkdir! out-dir package)
          (zen.package/zen-init! package-git-url)
          (zen.package/sh! "git" "add" "--all" :dir package-dir)
          (zen.package/sh! "git" "commit" "-m" "'Init commit'" :dir package-dir)
          :init))

      (spit-zen-schemas ztx (str package-dir "/zrc") {:package package})
      (spit-terminology-bundle ztx package-dir {:package package})
      (spit package-file-path (with-out-str (clojure.pprint/pprint package-file)))

      (zen.package/sh! "git" "add" "--all" :dir package-dir)
      (zen.package/sh! "git" "commit" "-m" "'Update zen package'" :dir package-dir))
    :done))

