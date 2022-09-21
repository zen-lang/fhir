(ns zen.fhir.writer
  (:require [clojure.string :as str]
            [zen.fhir.inter-utils]
            [cheshire.core :as json]
            [clojure.java.io]
            [clojure.pprint]
            [clojure.walk]
            [org.httpkit.client :as client]
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


(defn filter-zen-packages [ztx {:keys [package] :as config} package-name]
  (or (nil? package)
      (= (name package) package-name)))


(defn generate-package-config [ztx
                               {:keys [out-dir git-url-format zen-fhir-lib-url git-release-url-format]}
                               package]
  (let [package-dir (str out-dir \/ package \/)
        packages-deps (zen.fhir.inter-utils/packages-deps-nses (:fhir/inter @ztx))
        package-git-url (format git-url-format package)
        package-file-path (str package-dir "zen-package.edn")
        package-deps (into {'zen.fhir zen-fhir-lib-url}
                           (map (fn [dep] [(symbol dep) (format git-url-format dep)]))
                           (get packages-deps (symbol package)))
        package-file {:deps package-deps}
        git-release-url-format (some-> git-release-url-format (format package))]
    {:package package
     :package-dir package-dir
     :package-git-url package-git-url
     :package-file-path package-file-path
     :package-file package-file
     :git-release-url-format git-release-url-format
     :out-dir out-dir}))


(defn clone-zen-package [{:keys [package-git-url package-dir] :as config}]
  (assoc config :cloned?
         (zero? (:exit (zen.package/sh! "git" "clone" package-git-url package-dir)))))


(defn create-repo! [token org-name repo-name]
  @(client/post
     (str "https://api.github.com/orgs/" org-name "/repos")
     {:headers
      {"Authorization" (str "token " token)}
      :body
      (json/encode {:name repo-name
                    :private false
                    :visibility "public"})}))


(defn add-git-remote [ztx dir remote-url]
  (zen.package/sh! "git" "remote" "add" "origin" remote-url :dir dir))


(defn create-remote! [ztx {:as config :keys [cloned? package package-dir]}]
  (if cloned?
    config
    (let [org-name (:org-name @ztx)
          token    (get-in @ztx [:env :github-token])
          {:keys [status body]} (create-repo! token org-name package)]
      (if (< status 300)
        (do (add-git-remote ztx package-dir (str "https://github.com/" org-name \/ package))
            config)
        (reduced (assoc config :error {:status status :body body}))))))


(defn init-zen-repo! [ztx {:as config, :keys [cloned? out-dir package package-git-url package-dir]}]
  (if cloned?
    config
    (do
      (zen.package/mkdir! out-dir package)
      (zen.package/zen-init! package-dir)
      (zen.package/sh! "git" "add" "--all" :dir package-dir)
      (zen.package/sh! "git" "commit" "-m" "'Init commit'" :dir package-dir)
      (zen.package/sh! "git" "branch" "-M" "main")
      config)))


(defn spit-data [ztx {:keys [package-dir package package-file-path package-file] :as config}]
  (spit-zen-schemas ztx (str package-dir "/zrc") {:package package})
  (spit-terminology-bundle ztx package-dir {:package package})
  (spit package-file-path (with-out-str (clojure.pprint/pprint package-file)))
  config)


(defn commit-zen-changes [{:keys [package-dir] :as config}]
  (zen.package/sh! "git" "add" "--all" :dir package-dir)
  (zen.package/sh! "git" "commit" "-m" "'Update zen package'" :dir package-dir)
  config)


(defn release-zen-package [{:as config :keys [package-dir]}]
  (zen.package/sh! "git" "push" "-u" "origin" "main" :dir package-dir)
  config)


(defn release-xform [ztx config]
  (comp
   (filter (partial filter-zen-packages ztx config))
   (map (partial generate-package-config ztx config))
   (map clone-zen-package)
   (map (partial init-zen-repo! ztx))
   (map (partial create-remote! ztx))
   (map (partial spit-data ztx))
   (map commit-zen-changes)
   (map release-zen-package)))


(defn release-packages [ztx config]
  (into
   []
   (release-xform ztx config)
   (collect-packages ztx)))
