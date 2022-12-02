(ns zen.fhir.writer
  (:require [clojure.string :as str]
            [zen.fhir.inter-utils]
            [cheshire.core :as json]
            [clojure.java.io]
            [clojure.pprint]
            [clojure.walk]
            [org.httpkit.client :as client]
            [zen.package]
            [ftr.core]
            [clojure.java.io :as io]))


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


(defn add-zen-quote-reader-tag [obj]
  (clojure.walk/postwalk (fn [node]
                           (if (and (symbol? node)
                                    (:zen/quote (meta node)))
                             (symbol (str "#zen/quote" " " (str node)))
                             node))
                         obj))


(defn format-zen-ns [zen-ns-map]
  (clojure.pprint/write (-> zen-ns-map
                            add-zen-quote-reader-tag
                            order-zen-ns)
                        :stream nil))


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
        package-resources (map :zen.fhir/resource (filter #(or (= package-ns (name (:zen.fhir/package-ns %)))
                                                               (contains? (:zen.fhir/packages %) (symbol package-ns))) resources))
        filename (str package-ns "-terminology-bundle")]
    (clojure.java.io/make-parents (str package-dir \/ filename))
    (spit-ndjson-gz-bundle! package-dir filename package-resources)))


(defn collect-packages ;; TODO: Shouldn't be a function, result should be stored in ztx
  "Finds all zen packages in ztx"
  [ztx & [{:as _opts, :keys [blacklisted-packages]
           :or {blacklisted-packages #{}}}]]
  (->> (vals (:fhir/inter @ztx))
       (mapcat vals)
       (keep #(some-> % :zen.fhir/package-ns name))
       (remove blacklisted-packages)
       set))


(defn spit-zen-modules [ztx zrc-dir & [package-name]]
  (let [packages (-> (cond->> (collect-packages ztx)
                       (some? package-name)
                       (filter #{(name package-name)})))]
    (doseq [package packages]
      (spit-zen-schemas ztx zrc-dir {:package package})
      (spit-terminology-bundle ztx zrc-dir {:package package}))
    :done))


(defn spit-zen-npm-modules [ztx zrc-node-modules-dir ver & [package-name zen-fhir-lib-version]]
  (let [packages (cond->> (collect-packages ztx)
                   (some? package-name)
                   (filter #{(name package-name)}))
        packages-deps (zen.fhir.inter-utils/packages-deps-nses (:fhir.zen/ns @ztx) (:fhir/inter @ztx))]
    (doseq [package packages
            :let [package-dir (str zrc-node-modules-dir \/ package \/)
                  package-file-path (str package-dir "/package.json")
                  package-deps (into {}
                                     (map #(-> {(str "@zen-lang/" %)
                                                (if (= "zen.fhir" (str %))
                                                  zen-fhir-lib-version
                                                  ver)}))
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
                               {:keys [out-dir git-url-format zen-fhir-lib-url git-auth-url-format node-modules-folder ftr-context]}
                               package]
  (let [package-dir (str out-dir \/ package \/)
        packages-deps (zen.fhir.inter-utils/packages-deps-nses (:fhir.zen/ns @ztx) (:fhir/inter @ztx))
        package-git-url (format git-url-format package)
        package-file-path (str package-dir "zen-package.edn")
        package-deps (into {'zen.fhir zen-fhir-lib-url}
                           (map (fn [dep] [(symbol dep) (format git-url-format dep)]))
                           (get packages-deps (symbol package)))
        package-file {:deps package-deps}
        package-git-auth-url (some-> git-auth-url-format (format package))]
    {:package package
     :package-dir package-dir
     :package-git-url package-git-url
     :package-file-path package-file-path
     :package-file package-file
     :package-git-auth-url (or package-git-auth-url
                               package-git-url)
     :out-dir out-dir
     :node-modules-folder node-modules-folder
     :ftr-context ftr-context}))


(defn clone-zen-package [{:as config
                          :keys [package-git-auth-url
                                 package-git-url
                                 package-dir]}]
  (assoc config :cloned?
         (zero? (:exit (zen.package/sh! "git"
                                        "clone"
                                        "--depth=1"
                                        (or package-git-auth-url
                                            package-git-url)
                                        package-dir)))))


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


(defn create-remote! [ztx {:as config :keys [cloned? package package-dir package-git-auth-url package-git-url]}]
  (if cloned?
    config
    (let [org-name (:org-name @ztx)
          token    (get-in @ztx [:env :github-token])
          {:keys [status body]} (create-repo! token org-name package)]
      (if (< status 300)
        (do (add-git-remote ztx package-dir (or package-git-auth-url package-git-url))
            config)
        (reduced (assoc config :error {:status status :body body}))))))


(defn init-zen-repo! [ztx {:as config, :keys [cloned? out-dir package package-dir]}]
  (if cloned?
    config
    (do
      (zen.package/mkdir! out-dir package)
      (zen.package/zen-init! package-dir)
      (zen.package/sh! "git" "add" "--all" :dir package-dir)
      (zen.package/sh! "git" "commit" "-m" "'Init commit'" :dir package-dir)
      (zen.package/sh! "git" "branch" "-M" "main" :dir package-dir)
      config)))


(defn clean-up-clonned-repo! [{:as config, :keys [package-dir]}]
  (when package-dir
    (zen.package/sh! "bash" "-c" "ls | grep -v ftr | xargs rm -rf" :dir package-dir))
  config)


(defn infer-node-modules-path [f package-name]
  (-> f
      (str/split #"/")
      (->>
        (take-while (complement #{package-name}))
        (str/join "/"))))


(defn file-exists? [path]
  (.exists (io/file path)))


(defn get-ftr-source-urls [{{:as _npm-package-meta,
                             npm-package-name :name
                             dependencies :dependencies} :package
                            f :file}]
  (let [node-modules-path (infer-node-modules-path f npm-package-name) #_"FIXME HACK fragile as fuck"
        package-url (str node-modules-path \/ npm-package-name)
        deps-urls (->> dependencies
                       keys
                       (map (comp (partial str node-modules-path \/) name))
                       (filter file-exists?))]
    (into [package-url] deps-urls)))


(defn produce-ftr-manifests [ztx {:as config,
                                  :keys [package]}]
  (let [inter-valuesets (get-in @ztx [:fhir/inter "ValueSet"])

        loader-meta
        (->> inter-valuesets
             (filter (fn [[_vs-url {:as _vs, :zen.fhir/keys [package-ns]}]]
                       (= (name package-ns) package)))
             first
             second
             :zen/loader)

        ftr-source-urls (get-ftr-source-urls loader-meta)

        ftr-manifest {:module      "ig"
                      :source-urls  ftr-source-urls
                      :source-type :igs
                      :ftr-path    "ftr"
                      :tag         "init"}]

    (swap! ztx update :fhir.zen/ns
           (fn [namespaces]
             (into {}
                   (map (fn [[zen-ns ns-content]]
                          (let [nss  (name zen-ns)
                                package-name (first (str/split nss #"\." 2))]
                            (if (and (= package package-name) (get ns-content 'value-set))
                              [zen-ns (assoc-in ns-content ['value-set :ftr] ftr-manifest)]
                              [zen-ns ns-content]))))
                   namespaces)))
    config))

(defn spit-ftr [ztx ftr-context package-dir package]
  (let [value-sets (->> (get-in @ztx [:fhir.zen/ns])
                        (filter (fn [[zen-ns ns-content]]
                                  (let [nss  (name zen-ns)
                                        package-name (first (str/split nss #"\." 2))]
                                    (and (= package package-name) (get ns-content 'value-set)))))
                        (map (fn [[_zen-ns ns-content]] (get ns-content 'value-set))))
        vs-urls (map :uri value-sets)
        ftr-configs (->> value-sets
                         (group-by :ftr)
                         keys
                         (filter identity))]
    (doseq [ftr ftr-configs]
      (ftr.core/spit-ftr (merge ftr-context {:cfg (assoc ftr
                                                         :ftr-path (str package-dir "/ftr")
                                                         :vs-urls vs-urls)})))))


(defn spit-data [ztx {:keys [package-dir package package-file-path package-file ftr-context] :as config}]
  (spit-zen-schemas ztx (str package-dir "/zrc") {:package package})
  (spit-ftr ztx ftr-context package-dir package)
  (spit package-file-path (with-out-str (clojure.pprint/pprint package-file)))
  config)


(defn commit-zen-changes [{:keys [package-dir] :as config}]
  (zen.package/sh! "git" "add" "--all" :dir package-dir)
  (zen.package/sh! "git" "commit" "-m" "'Update zen package'" :dir package-dir)
  config)


(defn release-zen-package [{:as config :keys [package-dir]}]
  (zen.package/sh! "git" "push" "-u" "origin" "main" :dir package-dir)
  config)


(defn rm-local-repo! [{:as config, :keys [out-dir package]}]
  (zen.package/sh! "rm" "-rf" package :dir out-dir)
  config)


(defn reduced-shortcircuit-xf []
  (fn [rf]
    (fn
      ([] (rf))
      ([result] (rf result))
      ([result input]
       (if (reduced? input)
         (reduced (rf result (unreduced input)))
         (rf result input))))))


(defn release-xform [ztx config]
  (let [xforms [(filter (partial filter-zen-packages ztx config))
                (map (partial generate-package-config ztx config))
                (map clone-zen-package)
                (map (partial init-zen-repo! ztx))
                (map (partial create-remote! ztx))
                (map clean-up-clonned-repo!)
                (map (partial produce-ftr-manifests ztx))
                (map (partial spit-data ztx))
                (map commit-zen-changes)
                (map release-zen-package)
                (map rm-local-repo!)]]
    (apply comp (interleave xforms
                            (repeat (reduced-shortcircuit-xf))
                            #_"NOTE: this interleave is ugly, maybe there's a better way to somehow break into/transduce?"))))


(defn release-packages [ztx config]
  (into
   []
   (release-xform ztx config)
   (collect-packages ztx config)))
