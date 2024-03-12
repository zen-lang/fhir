(ns zen.fhir.tooling-packages
  (:gen-class)
  (:require [zen.core]
            [zen.fhir.loader]
            [zen.fhir.generator]
            [zen.fhir.writer]
            [clojure.string]
            [zen.package]
            [ftr.core]
            [zen.fhir.inter-utils]
            [clojure.java.shell]))


(defn github-release-zen-packages [{:keys [node-modules-folder out-dir package-name zen-fhir-lib-url git-url-format git-auth-url-format
                                           blacklisted-packages remote-repo-url produce-remote-ftr-manifests?
                                           blacklist]}]
  (let [github-user  (System/getenv "ZEN_FHIR_RELEASE_GITHUB_USER")
        github-token (System/getenv "ZEN_FHIR_RELEASE_GITHUB_TOKEN")
        org-name     (System/getenv "ZEN_FHIR_RELEASE_GITHUB_ORG")

        zen-fhir-lib-url     (or zen-fhir-lib-url    "https://github.com/zen-fhir/zen.fhir.git")
        git-url-format       (or git-url-format      "https://github.com/zen-fhir/%s.git")
        git-auth-url-format  (or git-auth-url-format (str "https://" github-user ":" github-token "@github.com/zen-fhir/%s.git"))

        ztx (zen.core/new-context {:env {:github-token github-token}
                                   :org-name org-name})
        _ (zen.fhir.loader/load-all ztx nil (cond-> {:node-modules-folder node-modules-folder
                                                     :skip-concept-processing true}
                                              blacklist
                                              (assoc :blacklist blacklist)))
        ftr-build-deps-coords (zen.fhir.inter-utils/build-ftr-deps-coords {:node-modules-folder node-modules-folder})

        ftr-context (ftr.core/extract {:cfg
                                       {:module      "ig"
                                        :source-url  node-modules-folder
                                        :source-type :igs
                                        :ftr-path    "ftr"
                                        :tag         "init"
                                        :extractor-options {:supplements
                                                            (vals zen.fhir.inter-utils/possible-dep-coords*)}}})

        _ (zen.package/sh! "rm" "-rf" node-modules-folder)
        _ (zen.fhir.generator/generate-zen-schemas ztx)
        release-result (zen.fhir.writer/release-packages ztx {:ftr-context                   ftr-context
                                                              :out-dir                       out-dir
                                                              :package                       package-name
                                                              :git-url-format                git-url-format
                                                              :git-auth-url-format           git-auth-url-format
                                                              :zen-fhir-lib-url              zen-fhir-lib-url
                                                              :blacklisted-packages          blacklisted-packages
                                                              :node-modules-folder           node-modules-folder
                                                              :remote-repo-url               remote-repo-url
                                                              :produce-remote-ftr-manifests? produce-remote-ftr-manifests?
                                                              :ftr-build-deps-coords         ftr-build-deps-coords})]
    (if-let [error (:error (last release-result))]
      (do (throw (ex-info "Release error" {:error error}))
          (println (format "RELEASE ERROR:\n%s" error)))
      (->> (map :package-git-url release-result)
           (clojure.string/join "\n")))))


(defn build-zen-packages [{:keys [node-modules-folder out-dir main-package]}]
  (let [zen-fhir-lib-url     "https://github.com/zen-fhir/zen.fhir.git"
        git-url-format       "https://github.com/zen-fhir/%s.git"

        ztx (zen.core/new-context {})
        _ (zen.fhir.loader/load-all ztx nil {:node-modules-folder node-modules-folder
                                             :skip-concept-processing true})

        ftr-build-deps-coords (zen.fhir.inter-utils/build-ftr-deps-coords {:node-modules-folder node-modules-folder})

        _ (zen.fhir.writer/show-success-message "Build FTR context")
        ftr-context (ftr.core/extract {:cfg
                                       {:module      "ig"
                                        :source-url  node-modules-folder
                                        :source-type :igs
                                        :ftr-path    "ftr"
                                        :tag         "init"
                                        :extractor-options {:supplements
                                                            (vals zen.fhir.inter-utils/possible-dep-coords*)}}})

        
        _ (zen.fhir.generator/generate-zen-schemas ztx)
        release-result (zen.fhir.writer/release-packages-cli ztx {:main-package          (some-> main-package (clojure.string/replace #"\." "-"))
                                                                  :ftr-context           ftr-context
                                                                  :out-dir               out-dir
                                                                  :git-url-format        git-url-format
                                                                  :zen-fhir-lib-url      zen-fhir-lib-url
                                                                  :node-modules-folder   node-modules-folder
                                                                  :ftr-build-deps-coords ftr-build-deps-coords})]
    (if-let [error (:error (last release-result))]
      (throw (ex-info "Release error" {:error error}))
      "Package was succesfully builded")))


(defn build
  [node-modules-folder out-dir main-package]
  (build-zen-packages
    {:node-modules-folder  node-modules-folder
     :out-dir              out-dir
     :main-package         main-package}))


(defn -main [return-path node-modules-folder out-dir & [zen-fhir-lib-url git-url-format git-auth-url-format package-name blacklist]]
  (spit return-path
        (github-release-zen-packages
          {:node-modules-folder  node-modules-folder
           :out-dir              out-dir
           :package-name         (when (not= package-name "nil") package-name)
           :zen-fhir-lib-url     (when (not= zen-fhir-lib-url "nil") zen-fhir-lib-url)
           :git-auth-url-format  (when (not= git-auth-url-format "nil") git-auth-url-format)
           :git-url-format       (when (not= git-url-format "nil") git-url-format)
           :remote-repo-url      "https://storage.googleapis.com"
           :produce-remote-ftr-manifests? true
           :blacklist (when blacklist (read-string (slurp blacklist)))
           :blacklisted-packages #{}})
        :append true))


(comment

  (-main "release.txt" "node_modules" "/tmp/output")



  )
