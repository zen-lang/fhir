(ns zen.fhir.tooling-packages
  (:gen-class)
  (:require [zen.core]
            [zen.fhir.loader]
            [zen.fhir.generator]
            [zen.fhir.writer]
            [clojure.string]))


(defn github-release-zen-packages [{:keys [node-modules-folder out-dir package-name zen-fhir-lib-url git-url-format]}]
  (let [github-user  (System/getenv "ZEN_FHIR_RELEASE_GITHUB_USER")
        github-token (System/getenv "ZEN_FHIR_RELEASE_GITHUB_TOKEN")
        org-name     (System/getenv "ZEN_FHIR_RELEASE_GITHUB_ORG")

        zen-fhir-lib-url (or zen-fhir-lib-url "https://github.com/zen-fhir/zen.fhir.git")
        git-url-format   (or git-url-format (str "https://" github-user ":" github-token "@github.com/zen-fhir/%s.git"))

        ztx (zen.core/new-context {:env {:github-token github-token}
                                   :org-name org-name})
        _ (zen.fhir.loader/load-all ztx nil {:node-modules-folder node-modules-folder})
        _ (zen.fhir.generator/generate-zen-schemas ztx)
        release-result (zen.fhir.writer/release-packages ztx {:out-dir          out-dir
                                                              :package          package-name
                                                              :git-url-format   git-url-format
                                                              :zen-fhir-lib-url zen-fhir-lib-url})]
    (->> release-result
         (mapv :package-git-url)
         (clojure.string/join " "))))


(defn -main [return-path node-modules-folder out-dir & [zen-fhir-lib-url git-url-format package-name]]
  (spit return-path
        (github-release-zen-packages
          {:node-modules-folder node-modules-folder
           :out-dir             out-dir
           :package-name        package-name
           :zen-fhir-lib-url    zen-fhir-lib-url
           :git-url-format      git-url-format})))
