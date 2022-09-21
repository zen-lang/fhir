(ns zen.fhir.tooling-packages
  (:gen-class)
  (:require [zen.core]
            [zen.fhir.loader]
            [zen.fhir.generator]
            [zen.fhir.writer]))


(defn -main [{:keys [node-modules-folder
                     out-dir
                     package-name
                     zen-fhir-lib-url
                     git-url-format]
              :or   {zen-fhir-lib-url "git@github.com:zen-fhir/zen.fhir.git"
                     git-url-format   "git@github.com:zen-fhir/%s.git"}}]
  (let [github-token (System/getenv "ZEN_FHIR_RELEASE_GITHUB_TOKEN")
        org-name     (System/getenv "ZEN_FHIR_RELEASE_GITHUB_ORG")
        ztx (zen.core/new-context {:env {:github-token github-token}
                                   :org-name org-name})]
    (zen.fhir.loader/load-all ztx nil {:node-modules-folder node-modules-folder})
    (zen.fhir.generator/generate-zen-schemas ztx)
    (zen.fhir.writer/release-packages ztx {:out-dir          out-dir
                                           :package          package-name
                                           :git-url-format   git-url-format
                                           :zen-fhir-lib-url zen-fhir-lib-url})
    (prn :done)))
