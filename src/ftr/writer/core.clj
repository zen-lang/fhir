(ns ftr.writer.core)


(defmulti write-terminology-file (fn [{:as _ctx, {:keys [source-type]} :cfg}]
                                   source-type))
