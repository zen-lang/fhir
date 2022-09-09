(ns ftr.writer.core
  (:require [ftr.writer.flat-table]))


(defmulti write-terminology-file (fn [{:as _ctx, {:keys [source-type]} :cfg}]
                                   source-type))
