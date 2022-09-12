(ns ftr.writer.core
  (:require [ftr.writer.flat-table]))


(defmulti write-terminology-file (fn [{:as _ctx, {:keys [source-type]} :cfg}]
                                   source-type))


(defmethod ftr.writer.core/write-terminology-file :flat-table
  [ctx]
  (ftr.writer.flat-table/write-terminology-file ctx))
