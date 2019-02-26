(ns cljds.avl
  (:require [cljds.core :as core]))

; helper functions to proxy into the core namespace
; for usage in tests. The avl implementation will
; end up here.
(def height core/height)
(def seq->avl core/seq->avl)
(def contains-element? core/contains-element?)
