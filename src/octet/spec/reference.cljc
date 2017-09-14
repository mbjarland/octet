;; Copyright (c) 2015-2016 Andrey Antukh <niwi@niwi.nz>
;; All rights reserved.
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are met:
;;
;; * Redistributions of source code must retain the above copyright notice, this
;;   list of conditions and the following disclaimer.
;;
;; * Redistributions in binary form must reproduce the above copyright notice,
;;   this list of conditions and the following disclaimer in the documentation
;;   and/or other materials provided with the distribution.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
;; AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
;; DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
;; FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
;; SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
;; CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
;; OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
;; OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(ns octet.spec.reference
  (:require [octet.buffer :as buffer]
            [octet.spec :as spec]
            [octet.spec.string :as string-spec]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Spec types for arbitrary length byte arrays/strings with a length reference
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- ref-size [type data]
  (if (satisfies? spec/ISpecSize type)
    (.size type)
    (.size* type data)))

(defn- coerce-types [types]
  "to make it terser to handle both maps (assoc spec) and seq/vector
  (indexed spec), we coerce the seq/vector types to maps where the
  keys are indexes"
  (cond (map? types) types
        (or (seq? types)
            (vector? types)) (apply array-map (interleave (range) types))
        :else (throw (ex-info "invalid type structure, not map, seq or vector"
                              {:type-structure types}))))

(defn- ref-len-offset [ref-kw-or-index types data]
  (reduce-kv
    (fn [acc kw-or-index type]
      (if (= ref-kw-or-index kw-or-index)
        (reduced acc)
        (+ acc (ref-size type (get data kw-or-index)))))
    0
    types))

(defn- ref-write* [ref-kw-or-index buff pos value types data]
  (let [input      (if (string? value) (string-spec/string->bytes value) value)
        length     (count input)
        types      (coerce-types types)
        len-offset (ref-len-offset ref-kw-or-index types data)
        len-type   (get types ref-kw-or-index)]
    (.write len-type buff len-offset length)
    (buffer/write-bytes buff pos length input)
    (+ length)))

(defn- ref-read* [ref-kw-or-index buff pos parent]
  (let [datasize (cond (map? parent) (ref-kw-or-index parent)
                       (or (seq? parent) (vector? parent))
                       (get parent ref-kw-or-index)
                       :else (throw (ex-info
                                      (str "bad ref-string*/ref-bytes* length reference  - " ref-kw-or-index)
                                      {:length-kw ref-kw-or-index
                                       :data-read parent})))
        data     (buffer/read-bytes buff pos datasize)]
    [datasize data]))

(defn ref-bytes*
  [ref-kw-or-index]
  (reify
    #?@(:clj
        [clojure.lang.IFn
         (invoke [s] s)]
        :cljs
        [cljs.core/IFn
         (-invoke [s] s)])

    spec/ISpecDynamicSize
    (size* [_ data]
      (count data))

    spec/ISpecWithRef
    (read* [_ buff pos parent]
      (ref-read* ref-kw-or-index buff pos parent))

    (write* [_ buff pos value types data]
      (ref-write* ref-kw-or-index buff pos value types data))))

(defn ref-string*
  [ref-kw-or-index]
  (reify
    #?@(:clj
        [clojure.lang.IFn
         (invoke [s] s)]
        :cljs
        [cljs.core/IFn
         (-invoke [s] s)])

    spec/ISpecDynamicSize
    (size* [_ data]
      (let [data (string-spec/string->bytes data)]
        (count data)))

    spec/ISpecWithRef
    (read* [_ buff pos parent]
      (let [[datasize bytes] (ref-read* ref-kw-or-index buff pos parent)]
        [datasize (string-spec/bytes->string bytes datasize)]))

    (write* [_ buff pos value types data]
      (ref-write* ref-kw-or-index buff pos value types data))))

(defn ref-vector*
  [ref-kw-or-index]
  ;; TODO: implement this
  )
