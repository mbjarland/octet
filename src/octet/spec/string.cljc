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

(ns octet.spec.string
  (:require [octet.buffer :as buffer]
            [octet.spec :as spec]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn zeropad-count
  "Given a byte array, returns a number of bytes
  allocated with zero padding (zero byte)."
  [input]
  (let [mark (byte 0)]
    (reduce (fn [sum index]
              (let [value (aget input index)]
                (if (= value mark)
                  (inc sum)
                  (reduced sum))))
            0
            (reverse (range (count input))))))

;; Clojure Helpers

#?(:clj
   (do
     (defn zeropad!
       "Add zero byte padding to the given byte array
       to the remaining bytes after specified data length."
       [^bytes input ^long datalength]
       (java.util.Arrays/fill input datalength (count input) (byte 0)))

     (defn bytes->string
       [^bytes input ^long length]
       (String. input 0 length "UTF-8"))

     (defn string->bytes
       [^String value]
       (.getBytes value "UTF-8"))

     (defn arraycopy
       [^bytes input ^bytes output ^long length]
       (System/arraycopy input 0 output 0 length)))

   :cljs
   (do
     (defn zeropad!
       [^bytes input ^number datalength]
       (doseq [^number i (range (.-length input))]
         (when (> i datalength)
           (aset input i 0))))

     (defn bytes->string
       [input length]
       (let [view (.subarray input 0 length)
             view (js/Uint8Array. view)
             fcc (.-fromCharCode js/String)]
         (.apply fcc nil view)))

     (defn string->bytes
       [value]
       (let [buff (js/ArrayBuffer. (count value))
             view (js/Uint8Array. buff)]
         (doseq [i (range (count value))]
           (aset view i (.charCodeAt value i)))
         (js/Int8Array. buff)))

     (defn arraycopy
       [^bytes input ^bytes output ^long length]
       (reduce (fn [_ i]
                 (aset output i (aget input i)))
               nil
               (range (.-length input))))

     (defn byte-array
       [length]
       (js/Int8Array. length))

     (extend-type js/Int8Array
       ICounted
       (-count [s]
         (.-length s)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Type Spec implementation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn string
  "Fixed length string type spec constructor."
  [size]
  (reify
    spec/ISpecSize
    (size [_] size)

    spec/ISpec
    (read [_ buff pos]
      (let [rawdata (buffer/read-bytes buff pos size)
            length  (- size (zeropad-count rawdata))
            data (bytes->string rawdata length)]
        [size data]))

    (write [_ buff pos value]
      (let [input (string->bytes value)
            length (count input)
            tmpbuf (byte-array size)]
        (if (< length size)
          (arraycopy input tmpbuf length)
          (arraycopy input tmpbuf size))

        (when (< length size)
          (zeropad! tmpbuf length))

        (buffer/write-bytes buff pos size tmpbuf)
        size))))

(def ^{:doc "Arbitrary length string type spec."}
  string*
  (reify
    #?@(:clj
        [clojure.lang.IFn
         (invoke [s] s)]
        :cljs
        [cljs.core/IFn
         (-invoke [s] s)])

    spec/ISpecDynamicSize
    (size* [_ data]
      (let [data (string->bytes data)]
        (+ 4 (count data))))

    spec/ISpec
    (read [_ buff pos]
      (let [datasize (buffer/read-int buff pos)
            data (buffer/read-bytes buff (+ pos 4) datasize)
            data (bytes->string data datasize)]
        [(+ datasize 4) data]))

    (write [_ buff pos value]
      (let [input (string->bytes value)
            length (count input)]
        (buffer/write-int buff pos length)
        (buffer/write-bytes buff (+ pos 4) length input)
        (+ length 4)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Spec types for arbitrary length byte arrays/strings with a length reference
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- ref-size [type data]
  (if (satisfies? spec/ISpecSize type)
    (.size type)
    (.size* type data)))

(defn- ref-len-offset-map [ref-kw types data]
  (reduce-kv
    (fn [acc kw type]
      (if (= ref-kw kw)
        (reduced acc)
        (+ acc (ref-size type (kw data)))))
    0
    types))

(defn- ref-len-offset-vec [ref-index types data]
  (reduce
    (fn [acc index]
      (if (= ref-index index)
        (reduced acc)
        (+ acc (ref-size (get types index) (get data index)))))
    0
    types))

(defn- ref-len-offset [ref-kw-or-index types data]
  (cond (map? types) (ref-len-offset-map ref-kw-or-index types data)
        (vector? types) (ref-len-offset-vec ref-kw-or-index types data)
        :else (throw (ex-info "invalid type structure, not map nor vector"
                              {:ref-kw-or-index ref-kw-or-index
                               :type-structure  types
                               :data            data}))))

(defn- ref-write* [ref-kw-or-index buff pos value types data]
  (let [input      (if (string? value) (string->bytes value) value)
        length     (count input)
        len-offset (ref-len-offset ref-kw-or-index types data)
        len-type   (if (map? types) (ref-kw-or-index types) (get types ref-kw-or-index))]
    (.write len-type buff len-offset length)
    (buffer/write-bytes buff pos length value)
    (+ length)))

(defn- ref-read* [ref-kw-or-index buff pos parent]
  (let [datasize (cond (map? parent) (ref-kw-or-index parent)
                       (vector? parent) (get parent ref-kw-or-index)
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
      (let [data (string->bytes data)]
        (count data)))

    spec/ISpecWithRef
    (read* [_ buff pos parent]
      (let [[datasize bytes] (ref-read* ref-kw-or-index buff pos parent)]
        [datasize (bytes->string bytes datasize)]))

    (write* [_ buff pos value types data]
      (ref-write* ref-kw-or-index buff pos value types data))))
