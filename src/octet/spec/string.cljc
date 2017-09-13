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

