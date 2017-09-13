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

(ns octet.util
  (:require [clojure.string :as str :refer [join ]])
  (:import [java.util.Arrays]))

(defmacro defalias
  [sym sym2]
  `(do
     (def ~sym ~sym2)
     (alter-meta! (var ~sym) merge (dissoc (meta (var ~sym2)) :name))))

(defn assoc-ordered [a-map key val & rest]
  "assoc into an array-map, keeping insertion order. The normal cloure
  assoc function switches to hash maps on maps > size 10 and loses insertion order"
  (let [kvs (interleave (concat (keys a-map) (list key))
                        (concat (vals a-map) (list val)))
        ret (apply array-map kvs)]
    (if rest
      (if (next rest)
        (recur ret (first rest) (second rest) (nnext rest))
        (throw (IllegalArgumentException.
                 "assoc-ordered expects even number of arguments after map/vector, found odd number")))
      ret)))

 ;
 ;          0    2    4    6     8    A    C    E
 ;00000000: 4869 6572 2069 7374  2065 696e 2042 6569  Hier ist ein Bei
 ;00000010: 7370 6965 6c74 6578  742e 2044 6572 2048  spieltext. Der H
 ;00000020: 6578 6475 6d70 2069  7374 2061 7566 2064  exdump ist auf d
 ;00000030: 6572 206c 696e 6b65  6e20 5365 6974 6520  er linken Seite
 ;00000040: 7a75 2073 6568 656e  2e20 4e65 7565 205a  zu sehen. Neue Z
 ;00000050: 6569 6c65 6e20 6f64  6572 2041 6273 c3a4  eilen oder Abs..
 ;00000060: 747a 6520 7369 6e64  2064 616e 6e20 6175  tze sind dann au
 ;00000070: 6368 2022 5a65 6963  6865 6e22 206d 6974  ch "Zeichen" mit
 ;00000080: 2065 696e 656d 2062  6573 7469 6d6d 7465   einem bestimmte
 ;00000090: 6e20 436f 6465 2028  3061 2900 0000 0000  n Code (0a).....




;;
;; Hexdumps
;;

(def j #(clojure.string/join "" %))
(def jnl #(clojure.string/join \newline %))

; formerly hexdump-bytes
(defn- bytes->hex [^bytes bytes]
  "converts a byte array to an ascii hex string"
  (let [[f & r] bytes
        fh (fn [_ b]
             (let [h (Integer/toHexString (bit-and b 0xFF))]
               (if (<= 0 b 15) (str "0" h) h)))]
    (join (reductions fh (fh 0 f) r))))

(defn- byte->ascii [byte]
  "convert a byte to 'printable' ascii where possible"
  (if (<= 32 (bit-and byte 0xFF) 127) (char byte) \.))

(defn- bytes->ascii [^bytes bytes]
  "returns a 16-per-line printable ascii view of the bytes"
  (->> bytes
       (map byte->ascii)
       (partition 16 16 "                ")
       (map join)))

(defn- to-ascii
  "Converts character to it's ascii representation"
  [c]
  (let [c (mod c 256)]
    (if (<= 0x1f c 0x7f)
      (char c)
      \.)))

(defn- chardump-bytes
  "Returns chardump of bytes"
  [bytes]
  (->> bytes
       (map to-ascii)
       j
       (partition-all 16)
       (map #(format "%-16s" (j %)))))

(defn- format-hex-line [^String hex-line]
  (->> hex-line
       (partition-all 4)
       (map join)
       (split-at 4)
       (map #(join " " %))
       (join "  ")))

(defn- bytes->hexdump [^bytes bytes]
  (->> bytes
       bytes->hex
       ;(partition-all 32) ;partition with pad?
       (partition 32 32 (join (repeat 32 " ")))
       (map format-hex-line)))

(defn- copy-bytes [bytes offset size]
  (let [size (if (nil? size) (alength bytes) size)]
    (if (and (= 0 offset) (= (alength bytes) size))
      bytes                                                 ; short circuit
      (java.util.Arrays/copyOfRange bytes
                                    offset
                                  (+ offset size)))))

(defn get-dump-bytes [x offset size]
  (cond (and (satisfies? octet.buffer/IBufferBytes x)
             (satisfies? octet.buffer/IBufferLimit x))
        (let [size (if (nil? size) (octet.buffer/get-capacity x) size)]
          (octet.buffer/read-bytes x offset size))

        (instance? (type (byte-array 0)) x)
        (copy-bytes x offset size)

        (instance? String x)
        (copy-bytes (.getBytes x) offset size)))

(defn hex-dump
  "Create hex dump. Accepts byte array, java.nio.ByteBuffer,
  io.netty.buffer.ByteBuf, or String as first argument. Offset will
  start the dump from an offset in the byte array, size will limit
  the number of bytes dumped, and frames will print a frame around
  the dump if true. Set print to true to print the dump on stdout
  (default) or false to return it as a string. Example call:
  (hex-dump (byte-array (range 200)) :print false)"
  [x & {:keys [offset size print frame]
                   :or   {offset 0
                          print  true
                          frame false}}]
  {:pre [(not (nil? x))]}
  (let [bytes (get-dump-bytes x offset size)
        size (if (nil? size) (alength bytes) size)
        dump (bytes->hexdump bytes)
        ascii (bytes->ascii bytes)
        offs (map #(format "%08x" %)
                  (range offset (+ offset size 16) 16))
        header (str " " (join (repeat 68 "-")))
        border (if frame "|" "")
        lines (map #(str border %1 ": " %2 "  " %3 border) offs dump ascii)
        lines (if frame (concat [header] lines [header]) lines)
        result (join \newline lines)]
    (if print (println result) result)))




