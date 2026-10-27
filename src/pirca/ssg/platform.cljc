(ns pirca.ssg.platform
  "Small platform-specific helpers that have no portable Clojure API."
  (:require
   [clojure.string :as str])
  #?(:clj (:import [java.io File]
                   [java.security MessageDigest])))

(defn md5
  "Return the lower-case hex MD5 digest of a file's contents."
  [file]
  #?(:clj  (let [digest (MessageDigest/getInstance "MD5")
                 bytes  (.readAllBytes (java.io.FileInputStream.
                                        (File. (str file))))]
             (str/join (map #(format "%02x" (bit-and % 0xff))
                            (.digest digest bytes))))
     :cljs (let [crypto (js/require "crypto")
                 fs     (js/require "fs")
                 buf    (.readFileSync fs (str file))]
             (-> (.createHash crypto "md5")
                 (.update buf)
                 (.digest "hex")))))
