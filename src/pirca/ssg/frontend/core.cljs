(ns pirca.ssg.frontend.core
  (:require
   [pirca.ssg.frontend.repo :as repo]))

(defn ^:dev/after-load mount-root
  []
  (repo/mount-root))
