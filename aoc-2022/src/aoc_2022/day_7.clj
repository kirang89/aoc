(ns aoc-2022.day-7)

;;
;; Note: Think of a cleaner alternative solution
;;

(defonce datastream
  (aoc-2022.core/load-input "day_7_input.txt"))

(def cd-cmd-re (re-pattern #"\$ cd (.*)"))
(def ls-cmd-re (re-pattern #"\$ ls"))
(def file-listing-re (re-pattern #"(\d+) (.*)"))
(def dir-listing-re (re-pattern #"dir (.*)"))
(def parent-directory "..")
(def root-directory "/")


(defn parse-command [ln]
  (cond
    (re-matches cd-cmd-re ln) {:type :cd
                               :dir (last (re-matches cd-cmd-re ln))}
    (re-matches ls-cmd-re ln) {:type :ls}))


(defn parse-listing [s]
  (let [file-match (re-matches file-listing-re s)
        dir-match  (re-matches dir-listing-re s)]
    (cond
      file-match
      {:type :file
       :name (get file-match 2)
       :size (Integer/parseInt (get file-match 1))}

      dir-match
      {:type :dir :name (get dir-match 1)}

      :default
      nil)))

(defn parse-line [l]
  (if-let [cmd (parse-command l)]
    {:type :command :data cmd}
    (if-let [listing (parse-listing l)]
      {:type :listing :data listing})))

(defn process-input [{:keys [tree wd] :as acc} line]
  (let [{:keys [type data]} (parse-line line)]
    (cond
      (and (= :command type)
           (= :cd (:type data))
           (= parent-directory (:dir data)))
      (update-in acc [:wd] #(vec (drop-last 1 %)))

      (and (= :command type)
           (= :cd (:type data))
           (= root-directory (:dir data)))
      (-> acc
          (update-in [:wd] #(conj % (:dir data)))
          (assoc :tree {"/" {}}))

      (and (= :command type) (= :cd (:type data)))
      (-> acc
          (update-in [:wd] #(conj % (:dir data)))
          (update-in (into [:tree] wd)
                     #(merge-with conj % {(:dir data) {}})))

      (and (= :listing type) (= :file (:type data)))
      (update-in acc
                 (into [:tree] wd)
                 #(merge-with conj % {(:name data) (:size data)}))

      :default
      acc)))

(defn build-tree [datastream]
  (->> datastream
       (reduce process-input {:tree {} :wd []})
       :tree))

(defn total-dir-size [dir-size-map subdirs]
  (->> dir-size-map
       flatten
       (partition 2)
       (filter (fn [[d _]] (some #{d} subdirs)))
       (map second)
       (reduce +)))

(defn dir-size-distribution
  ([tree]
   (dir-size-distribution (get tree "/") "/"))

  ([tree dir]
   (let [size-sum (fn [t] (->> t flatten (partition 2) (map second) (reduce +)))
         files    (->> tree
                       (filter (fn [[_ v]] (number? v))))
         subdirs  (filter (fn [[_ v]] (not (number? v))) tree)
         ssize    (loop [sds subdirs
                         acc []]
                    (if (empty? sds)
                      acc
                      (let [[name sd] (first sds)
                            ds        (dir-size-distribution sd name)
                            acc       (if (empty? acc) [ds] (conj acc ds))]
                        (recur (next sds) acc))))]
     (cond
       (empty? subdirs)
       [dir (size-sum files)]

       (= "/" dir)
       (let [dirs (->> ssize flatten (partition 2) (map vec) vec)]
         (conj dirs [dir (+ (size-sum files) (total-dir-size dirs (map first subdirs)))]))

       :default
       (conj ssize [dir (+ (size-sum files) (total-dir-size ssize (map first subdirs)))])))))


(defn dirs-within-size [size tree]
  (->> tree
       dir-size-distribution
       (filter (fn [[_ s]] (< s size)))))

(defn smallest-deletable-dir [update-threshold available-space tree]
  (let [dirsize-dist    (dir-size-distribution tree)
        rootdir-size    (->> dirsize-dist
                             (filter (fn [[d _]] (= root-directory d)))
                             first
                             second)
        free            (- available-space rootdir-size)
        free-for-update (- update-threshold free)]
    (->> dirsize-dist
         (map second)
         (filter #(> % free-for-update))
         (apply min))))

(comment

  (->> datastream
       build-tree
       (dirs-within-size 100000)
       (map second)
       (reduce +)
       clojure.pprint/pprint)

  (->> datastream
       build-tree
       (smallest-deletable-dir 30000000 70000000)
       clojure.pprint/pprint)
  )
