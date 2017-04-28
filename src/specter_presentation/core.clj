(ns specter-presentation.core
  (:use [com.rpl.specter])
  (:require [clojure.walk :as walk]))


;; Good immutable programming

(assoc {:a 1 :b 2} :a 3)

(dissoc {:a 1 :b 2} :a)

(conj [1 2 3] 4)

(pop [1 2 3])

(conj #{1 2 3} 4)


(defn ns-qualify-keys-1 [m]
  (into {}
    (map (fn [[k v]]
      [(keyword (str *ns*) (name k)) v]
      ))
    m
    ))

#_
(ns-qualify-keys-1 {:a 1 :b 2})

#_
(class (ns-qualify-keys-1 (sorted-map :a 1 :b 2)))


(defn ns-qualify-keys-2 [m]
  (into (empty m)
    (map (fn [[k v]]
      [(keyword (str *ns*) (name k)) v]
      ))
    m
    ))

#_
(defn ns-qualify-keys-3 [m]
  (map-keys
   (fn [k]
     (update-namespace (fn [_] (str *ns*)) k))
   m
   ))

;; - Transform each key of a map
;; - Transform each value of a map
;; - Transform each value of a sequence
(map inc [1 2 3 4])

(let [data '(1 2 3 4)]
  (into (empty data) (map inc) data)
  )
;; - Append
;; - Prepend
;; - Add anywhere into a generic sequence (e.g. [1 2 4 5] => [1 2 3 4 5])
;; - Remove anywhere from a generic sequence (e.g. '(1 :a 2) => '(1 2))

;; fastest way to transform each value of a
;; vector?
;; list?
;; hash map?
;; array map?



#_
(map-vals
  (fn [v]
    (mapv
      (fn [m2]
        (map-vals
          (fn [n] (if (even? n) (inc n) n))
          m2
          ))
      v
      ))
  m)
{:a [{:b 2}
     {:c 3}]
 :d [{:e 4}]}


(defn update-last-vec [pred transformer v]
  (let [idx (reduce-kv (fn [res i e] (if (pred e) i res)) nil v)]
    (if idx (update v idx transformer) v)
    ))

(update-last-vec odd? inc [1 2 3 4 5 6])

;; only works on vectors, need different code for lists

(defn remove-last-vec [pred v]
  (let [idx (reduce-kv (fn [res i e] (if (pred e) i res)) nil v)]
    (if idx
      (reduce-kv
        (fn [res i e]
          (if (= i idx)
            res
            (conj res e))
          )
        []
        v)
      v
      )))

(remove-last-vec odd? [1 2 3 4 5 6])
;; not using transients
;; doesn't work on lists
;; code very different to update-last-vec




(setval [MAP-KEYS NAMESPACE] (str *ns*) {:a 1 :b 2})

(class (setval [MAP-KEYS NAMESPACE] (str *ns*) (sorted-map :a 1 :b 2)))

(transform [MAP-VALS ALL MAP-VALS even?] inc {:a [{:b 2} {:c 3}] :d [{:e 4}]})

(transform [(filterer odd?) LAST] inc [1 2 3 4 5 6])

(transform [(filterer odd?) LAST] inc '(1 2 3 4 5 6))

(setval [(filterer odd?) LAST] NONE [1 2 3 4 5 6])

(setval [(filterer odd?) LAST] NONE '(1 2 3 4 5 6))



;; there's infinite variety of ways you might want to manipulate data structures
;; making a separate function for each task is tiresome and verbose
;; fundamentally combinatoric problem space
;;   - can combine any data structures together
;;   - combining concepts of filter, last, first, "all vals", "all keys", etc...


;; subvalue navigation:

(transform [:a :b :c] inc {:a {:b {:c 1}}})

(select-any [:a :b :c] {:a {:b {:c 1}}})

(transform [ALL :a even?] inc [{:a 1} {:a 2 :b 1} {:a 4}])

(setval [MAP-VALS NAME] "a" {:a :a/v :b :c/v2})

(transform FIRST inc '(1 2 3 4))


;; substructure navigation:

(setval [ALL NAME END] "!" [:a :b :c :d])

(setval (srange 1 7) [] [1 2 3 4 5 6 7 8])

(transform (srange 1 7) reverse [1 2 3 4 5 6 7 8])

(transform (filterer even?) reverse [1 2 3 4 5 6 7 8 9])


(def plans
  {:plans '({:planId "1"
             :disbursements ({:history ({:amount "1" :disbursementDate "2017-01-01"}
                                        {:amount "1" :disbursementDate "2017-01-01"}
                                        {:amount "2" :disbursementDate "2017-02-01"}
                                        {:amount "3" :disbursementDate "2017-03-01"}
                                        {:amount "3" :disbursementDate "2017-03-01"}
                                        )})})})

(transform
  [:plans
   ALL
   #(= "1" (:planId %))
   :disbursements
   ALL
   :history
   (filterer :disbursementDate #(= % "2017-03-01"))]
  dedupe
  plans)

(transform (filterer even?) #(take 3 %) [1 2 3 4 5 6 7 8 9 10])
(setval [(filterer even?) (srange 0 3)] (repeat 3 NONE) [1 2 3 4 5 6 7 8 9 10])



(transform (subselect ALL :a even?)
  reverse
  [{:a 1} {:a 2 :b 1} {:a 4} {:a 5} {:a 6} {:a 8}])



;; recursive navigation:

(def TREE-VALUES
  (recursive-path [] p
    (if-path vector?
      [ALL p]
      STAY)))

(def tree [1 [2 [[3]] 4] [[5] 6] [7] 8 [[9]]])


(transform [TREE-VALUES even?] inc tree)

(select [TREE-VALUES odd?] tree)

;; Reverse order of even leaves (order based on depth-first search)

(transform (subselect TREE-VALUES even?) reverse tree)



(defn java-convert-bad [data]
  (walk/postwalk
    (fn [e]
      (cond (map? e) (java.util.HashMap. e)
            (vector? e) (into-array Object e)
            :else e
            ))
    data
    ))

#_
(java-convert-bad {:a [1 2 {:b 2}] :c 3})
;; => exception

(defn java-convert-bad-2 [data]
  (walk/prewalk
    (fn [e]
      (cond (map? e) (java.util.HashMap. e)
            (and (-> e map-entry? not) (vector? e)) (into-array Object e)
            :else e
            ))
    data
    ))

#_
(java-convert-bad-2 {:a [1 2 {:b 2}] :c 3})


(defn java-convert [data]
  (let [node-walker (recursive-path [] p
                      (cond-path
                        map? (continue-then-stay MAP-VALS p)
                        vector? (continue-then-stay ALL p)
                        ))]
    (transform node-walker
      (fn [e]
        (if (map? e)
          (java.util.HashMap. e)
          (into-array Object e)
          ))
      data
      )))



;; Automatically stops traversal (for performance)
(select-first [ALL even?] [1 2 3 4 5])

(defn spy [v] (println "Traversed:" v) v)

(select-first [ALL ALL (view spy) even?] [[1] [2 3 4] [5]])

;; NONE for removal
(setval :a NONE {:a 1 :b 2})
(setval (keypath 1) NONE [1 2 3 4 5])
(setval [ALL even?] NONE [1 2 3 4 5 6])
(setval [MAP-VALS even?] NONE {:a 1 :b 2 :c 3 :d 4})
(setval [MAP-KEYS even?] NONE {1 :a 2 :b 3 :c 4 :d})
(setval FIRST NONE '(1 2 3 4))

;; Navigators extended to strings
(setval (srange 2 4) "(())" "abcdefg")
(setval FIRST NONE "abcd")
(setval END "!!!" "abcd")


;; show implementation of keypath (dynamic navs)
(select-any (keypath "a" "b" "c") {"a" {"b" {"c" 1}}})

#_
(defnav keypath* [key]
  (select* [this structure next-fn]
    (next-fn (get structure key)))
  (transform* [this structure next-fn]
    (let [newv (next-fn (get structure key))]
      (if (identical? newv NONE)
        (if (sequential? structure)
          (i/srange-transform* structure key (inc key) (fn [_] []))
          (dissoc structure key))
        (assoc structure key newv))
      )))

#_
(defdynamicnav
  ^{:doc "Turns a navigator that takes one argument into a navigator that takes
          many arguments and uses the same navigator with each argument. There
          is no performance cost to using this. See implementation of `keypath`"}
  eachnav
  [navfn]
  (let [latenavfn (late-resolved-fn navfn)]
    (dynamicnav [& args]
      (map latenavfn args))))

#_
(def keypath (eachnav keypath*))


(transduce
  (comp (map :a) (mapcat identity) (filter odd?))
  +
  [{:a [1 2]} {:a [3]} {:a [4 5]}])

(transduce
 (traverse-all [:a ALL odd?])
  +
  [{:a [1 2]} {:a [3]} {:a [4 5]}])


;; Performance
; Benchmark: get value in nested map (2500000 iterations)
;
; Avg(ms)		vs best		Code
; 42.784 		 1.00 		 (-> data (get :a) (get :b) (get :c))
; 50.473 		 1.18 		 (-> data :a :b :c)
; 110.22 		 2.58 		 (compiled-select-any p data)
; 123.02 		 2.88 		 (select-any [:a :b :c] data)
; 151.38 		 3.54 		 (get-in data [:a :b :c])
;
; ********************************
;
; Benchmark: set value in nested map (2500000 iterations)
;
; Avg(ms)		vs best		Code
; 408.16 		 1.00 		 (setval [:a :b :c] 1 data)
; 1539.8 		 3.77 		 (assoc-in data [:a :b :c] 1)
;
; ********************************
;
; Benchmark: update value in nested map (500000 iterations)
;
; Avg(ms)		vs best		Code
; 98.195 		 1.00 		 (manual-transform data inc)
; 107.82 		 1.10 		 (transform [:a :b :c] inc data)
; 565.33 		 5.76 		 (update-in data [:a :b :c] inc)
;
; ********************************
;
; Benchmark: transform values of a small map (500000 iterations)
;
; Avg(ms)		vs best		Code
; 61.913 		 1.00 		 (transform MAP-VALS inc data)
; 90.485 		 1.46 		 (persistent! (reduce-kv (fn [m k v] (assoc! m k (inc v))) (transient {}) data))
; 91.506 		 1.48 		 (reduce-kv (fn [m k v] (assoc m k (inc v))) {} data)
; 95.772 		 1.55 		 (map-vals-map-iterable data inc)
; 111.07 		 1.79 		 (map-vals-map-iterable-transient data inc)
; 142.80 		 2.31 		 (reduce-kv (fn [m k v] (assoc m k (inc v))) (empty data) data)
; 162.93 		 2.63 		 (transform [ALL LAST] inc data)
; 375.73 		 6.07 		 (zipmap (keys data) (map inc (vals data)))
; 513.46 		 8.29 		 (into {} (for [[k v] data] [k (inc v)]))
; 520.31 		 8.40 		 (into {} (map (fn [e] [(key e) (inc (val e))]) data))
;
; ********************************
;
; Benchmark: transform values of large map (600 iterations)
;
; Avg(ms)		vs best		Code
; 83.003 		 1.00 		 (persistent! (reduce-kv (fn [m k v] (assoc! m k (inc v))) (transient clojure.lang.PersistentHashMap/EMPTY) data))
; 87.987 		 1.06 		 (persistent! (reduce-kv (fn [m k v] (assoc! m k (inc v))) (transient {}) data))
; 88.688 		 1.07 		 (transform MAP-VALS inc data)
; 102.25 		 1.23 		 (reduce-kv (fn [m k v] (assoc m k (inc v))) {} data)
; 111.21 		 1.34 		 (reduce-kv (fn [m k v] (assoc m k (inc v))) (empty data) data)
; 120.65 		 1.45 		 (transform [ALL LAST] inc data)
; 156.83 		 1.89 		 (map-vals-map-iterable-transient data inc)
; 175.89 		 2.12 		 (map-vals-map-iterable data inc)
; 176.70 		 2.13 		 (into {} (for [[k v] data] [k (inc v)]))
; 207.42 		 2.50 		 (into {} (map (fn [e] [(key e) (inc (val e))]) data))
; 239.22 		 2.88 		 (zipmap (keys data) (map inc (vals data)))
;
; ********************************
;
; Benchmark: first value of a size 10 vector (10000000 iterations)
;
; Avg(ms)		vs best		Code
; 329.29 		 1.00 		 (select-any FIRST data)
; 355.91 		 1.08 		 (select-any ALL data)
; 391.98 		 1.19 		 (select-first ALL data)
; 617.86 		 1.88 		 (first data)
;
; ********************************
;
; Benchmark: even :a values from sequence of maps (500000 iterations)
;
; Avg(ms)		vs best		Code
; 141.93 		 1.00 		 (select [ALL :a even?] data)
; 198.56 		 1.40 		 (into [] xf data)
; 206.06 		 1.45 		 (into [] (comp (map :a) (filter even?)) data)
; 232.89 		 1.64 		 (->> data (mapv :a) (filter even?) doall)
;
; ********************************
;
; Benchmark: Append to a large vector (2000000 iterations)
;
; Avg(ms)		vs best		Code
; 75.461 		 1.00 		 (conj v 1)
; 108.48 		 1.44 		 (setval AFTER-ELEM 1 v)
; 245.41 		 3.25 		 (reduce conj v [1])
; 293.18 		 3.89 		 (setval END [1] v)
;
; ********************************
;
; Benchmark: prepend to a vector (1000000 iterations)
;
; Avg(ms)		vs best		Code
; 352.52 		 1.00 		 (into [0] data)
; 357.82 		 1.02 		 (vec (cons 0 data))
; 469.81 		 1.33 		 (setval BEFORE-ELEM 0 data)
;
; ********************************
;
; Benchmark: vary metadata (800000 iterations)
;
; Avg(ms)		vs best		Code
; 222.78 		 1.00 		 (setval [META :y] 2 data)
; 257.13 		 1.15 		 (vary-meta data assoc :y 2)
;
; ********************************
;
; Benchmark: namespace qualify keys of a small map (1000000 iterations)
;
; Avg(ms)		vs best		Code
; 263.63 		 1.00 		 (setval [MAP-KEYS NAMESPACE] (str *ns*) data)
; 484.88 		 1.84 		 (reduce-kv (fn [m k v] (assoc m (keyword (str *ns*) (name k)) v)) {} data)
; 702.13 		 2.66 		 (into {} (map (fn [[k v]] [(keyword (str *ns*) (name k)) v])) data)
;
; ********************************
;
; Benchmark: namespace qualify keys of a large map (1200 iterations)
;
; Avg(ms)		vs best		Code
; 227.54 		 1.00 		 (setval [MAP-KEYS NAMESPACE] (str *ns*) data)
; 338.59 		 1.49 		 (reduce-kv (fn [m k v] (assoc m (keyword (str *ns*) (name k)) v)) {} data)
; 403.91 		 1.78 		 (into {} (map (fn [[k v]] [(keyword (str *ns*) (name k)) v])) data)




;; Inline caching
#_
(with-inline-debug
  (path :a :b :c))

#_
(with-inline-debug
  (let [k 1]
    (path :a (selected? (keypath k) even?) ALL)))
