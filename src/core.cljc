(ns core
  (:import [java.lang Long]))

;; NOTE: based on the exercise description, "graph" here always assumes a
;; directed, possibly cyclical, simple graph.

(def unweighted-graph
  "A malli schema for an unweighted graph.
   
   Example:
   {:1 [:2 :3]
    :2 [:4]
    :3 [:4]
    :4 []}"
  [:map-of :keyword [:vector :keyword]])

(def weighted-graph
  "A malli schema for a weighted graph, assumes non-negative weights, as it's
   required for Dijkstra's.
   
   Example:
   {:1 {:2 1, :3 2}
    :2 {:4 4}
    :3 {:4 2}
    :4 {}}"
  [:map-of
   :keyword
   [:map-of :keyword nat-int?]])

(defn random-graph
  "Generates a random, connected graph with n vertices and s edges.
   If s < n(n - 1), then graph will only be weakly connected.
   
   Opts:
   max-weight - the max. weight that will be generated for an edge"
  [n s & {:keys [max-weight] :or {max-weight 100}}]
  (assert (nat-int? n) "n must be a natural integer")
  (assert (<= (dec n) s (* n (dec n))) "s must be between n-1 & n(n-1)")
  (case n
    0 {}
    1 {:1 {}}
    (let [vertices (shuffle (map (comp keyword str) (range 1 (inc n))))
          [v1 v2 & v3-onward] vertices
          {:keys [min excess]}
          (reduce (fn [{:keys [seen min excess]} vertex]
                    (let [edges (mapcat (juxt #(vector vertex %)
                                              #(vector % vertex))
                                        seen)
                          random-edge (rand-nth (vec edges))]
                      {:seen (conj seen vertex)
                       :min (conj min random-edge)
                       :excess (into excess (remove #{random-edge}) edges)}))
                  {:seen [v1 v2]
                   :min [[v1 v2]]
                   :excess [[v2 v1]]}
                  v3-onward)
          edges (->> (shuffle excess)
                     (into min)
                     (take s))]
      (reduce (fn [graph edge]
                (assoc-in graph edge (rand-int max-weight)))
              (into {} (map vector vertices (repeat {})))
              edges))))

(defn dijkstra
  "Takes a graph and an origin, returns a map of node keys to maps. Each map
   under a node key has two keys:
   :path - A vector of keys detailing the shortest path from origin to that node
   :distance - The geodesic distance from origin to that node"
  [graph origin]
  (let [infinity Long/MAX_VALUE
        sum-dist #(or (some #{infinity} %&) (apply + %&))
        init-acc-vals {:path [], :distance infinity}
        init-origin {:path [origin], :distance 0}]
    (loop [current origin
           unvisited (disj (set (keys graph)) origin)
           accumulator (assoc (zipmap (keys graph) (repeat init-acc-vals))
                              origin init-origin)]
      (if (empty? unvisited)
        (update-vals accumulator #(if (= infinity (:distance %)) ::no-path %))
        (let [{current-path :path, current-dist :distance} (accumulator current)
              dists-via-current
              (-> (graph current)
                  (select-keys unvisited)
                  (update-vals #(sum-dist % current-dist)))
              new-accumulator
              (reduce (fn [acc [node dist-via-current]]
                        (if (>= dist-via-current (get-in acc [node :distance]))
                          acc
                          (-> acc
                              (assoc-in [node :distance] dist-via-current)
                              (assoc-in [node :path] (conj current-path node)))))
                      accumulator
                      dists-via-current)
              new-current
              (apply min-key (comp :distance new-accumulator) unvisited)]
          (recur new-current
                 (disj unvisited new-current)
                 new-accumulator))))))

(defn shortest-path
  "Takes a graph, an origin and destination key and returns a vector containing
   the shortest path from origin to destination."
  [graph origin destination]
  (get-in (dijkstra graph origin) [destination :path] ::no-path))

(defn eccentricity
  "Takes a graph and an origin and returns the furthest node away from origin by
   geodesic distance."
  [graph origin]
  (apply max (->> (dijkstra graph origin)
                  vals
                  (remove #{::no-path})
                  (map :distance))))

(defn radius
  "Takes a graph and returns the minimum eccentricity among its vertices."
  [graph]
  (apply min (map #(eccentricity graph %) (keys graph))))

(defn diameter
  "Takes a graph and returns the maximum eccentricity among its vertices."
  [graph]
  (apply max (map #(eccentricity graph %) (keys graph))))

(comment
  (let [n 5
        s 4
        g (random-graph n s)]
    (tap> {:graph g})
    (tap> {:shortest-path (shortest-path g :1 (-> n str keyword))})
    (tap> {:eccentricity (eccentricity g :1)})
    (tap> {:radius (radius g)})
    (tap> {:diameter (diameter g)}))
  )