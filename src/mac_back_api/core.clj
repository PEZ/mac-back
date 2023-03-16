(ns mac-back-api.core
  (:require
   [mac-back-api.server :refer [mac-incoming mac-outgoing run-server stop-server]]
   [clojure.core.async :refer [>! go chan go-loop alts!!]]
   [clojure.pprint]))

(def loop-stopper (chan))

(defn move [direction]
  (go (>! mac-outgoing {:action :step :direction direction})))

(defn mk-arena [data]
  (let [grid (into {} (for [[row y] (map-indexed (fn [v i] [i v]) (:current data))
                            [cell x] (map-indexed (fn [v i] [i v]) row)]
                        {{:x x :y y} cell}))]
    {:grid grid
     :me (->> grid
              (keep (fn [[k v]] (when (= "P" v) k)))
              first)}))

(def look-direction
  {:right {:x 1 :y 0}
   :left {:x -1 :y 0}
   :up {:x 0 :y -1}
   :down {:x 0 :y 1}})

(defn look [{:keys [grid me]} direction]
  (let [look-at (merge-with + me (look-direction direction))]
    (grid look-at)))

(defn choose-direction [arena]
  (let [thingy->direction (into {}
                                (keep (fn [direction]
                                        (case (look arena direction)
                                          "." {"." direction}
                                          " " {" " direction}
                                          nil))
                                      [:left :right :up :down]))]
    (if (thingy->direction ".")
      (thingy->direction ".")
      (thingy->direction " "))))

(def !frames (atom []))

(defn process-incoming []
  (reset! !frames [])
  (go-loop []
    (let [[data the-chan] (alts!! [mac-incoming loop-stopper])]
      (swap! !frames conj data)
      (clojure.pprint/pprint data)
      (if (= loop-stopper the-chan)
        (println "Process incoming go loop terminated")
        (when-let [direction (choose-direction (mk-arena data))]
          (println "Chosen direction: " direction)
          (move direction)
          (recur))))))

(comment
  @!frames
  
  (process-incoming)
  (go (>! mac-outgoing {:action :start}))
  (go (>! mac-outgoing {:action :step :direction :right}))
  
  (go (>! loop-stopper "Exterminate!!"))
  (go (>! mac-outgoing {:action :restart}))

  (look (mk-arena (first @!frames)) :right)
  (choose-direction (mk-arena (first @!frames)))
  
  (stop-server)
  :rfc)

(comment
  ;; === Step 1
  ;; Start the websocket server
  (run-server)
  ;; === Step 2
  ;; In the browser click the WS button in the upper left corner. It will be red initially then green when connected.
  ;; === Step 3 (Optional)
  ;; Although websocket data is sent and received via core async channels. There is no need to add additional async code.
  ;; The `process-incoming` function is provided as a starting place for putting move data and taking game state data from
  ;; the channels.
  (process-incoming)
  ;; === Step 3
  ;; Start the game
  (go (>! mac-outgoing {:action :start}))
  ;; === Step 4
  ;; Move the player
  ;; Valid :direction values are :up :down :left :right
  (go (>! mac-outgoing {:action :step :direction :down}))

  ;; == Other functions
  ;; Iterate and improve how you are processing where to move the player.
  ;; Stop the process-incoming go-loop.
  (go (>! loop-stopper "Exterminate!!"))

  ;; Update your move calculating logic then restart to loop
  (process-incoming)

  ;; Move, laugh, cry, get ended by ghosts and then start all over
  (go (>! mac-outgoing {:action :restart})))