(ns mac-back-api.server
  (:require
    [muuntaja.core :as m]
    [reitit.ring :as ring]
    [cognitect.transit :as transit]
    [clojure.data.json :as json]
    [org.httpkit.server :as httpkit]
    [clojure.core.async :as a :refer [>! go chan go-loop alts!!]])
  (:import
    [java.io ByteArrayInputStream ByteArrayOutputStream]))

(def default-state {:server nil})
(def state (atom default-state))
(def mac-incoming (chan))
(def mac-outgoing (chan))
(def loop-killswitch (chan))

(defn clj->transit-str [arg]
  (let [out (ByteArrayOutputStream. 4096)
        writer (transit/writer out :json)]
    (transit/write writer arg)
    (.toString out)))

(defn transit-str->clj [arg]
  (let [arg-json (json/read-str arg)
        arg-bytes (.getBytes arg-json "UTF-8")
        in (ByteArrayInputStream. arg-bytes)
        reader (transit/reader in :json)]
    (transit/read reader)))

(defn response [request]
  (httpkit/with-channel
    request
    channel
    (do
      (println "We have a connection")
      (go-loop []
        (let [[message the-chan] (alts!! [mac-outgoing loop-killswitch])]
          (if (= loop-killswitch the-chan)
            (println "Killing go loop")
            (do
              (httpkit/send! channel (clj->transit-str message))
              (recur))))))
    (httpkit/on-close
      channel
      (fn [status]
        (go (>! loop-killswitch "kill"))
        (println "The WS connection was closed")))
    (httpkit/on-receive channel (fn [data]
                                  (go (>! mac-incoming (transit-str->clj data)))))))

(def app
  (ring/ring-handler
    (ring/router
      ["/api/v1" [["/game-controller" {:get response}]]]
      {:data {:muuntaja m/instance}})
    (ring/create-default-handler)))

(defn run-server []
  (when-not (:server @state)
    (let [port 9998
          ;; run-server returns a function that stops the server
          stop-fn (httpkit/run-server app {:port port
                                           :max-body 100000000
                                           :join false})]
      (swap! state assoc :server stop-fn)
      (println "server started on port:" port))))

(defn stop-server []
  ((:server @state))
  (reset! state default-state))