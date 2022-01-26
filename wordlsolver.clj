(require '[clojure.java.io :as io]
         '[org.httpkit.server :as srv]
         '[hiccup.core :as hp])

(import 'java.io.ByteArrayOutputStream)

(def port 9093)

(def word-file "words.json")

(defn home [cljs-file]
  (hp/html
   [:html
    [:head
     [:meta {:charset "UTF-8"}]
     [:meta {:name "viewport" :content "width=device-width, initial-scale=1"}]
     [:link {:rel "stylesheet" :href (str "http://localhost:" port "/stylesheet.css")}]
     [:script {:crossorigin nil :src "https://unpkg.com/react@17/umd/react.production.min.js"}]
     [:script {:crossorigin nil :src "https://unpkg.com/react-dom@17/umd/react-dom.production.min.js"}]
     [:script {:src "https://cdn.jsdelivr.net/gh/borkdude/scittle@0.0.1/js/scittle.js" :type "application/javascript"}]
     [:script {:src "https://cdn.jsdelivr.net/gh/borkdude/scittle@0.0.1/js/scittle.reagent.js" :type "application/javascript"}]
     [:script {:src "https://cdn.jsdelivr.net/gh/borkdude/scittle@0.0.1/js/scittle.cljs-ajax.js" :type "application/javascript"}]
     [:title "wordlsolver"]]
    [:body
     [:div {:id "app"}]
     [:script {:type "application/x-scittle" :src cljs-file}]]]))

(defn get-words []
  (let [file (io/file word-file)]
    (when (.exists file)
      (slurp file))))

(defn get-stylesheet []
  (let [file (io/file "stylesheet.css")]
    (when (.exists file)
      (slurp file))))

(defn routes [{:keys [request-method uri] :as req}]
  (case [request-method uri]
    [:get "/favicon.ico"] {:status 200}
    [:get "/stylesheet.css"] {:status 200
                              :body (get-stylesheet)}
    [:get "/words"] {:headers {"Content-type" "application/json"}
                     :body (get-words)
                     :status 200}
    [:get "/wordlsolver.cljs"] {:body (slurp "wordlsolver.cljs")
                                :status 200}
    [:get "/"] {:body (home "wordlsolver.cljs")
                :status 200}))

(defn serve []
  (srv/run-server routes {:port port}))

(let [url (str "http://localhost:" port "/")]
  (serve)
  (println "serving" url)
  @(promise))
