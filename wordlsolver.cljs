(ns worldsolvr.core
  (:require [reagent.core :as r]
            [reagent.dom :as dom]
            [ajax.core :refer [GET POST]]
            [clojure.string :as s]
            [clojure.set :as set]
            #_[cljs.pprint :refer [pprint]]))

;; static state

(def alphas (set (map char (range 65 91))))

;; dynamic state

(def blacklist (r/atom #{}))

(def yellowlist (r/atom [#{} #{} #{} #{} #{}]))

(def greenlist (r/atom [nil nil nil nil nil]))

(def guesses (r/atom []))

(def words (map s/upper-case {% include "words.json" %}))

;; utility fns

(defn log [& args]
  (apply (.-log js/console) args))

(defn remove-index [xs n]
  (vec (concat (take n xs) (drop (inc n) xs))))

;; color management

(defn remove-grey! [letter]
  (swap! blacklist disj letter))

(defn remove-yellow!
  ([letter index]
   (swap! yellowlist update index #(disj % letter)))
  ([letter]
   (reset! yellowlist (mapv #(disj % letter) @yellowlist))))

(defn remove-green! [{:keys [index letter]}]
  (cond
    (and index letter) (when (= letter (get @greenlist index))
                         (swap! greenlist assoc index nil))
    letter (swap! greenlist #(replace {letter nil} %))
    index (swap! greenlist assoc index nil)))

(defn set-grey! [letter]
  (remove-yellow! letter)
  (remove-green! {:letter letter})
  (swap! blacklist conj letter))

(defn set-yellow! [letter index]
  (remove-grey! letter)
  (remove-green! {:letter letter :index index})
  (swap! yellowlist update index #(conj % letter)))

(defn set-green! [letter index]
  (remove-grey! letter)
  (remove-yellow! letter index)
  (swap! greenlist assoc index letter))

(defn set-unknown! [letter]
  (remove-grey! letter)
  (remove-yellow! letter)
  (remove-green! {:letter letter}))

(defn change-color! [current-color letter index]
  (case current-color
    (:unknown :grey) (set-yellow! letter index)
    :yellow (set-green! letter index)
    :green (set-grey! letter)))

;; letter inspection

(defn get-color [index letter]
  (cond
    (= letter (get @greenlist index)) :green
    (@blacklist letter) :grey
    ((get @yellowlist index) letter) :yellow
    :else :unknown))

(defn get-known-possible-letters [index]
  (set/difference
   (apply set/union @yellowlist)
   (get @yellowlist index)))

(defn get-possible-letters [index]
  (if-let [letter (get @greenlist index)]
    #{letter}
    (set/difference alphas @blacklist (get @yellowlist index))))

;; guess management

(defn add-guess! [guess]
  (when (and (some? guess) (== 5 (count guess)))
    (let [guess (s/upper-case guess)]
      (doseq [[index letter] (map-indexed vector guess)]
        (when (and (= (get-color index letter) :unknown)
                   (nil? ((get-known-possible-letters index) letter)))
          (set-grey! letter)))
      (swap! guesses conj guess))))

(defn get-guess-letters [index]
  (map #(nth % index) @guesses))

(defn recalculate! []
  (doseq [letter (set/difference alphas (set (apply concat @guesses)))]
    (set-unknown! letter)))

(defn remove-guess! [index]
  (swap! guesses remove-index index)
  (recalculate!))

;; filter words

(defn index-yellow-letters []
  (reduce-kv
   (fn [a k v]
     (reduce
      (fn [ax bx]
        (update ax bx conj k))
      a v))
   {} @yellowlist))

(defn replace-idx [smap v]
  (reduce-kv
   (fn [a k v]
     (assoc a k v))
   v smap))

(defn unfit-letter? [[index letter]]
  (nil? ((get-possible-letters index) letter)))

(defn unfit-subword? [word [letter ix]]
  (nil?
   ((disj (set (replace-idx (into {} (map vector ix (repeat nil)))
                            (vec word)))
          nil)
    letter)))

(defn unfit-word? [indexed-yellows word]
  (or (some unfit-letter? (map-indexed vector word))
      (some (partial unfit-subword? word) indexed-yellows)))

(defn filter-words [words]
  (remove (partial unfit-word? (index-yellow-letters)) words))

;; react elements

(defn banner []
  [:div
   [:div.banner
    (for [x "WORDLE"]
      [:div.char x])]
   [:div.description
    [:div "Make guesses in the input box"]
    [:div "Click on the characters in the guesses to change the color"]]])

(defn guess [word index]
  [:div.guess
   (for [[[i c] color] (map-indexed
                        (juxt vector get-color)
                        word)]
     [:div
      {:class [:char color]
       :on-click (fn [e]
                   (change-color!
                    color c i))}
      c])
   [:button
    {:on-click (fn [e] (remove-guess! index))}
    "Remove"]])

(defn submit-guess! [e]
  (.preventDefault e)
  (add-guess! (.-value (.getElementById js/document "guess"))))

(defn guess-form []
  [:form {:style {:display "flex" :flex-direction "column"}
          :onSubmit submit-guess!}
   [:input#guess.input-field]
   [:button.btn {:type "submit"} "GUESS"]])

(defn possible-words []
  (let [words (filter-words words)]
    [:div
     [:div (str "Possible words (" (count words) ")")]
     [:div.list-container
      (for [[i lword] (map-indexed vector (take 101 words))
            :let [uword (if (== i 100)
                          "..." (s/upper-case lword))]]
        [:div uword])]]))

(defn home []
  [:div
   [banner]
   [:div.guesses
    (for [[i word] (map-indexed vector @guesses)]
      [guess word i])]
   [possible-words]
   [guess-form]])

(dom/render [home] (.getElementById js/document "app"))
