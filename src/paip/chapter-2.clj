(ns paip.chapter-2)

(def simple-grammar
  {"sentence" [["noun-phrase" "verb-phrase"]]
   "noun-phrase" [["Article" "Noun"]]
   "verb-phrase" [["Verb" "noun-phrase"]]
   "Article" ["the" "a"]
   "Noun" ["man" "ball" "woman" "table"]
   "Verb" ["hit" "took" "saw" "liked"]})

(defn make-generate [grammar]
  (fn [phrase]
    (cond (coll? phrase)
          (mapcat generate phrase)
          (coll? (grammar phrase))
          (generate (rand-nth (grammar phrase)))
          :else [phrase])))

(def generate-simple (make-generate simple-grammer))
