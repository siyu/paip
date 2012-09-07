(ns paip.chapter-2)

(def simple-grammar
  {"sentence" [["noun-phrase" "verb-phrase"]]
   "noun-phrase" [["Article" "Noun"]]
   "verb-phrase" [["Verb" "noun-phrase"]]
   "Article" ["the" "a"]
   "Noun" ["man" "ball" "woman" "table"]
   "Verb" ["hit" "took" "saw" "liked"]})

(def bigger-grammar
  {"sentence" [["noun-phrase" "verb-phrase"]]
   "noun-phrase" [["Article" "Adj*" "Noun" "PP*"] ["Name"] ["Pronoun"]]
   "verb-phrase" [["Verb" "noun-phrase" "PP*"]]
   "PP*" [[] ["PP" "PP*"]]
   "Adj*" [[] ["Adj" "Adj*"]]
   "PP" [["Prep" "noun-phrase"]]
   "Prep" ["to" "in" "by" "with" "on"]
   "Adj" ["big" "little" "blue" "green" "adiabatic"]
   "Article" ["the" "a"]
   "Name" ["Pat" "Kim" "Lee" "Terry" "Robin"]
   "Noun" ["man" "ball" "woman" "table"]
   "Verb" ["hit" "took" "saw" "liked"]
   "Pronoun" ["he" "she" "it" "these" "those" "that"]})

(defn make-generate [grammar]
  (fn gen [phrase]
    (cond (coll? phrase)
          (mapcat gen phrase)
          (coll? (grammar phrase))
          (gen (rand-nth (grammar phrase)))
          :else [phrase])))

(def generate-simple (make-generate simple-grammar))
(def generate-bigger (make-generate bigger-grammar))

(defn make-generate-tree [grammar]
  (fn gen [phrase]
    (cond (coll? phrase)
          (map gen phrase)
          (coll? (grammar phrase))
          (conj [phrase] (gen (rand-nth (grammar phrase))))
          :else [phrase])))

(def generate-tree-simple (make-generate-tree simple-grammar))
(def generate-tree-bigger (make-generate-tree bigger-grammar))
