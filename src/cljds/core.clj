(ns cljds.core
  "Implementation of AVL-tree")

(defrecord Node [element left right])

(defn insert [{:keys [element left right] :as tree} value]
  (cond
   (nil? tree) (Node. value nil nil)
   (< value element) (Node. element (insert left value) right)
   (> value element) (Node. element left (insert right value))
   :else tree))

(defn min-element [{:keys [element left]}]
  (if left (recur left) element))

(defn max-element [{:keys [element right]}]
  (if right (recur right) element))

(defn remove-element [{:keys [element left right] :as tree} value]
  (cond
   (nil? tree) nil
   (< value element) (Node. element (remove-element left value) right)
   (> value element) (Node. element left (remove-element right value))
   (nil? left) right
   (nil? right) left
   :else (let [min-value (min-element right)]
           (Node. min-value left (remove-element right min-value)))))

(defn contains-element?
  ([tree x] (contains-element? tree nil x))
  ([{:keys [element left right] :as tree} candidate x]
   (cond
    (nil? tree)   (= x candidate)
    (< x element) (contains-element? left candidate x)
    :else         (contains-element? right element x))))

(defn count-elements [{:keys [left right] :as tree}]
  (if tree
    (+ 1 (count-elements left) (count-elements right))
    0))

(defn height
  ([tree] (height tree 0))
  ([{:keys [element left right] :as tree} count]
   (if tree
     (max (height left (inc count))
          (height right (inc count)))
     count)))

(defn bst?
  ([tree] (bst? tree Integer/MIN_VALUE Integer/MAX_VALUE))
  ([{:keys [element left right] :as tree} lb ub]
   (cond
    (nil? tree) true
    (< element lb) false
    (< ub element) false
    :else (and (bst? left lb (dec element))
               (bst? right (inc element) ub)))))

(def into-tree #(reduce insert nil %))
(defn into-list [{:keys [element left right] :as tree}]
  (cond
    (nil? tree) (list)
    :else       (concat (into-list left) (list element) (into-list right))))

(defn factor [{:keys [left right]}]
  (- (height left) (height right)))

(defn rotate-right
  "rotate tree right (x (lx ll lr) r) -> (lx ll (x lr r))"
  [{:keys [element left right] :as tree}]
  (if left
    (Node. (:element left) (:left left) (Node. element (:right left) right))
    tree))

(defn rotate-left
  "rotate tree left (x l (rx rl rr)) -> (rx (x l rl) rr)"
  [{:keys [element left right] :as tree}]
  (if right
    (Node. (:element right) (Node. element left (:left right)) (:right right))
    tree))

(defn is-left-case? [tree]
  (< (factor tree) -1))

(defn is-left-right-case? [tree]
  (and (is-left-case? tree)
       (> (factor (:right tree)) 0)))

(defn is-right-case? [tree]
  (> (factor tree) 1))

(defn is-right-left-case? [tree]
  (and (is-right-case? tree)
       (< (factor (:left tree)) 0)))

(defn balance [{:keys [element left right] :as tree}]
  (cond
   (is-right-left-case? tree) (rotate-right (Node.
                                             element
                                             (rotate-left left)
                                             right))
   (is-right-left-case? tree) (rotate-left (Node.
                                            element
                                            left
                                            (rotate-right right)))
   (is-right-case? tree) (rotate-right tree)
   (is-left-case? tree) (rotate-left tree)
   :else tree))

(def avl-insert (comp balance insert))
(def avl-remove (comp balance remove-element))
(def seq->avl (partial reduce avl-insert nil))

(comment
 (seq->avl [1 2 3 4 5]))
