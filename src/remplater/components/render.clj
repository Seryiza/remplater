(ns remplater.components.render
  (:require
    [clojure.zip :as zip]
    [remplater.pdf :as pdf]))

(def ^:dynamic *document* nil)
(def ^:dynamic *all-pages* nil)
(def ^:dynamic *page* nil)
(def ^:dynamic *cs* nil)

(defmulti render
  (fn [component attrs & children]
    (cond
      (keyword? component) component
      (fn? component) :fn)))

(defmethod render :document [_ attrs & children]
  children)

(defmethod render :page [_ attrs & children]
  children)

(defmethod render :fn [f attrs & children]
  (apply f attrs children))

(defn component? [el]
  (and (vector? el)
    (or (fn? (first el)) (keyword? (first el)))
    (map? (second el))))

(defn normalize-component [el]
  (let [el (cond
             (vector? el) el
             (fn? el) [el])
        component (first el)
        has-attrs? (map? (second el))
        attrs (if has-attrs?
                (second el)
                {})
        children (if has-attrs?
                   (drop 2 el)
                   (rest el))
        children (->> children
                   (map normalize-component))]
    (into [component attrs] children)))

(defn get-attr [component & attr-keys]
  (let [path (into [1] attr-keys)]
    (get-in component path)))

(defn get-attrs [component]
  (get component 1 {}))

(defn assoc-attrs [component new-attrs]
  (assoc component 1 new-attrs))

(defn assoc-attr [component attr-key attr-value]
  (assoc-in component [1 attr-key] attr-value))

(defn update-attr [component attr-key f & args]
  (apply update-in component [1 attr-key] f args))

(defn update-attrs [component f & args]
  (apply update component 1 f args))

(defn merge-attrs [component attrs]
  (update-attrs component merge attrs))

(defn get-children [component]
  (drop 2 component))

(defn merge-unexisting-attrs [component & attrs]
  (-> component
    (normalize-component)
    (update-attrs #(apply merge (concat (reverse attrs) [%])))))

(defn find-first [pred coll]
  (some (fn [x]
          (when (pred x)
            x))
    coll))

(defn alter-loc [loc loc-fn]
  (loop [loc loc]
    (if (zip/end? loc)
      loc
      (-> loc loc-fn zip/next recur))))

(defn iter-zip [zipper]
  (->> zipper
    (iterate zip/next)
    (take-while (complement zip/end?))))

(defn zip-up [loc]
  (->> loc
    (iterate zip/up)
    (take-while some?)))

(defn loc-page? [loc]
  (-> loc zip/node first (= :page)))

(defn loc-document? [loc]
  (-> loc zip/node first (= :document)))

(defn loc-has-position? [loc]
  (let [attrs (-> loc zip/node second)]
    (= 4 (count (select-keys attrs [:x1 :x2 :y1 :y2])))))

(defn with-graphic-state [f]
  (pdf/with-graphics-state *cs*
    (fn [cs]
      (binding [*cs* cs]
        (f)))))

(defn render-one [node]
  (let [[component attrs & children] (normalize-component node)
        result (apply render component attrs children)
        next-children (if (and (coll? result) (coll? (first result)))
                        result
                        [result])
        next-children (->> next-children
                        (map normalize-component))]
    (into [component attrs] next-children)))

(defn render-loc [loc]
  (let [node (zip/node loc)]
    (if (component? node)
      (let [node-with-position (->> loc
                                 (zip-up)
                                 (find-first loc-has-position?)
                                 (zip/node))
            position (-> node-with-position
                       (get-attrs)
                       (select-keys [:x1 :y1 :x2 :y2]))
            node (merge-unexisting-attrs node position)
            rendered-node (with-graphic-state #(render-one node))]
        (zip/replace loc rendered-node))
      loc)))

(defn render-until-page-loc [loc]
  (let [node (-> loc zip/node)]
    (if (and (component? node)
          (not (->> loc zip-up (find-first loc-page?))))
      (zip/replace loc (render-one node))
      loc)))

(defn doc-tree-zip [tree]
  (zip/zipper
    (fn [node]
      (and (component? node)
        (seq (get-children node))))
    (fn [node]
      (get-children node))
    (fn [node children]
      (into [(first node) (second node)] children))
    tree))

(defn load-fonts [document fonts]
  (->> fonts
    (reduce-kv
      (fn [fonts name path]
        (assoc fonts name (pdf/load-font document path)))
      {})))

(defn render-document [document-node]
  (with-open [document (pdf/make-document)]
    (let [tree (-> document-node
                 (normalize-component)
                 (doc-tree-zip)
                 (alter-loc render-until-page-loc)
                 (zip/node))
          document-node (->> tree
                          (doc-tree-zip)
                          (iter-zip)
                          (filter loc-document?)
                          (first)
                          (zip/node))
          document-node (-> document-node
                          (update-attr :fonts #(load-fonts document %)))
          document-attrs (get-attrs document-node)
          output-path (:output document-attrs)
          pages (->> tree
                  (doc-tree-zip)
                  (iter-zip)
                  (filter loc-page?)
                  (map zip/node)
                  (map (fn [page-node]
                         (let [page-attrs (get-attrs page-node)
                               page-size (or (:size page-attrs)
                                           (:page-size document-attrs))
                               page-obj (pdf/make-page
                                          {:document document
                                           :size page-size})]
                           (-> page-node
                             (assoc-attr :page-obj page-obj)
                             (merge-attrs (-> page-obj
                                            pdf/page->pdrect
                                            pdf/pdrect->attrs)))))))
          all-pages-attrs (mapv get-attrs pages)]
      (doseq [page pages]
        (let [page-obj (get-attr page :page-obj)]
          (pdf/with-page-content-stream document page-obj
            (fn [cs]
              (binding [*document* document-node
                        *all-pages* all-pages-attrs
                        *page* page-obj
                        *cs* cs]
                (-> page
                  (doc-tree-zip)
                  (alter-loc render-loc)
                  (zip/node)))))))
      (pdf/save-document document output-path))))
