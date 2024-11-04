(ns remplater.render
  (:require
    [clojure.walk :as walk]
    [remplater.components :as c]
    [remplater.fig-operations :as fo]
    [remplater.pdf :as pdf]))

(defn render-tree [tree]
  (->> tree
    (walk/prewalk
      (fn [node]
        (let []
          (cond
            (and (vector? node)
              (fn? (first node)))
            (let [f (first node)
                  [fig-opts & children] (rest node)
                  next-children (apply f fig-opts children)]
              (into [f fig-opts]
                (->> next-children
                  (mapv (fn [next-child]
                          (-> next-child
                            (update 1 #(merge fig-opts %))))))))

            :else
            node))))))

(defn render-page [page-tree]
  (let [page-component (first page-tree)
        [page-opts & children] (rest page-tree)
        {:keys [document page]} page-opts]
    (pdf/with-page-content-stream document page
      (fn [cs]
        (render-tree
          (into [page-component (assoc page-opts :cs cs)]
            children))))))

(defn render-document [document-tree]
  (with-open [document (pdf/make-document)]
    (let [{:keys [output]} (second document-tree)
          page-trees (->> (drop 2 document-tree)
                       (mapv (fn [page-tree]
                               (let [page-obj (pdf/make-page document)]
                                 (-> page-tree
                                   (assoc-in [1 :document] document)
                                   (assoc-in [1 :page] page-obj)
                                   (update 1 #(merge % (-> page-obj
                                                         pdf/page->pdrect
                                                         pdf/pdrect->fig-opts))))))))]
      (doseq [page-tree page-trees]
        (render-page page-tree))
      (pdf/save-document document output))))

(comment
  (render-document
    [c/document {:output "/tmp/blank.pdf"}
     [c/page {}
      [c/rect {}]
      #_[c/text {:text "Hello World"}]]])

  (render-document
    [c/document {:output "/tmp/blank.pdf"}
     [c/page {}
      [c/grid {:rows 3 :cols 3}
       [c/rect {}]]]])

  (render-document
    [c/document {:output "/tmp/blank.pdf"}
     [c/page {}
      [c/grid {:rows 3 :cols 3}
       [(fn [fig-opts]
          [[c/rect fig-opts]
           [c/text {:text (str (:index fig-opts))
                    :font-size 30}]])]]]])

  (pdf/with-document "/tmp/blank.pdf"
    (fn [doc]
      (let [page-1 (pdf/make-page doc)]
        (pdf/with-page-content-stream doc page-1
          (fn [cs]
            (render-tree
              [c/document {}
               [c/page {:x1 200 :y1 200 :x2 600 :y2 600 :cs cs}
                [c/rect {}]
                [c/text {:text "Hello World"}]]])))))))
