;;A set of layout and formatting extensions to marginalia to allow
;;a vertical layout.  Ideally, we would have a way to let the caller
;;supply a layout, as they do with css, but eh this is good enough for
;;me!
(ns marginalia.extensions
  (:use [marginalia.html :exclude [section-to-html
                                   dependencies-html
                                   header-html
                                   toc-html
                                   groups-html
                                   body-html
                                   page-template
                                   uberdoc-html
                                   index-html
                                   single-page-html]]
        [marginalia.hiccup :only (html escape-html)]))

;;Added an option to modify layouts.
;;The marginal default is okay, but having a centered layout is mo'
;;bettah for my use cases, particularly  large projects with lots of
;;prose.  That's also the way Knuth likes it! 
(def ^:dynamic *layout* :vertical)

;;Where the used to be normal functions - due to a single layout
;;we swap in some multimethods to generate html content based on the
;;layout binding.  This is a dispatch function they all use.
(def layout-dispatch (fn [& args] *layout*))

;;Since we provide layout options, now, an alternative is to provide 
;;a classic layout, where code is centered with text, rather than 
;;arrayed side by side.  Rephrasing section-to-html as a multimethod 
;;that dispatches on the binding of *layout*

(defmulti section-to-html layout-dispatch)
;;The default parallel table.
(defmethod section-to-html :parallel [section]
  (html [:tr
         [:td {:class "docs"} (docs-to-html
                               (if (= (:type section) :comment)
                                 (:raw section)
                                 (:docstring section)))]
         [:td {:class "codes"} (if (= (:type section) :code)
                                  (codes-to-html (:raw section))
                                  "")]]))

;;We just vertically align two divs instead of a table.
(defmethod section-to-html :vertical [section]
  (html 
   [:div {:class "docs"} (docs-to-html
                          (if (= (:type section) :comment)
                            (:raw section)
                            (:docstring section)))]
   [:div {:class "codes"} (if (= (:type section) :code)
                            (codes-to-html (:raw section))
                            "")]))

(defn dependencies-html [deps & header-name]
  (when-let [deps (seq deps)]
    (let [header-name (or header-name "dependencies")]
      (html [:div {:class "dependencies"}
             [:h3 header-name]
             [:table
              (map #(html [:tr
                           [:td {:class "dep-name"} (str (first %))]
                           [:td {:class "dotted"} [:hr]]
                           [:td {:class "dep-version"} (second %)]])
                   deps)]]))))


(defmulti header-html layout-dispatch) 

(defmethod header-html :parallel [project-info]
  (html
   [:tr
    [:td {:class "docs"}
     [:div {:class "header"}
      [:h1 {:class "project-name"} (:name project-info)]
      [:h2 {:class "project-version"} (:version project-info)]
      [:br]
      (md (:description project-info))]
     (dependencies-html (:dependencies project-info))
     (dependencies-html (:dev-dependencies project-info) "dev dependencies")]
    [:td {:class "codes"
          :style "text-align: center; vertical-align: middle;color: #666;padding-right:20px"}
     [:br]
     [:br]
     [:br]
     "(this space intentionally left almost blank)"]]))

;;this is a hack, although the output is right.  I spliced it in due
;;to some experimentation.  Can be removed later.
(defn get-dependencies [xs]
  (loop [acc ""
         remaining xs]
    (if (empty? remaining) acc
        (let [[k v] (first remaining)]
          (recur (clojure.string/join \newline [acc (dependencies-html k v)])
                 (rest remaining))))))
    
    
(defmethod header-html :vertical [project-info]
  (html
   [:div {:class "docs"}
    [:div {:class "header"}
     [:h1 {:class "project-name"} (:name project-info)]
     [:h2 {:class "project-version"} (:version project-info)]
     [:br]
     (md (:description project-info))]    
    (get-dependencies [[(:dependencies project-info) "dependencies"]
                       [(:dev-dependencies project-info) "dev dependencies"]])]))

(defmulti  toc-html layout-dispatch)
(defmethod toc-html :parallel [props docs]
  (html
   [:tr
    [:td {:class "docs"}
     [:div {:class "toc"}
      [:a {:name "toc"} [:h3 "namespaces"]]
      [:ul
       (map #(vector :li (link-to-namespace (:ns %) (:uberdoc? props)))
            docs)]]]
    [:td {:class "codes"} "&nbsp;"]]))

(defmethod toc-html :vertical [props docs]
  (html
   [:div {:class "docs"}
    [:div {:class "toc"}
     [:a {:name "toc"} [:h3 "namespaces"]]
     [:ul
      (map #(vector :li (link-to-namespace (:ns %) (:uberdoc? props)))
           docs)]]]))

(defmulti groups-html  layout-dispatch)

(defmethod  groups-html :parallel [props doc]
  (html
   [:tr
    [:td {:class "docs"}
     [:div {:class "docs-header"}
      [:a {:class "anchor" :name (:ns doc) :href (str "#" (:ns doc))}
       [:h1 {:class "project-name"}
        (:ns doc)]
       (link-to-toc (:uberdoc? props))]]]
    [:td {:class "codes"}]]
   (map section-to-html (:groups doc))
   [:tr
    [:td {:class "spacer docs"} "&nbsp;"]
    [:td {:class "codes"}]]))

(defmethod  groups-html :vertical [props doc]
  (html
   [:div {:class "docs"}
    [:div {:class "docs-header"}
     [:a {:class "anchor" :name (:ns doc) :href (str "#" (:ns doc))}
      [:h1 {:class "project-name"}
       (:ns doc)]
      (link-to-toc (:uberdoc? props))]]]
   [:div {:class "codes"}]
   (map section-to-html (:groups doc))
   [:div {:class "spacer docs"} "&nbsp;"]
   [:div {:class "codes"}]))

;;Added more pleasant styling for vertical layout.  Thanks RickyBoy.
(def vertical-css
  (css
   [:body {:font-family "'Palatino Linotype', 'Book Antiqua', Palatino, FreeSerif, serif;"
           :font-size "14px"
           :color "#252519"}]
   [:h1 {:font-size "20px"
         :margin-top 0}]
   [:a.anchor {:text-decoration "none"
               :color "#252519"}]
   [:a.anchor:hover {:color "#5050A6"}]
   [:table {:border-spacing 0
            :border-bottom "solid #ddd 1px;"
            :margin-bottom "10px"}]
   [:code {:display "inline"}]
   [:p {:margin-top "8px"}]
   [:tr {:margin "0px"
         :padding "0px"}]   
   [:div.docs {:vertical-align "top"
               :margin "0px"
               :padding-left "55px"
               :padding-right "20px"
               :border "none"
               :background-color "white"}]
   [:div.docs :pre {:font-size "12px"
                    :overflow "hidden"}]
   [:.codes {:border "none"
             :margin "0px"
             :margin-left "55px"
;             :padding "0px"
             :padding-left "20px"
             :border-left "solid #E5E5EE 6px"
             :font-size "12pt"
             :vertical-align "top"}]
   [:td.spacer {:padding-bottom "40px"}]
   [:td.docs {:width "410px"
              :max-width "410px"
              :vertical-align "top"
              :margin "0px"
              :padding-left "55px"
              :padding-right "20px"
              :border "none"
              :background-color "#FFF"}]
   [:td.docs :pre {:font-size "12px"
                   :overflow "hidden"}]
   [:td.codes {:width "55%"
               :background-color "#F5F5FF"
               :vertical-align "top"
               :margin "0px"
               :padding-left "20px"
               :border "none"
               :overflow "hidden"
               :font-size "10pt"
               :border-left "solid #E5E5EE 1px"}]
   [:pre :code {:display "block"
                :padding "4px"}]
   [:code {:border "solid #DEDEDE 1px"
           :padding-left "3px"
           :padding-right "3px"
           :font-size "14px"}]
   [:.syntaxhighlighter :code {:font-size "13px"}]
   [:.footer {:text-align "center"}]))

(defn get-general-css []
  (case *layout* 
    ("vertical" :vertical) vertical-css
    ("parallel" :parallel) general-css 
    (throw (Exception. (str "Unknown CSS style!"  *layout*)))))

;;body is variable layout
(defmulti  body-html layout-dispatch)
(defmethod body-html :parallel [header toc content]
  [:body
   [:table
    header
    toc
    content]])

(defmethod body-html :vertical [header toc content]
  [:body
   header
   toc
   content])

;;Using layouts now
(defn page-template
  "Notice that we're inlining the css & javascript for [SyntaxHighlighter](http://alexgorbatchev.com/SyntaxHighlighter/) (`inline-js`
   & `inline-css`) to be able to package the output as a single file (uberdoc if you will).  It goes without
   saying that all this is WIP and will probably change in the future."
  [project-metadata opt-resources header toc content]
  (binding [*layout* (get project-metadata :layout)]
    (html
     "<!DOCTYPE html>\n"
     [:html
      [:head
       [:meta {:http-equiv "Content-Type" :content "text/html" :charset "utf-8"}]
       [:meta {:name "description" :content (:description project-metadata)}]
       (inline-css  "shCore.css")
       (css
        [:.syntaxhighlighter {:overflow "hidden !important"}])
       (inline-css  "shThemeMarginalia.css")
       reset-css
       header-css
       floating-toc-css
                                        ;used to be 'general-css
       (get-general-css) ;new, we determine based on bindings.
       (inline-js "jquery-1.7.1.min.js")
       (inline-js "xregexp-min.js")
       (inline-js "shCore.js")
       (inline-js "shBrushClojure.js")
       opt-resources
       [:title (:name project-metadata) " -- Marginalia"]]
      (body-html header toc content) ;determined based on bindings
      [:div {:class "footer"}
        "Generated by "
        [:a {:href "https://github.com/fogus/marginalia"} "Marginalia"]
        ".&nbsp;&nbsp;"
        "Syntax highlighting provided by Alex Gorbatchev's "
        [:a {:href "http://alexgorbatchev.com/SyntaxHighlighter/"}
         "SyntaxHighlighter"]
        floating-toc-html]                 
      (inline-js "app.js")])))

;;Bind our own functions to override the marginalia.html ones.

(defn uberdoc-html
  "This generates a stand alone html file (think `lein uberjar`).
   It's probably the only var consumers will use."
  [project-metadata docs]
  (page-template
   project-metadata
   (opt-resources-html project-metadata)
   (header-html project-metadata)
   (toc-html {:uberdoc? true} docs)
   (map #(groups-html {:uberdoc? true} %) docs)))

(defn index-html
  [project-metadata docs]
  (page-template
   project-metadata
   (opt-resources-html project-metadata)
   (header-html project-metadata)
   (toc-html {:uberdoc? false} docs)
   "")) ;; no contents

(defn single-page-html
  [project-metadata doc all-docs]
  (page-template
   project-metadata
   (opt-resources-html project-metadata)
   "" ;; no header
   "" ;; no toc
   (groups-html {:uberdoc? false} doc)))


