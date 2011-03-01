(ns mockdbs-clj.core
  (:require [mockdbs-clj.widgets :as wd])
  (:import [javax.swing JLabel JPanel JFrame JTextField]
	   [java.awt.event ActionListener]
	   [javax.swing.event DocumentListener ChangeListener]
	   [java.awt GridBagLayout GridBagConstraints Insets]
	   [java.awt BorderLayout Color Font BasicStroke] 
	   [java.awt.geom Point2D$Double]
	   [javax.swing JFrame JButton JSlider JPanel JLabel]
	   (com.explodingpixels.macwidgets MacUtils
                                           UnifiedToolBar
                                           MacWidgetFactory
                                           BottomBarSize
                                           BottomBar)
	   [edu.umd.cs.piccolo PCanvas PLayer PNode]
	   [edu.umd.cs.piccolo.nodes PPath PText]
	   (edu.umd.cs.piccolo.event PZoomEventHandler
                                     PInputEvent
                                     PDragSequenceEventHandler)
	   [edu.umd.cs.piccolo.util PPaintContext]
           [mockdbs_clj BirdsEyeView]))

;;import edu.umd.cs.piccolo.event.PBasicInputEventHandler;
;;import edu.umd.cs.piccolo.event.PInputEvent;
;;import edu.umd.cs.piccolo.event.PInputEventListener;
;;import edu.umd.cs.piccolo.util.PPaintContext;

(def *neurons* (ref #{}))

(defmacro defbindable 
  "Creates the infrastucture for binding or watching a ref.
   There are several vars that are created as a result of
   using this macro:


   ex. (defbindable depth 10) creates the following:

   depth-ref - ref with value of 10
   depth-callbacks - a list of fns to call when depth changes
   clear-depth-callbacks - resets the callbacks var to []
   watch-depth - (watch-depth my-fn) will execute my-fn when
                 depth changes
   set-depth - (set-depth 50) will set depth to 50 and
                  execute all depth callbacks"
  [name initial-value]
  (let [ref-name (symbol (str name "-ref"))
	callbacks-name (symbol (str name "-callbacks"))
	clear-callbacks-name (symbol (str "clear-" name "-callbacks"))
	watch-name (symbol (str "watch-" name))
	set-name (symbol (str "set-" name))]
    `(do
       (def ~ref-name (ref ~initial-value))
       (def ~callbacks-name (ref []))
       (defn ~watch-name [f#]
	 (dosync
	  (alter ~callbacks-name conj f#)))
       (defn ~set-name [new-val#]
	 (dosync
	  (ref-set ~ref-name new-val#))
	 (doseq [f# @~callbacks-name]
	   (f# @~ref-name))
	 @~ref-name)
       (defn ~clear-callbacks-name []
	 (dosync
	  (ref-set ~callbacks-name []))))))


(defbindable depth 20)
(defbindable noise 0)


(defn neuron [& traits]
  (merge 
   {:name (str (java.util.UUID/randomUUID))
    :depth 0.0}
   (reduce 
    #(assoc %1 (first %2) (second %2)) 
    {} 
    (partition 2 traits))))

(defn stn [& depth]
  (let [n (neuron :type stn)]
    (if (first depth)
      (assoc n :depth (first depth))
      n)))

(defn get-neurons [] @*neurons*)

(defn add-neuron [n]
  (dosync
   (alter *neurons* conj n)))

(defn remove-neuron [n]
  (dosync
   (alter *neurons* disj n)))

(defn clear-neurons []
  (dosync
   (ref-set *neurons* #{})))

(defn mk-button [text on-click]
  (let [button (JButton. text)]
    (doto button
      (.putClientProperty "JButton.buttonType" "textured")
      (.addActionListener
       (proxy [ActionListener] []
	 (actionPerformed [evt] (on-click evt button)))))))

(defn zoom [camera zoom-point scale-delta]
  (.scaleViewAboutPoint
   camera
   scale-delta
   (.getX zoom-point)
   (.getY zoom-point)))

(defn mk-zoomhandler [] 
  (proxy [PZoomEventHandler] []
    (getMinScale [] 0)
    (getMaxScale [] Double/MAX_VALUE)
    (processEvent 
     [evt i]
     (when (.isMouseWheelEvent evt)
       (let [zoom-point (.getPosition evt)
	     camera (.getCamera evt)
	     dx (* -1 (.getWheelRotation evt))
	     scale-delta (+ 1 (* 0.05 dx))
	     current-scale (.getViewScale camera)
	     new-scale (* current-scale scale-delta)]
	 (when (and (> new-scale 0.001) (< new-scale Double/MAX_VALUE))
           (zoom camera zoom-point scale-delta)))))))

(defn mk-neuron-path [attrs]
  (let [attrs (merge {:diameter 25
		      :color Color/WHITE} attrs)
	node (PNode.)
	diameter (:diameter attrs)
	color (:color attrs)
	neuron (PPath/createEllipse (- (/ diameter 2)) (- (/ diameter 2)) diameter diameter)]
    (.addChild node neuron)
    (.setPaint neuron color)
    (.addChild node (PPath/createEllipse -2.5 -2.5 5 5))
    node))

(defn hex-to-color [hex-string-no-pound]
  (try
   (Color/decode hex-string-no-pound)
   (catch Exception e Color/WHITE)))

(defn lookup-color [color]
  (let [color-map {:white Color/WHITE
		   :black Color/BLACK
		   :red Color/RED
		   :green Color/GREEN
		   :blue Color/BLUE}
	c (color-map color)
	c (if c c
	      (hex-to-color (str "#" (name color))))]
    c))

(defn mk-probe []
  (let [node (PNode.)
	track (PPath/createLine 0 -1000 0 2000)
	probe (PPath/createLine 0 0 0 -3000)]
    (.setStroke track (BasicStroke. 3))
    (.setPaint track (Color/lightGray))
    (.setStrokePaint track (Color/lightGray))

    (.setStroke probe (BasicStroke. 5))
    (.setOffset probe (Point2D$Double. 0 -2000))
    (.addChild node track)
    (.addChild node probe)

    (doseq [i (range -20 10)]
      (let [text (PText. (str (- i) "mm"))]
	(.setFont text (Font. "Arial" Font/PLAIN 16))
	(.setOffset text (Point2D$Double. 30 (* i 100)))
	(.setTextPaint text Color/lightGray)
	(.addChild node text)))

    (watch-depth
     (fn [depth] 
       (println depth) 
       (.setOffset probe (Point2D$Double. 0 (* 100 (- depth))))))

    (.rotate node (* (/ Math/PI 360) 30))
    node))


(defn mk-neuron [attrs]
  (let [type (get attrs :type :generic)
	color (lookup-color (get attrs :color :white))]
    {:type type
     :color color
     :path (mk-neuron-path {:color color})}))

(def *paths-to-neuron* (atom {}))

(defn toolbar [& opts]
  (let [opts (apply hash-map opts)
        tb (UnifiedToolBar.)]
    (doseq [l (:left opts)]
      (.addComponentToLeft tb l))
    (doseq [r (:right opts)]
      (.addComponentToRight tb r))
    tb))

(defn mk-toolbar []
  (let [thal (mk-button "Thalamus" (fn [evt button] (add-neuron {:type :thalamus :color :blue})))
	stn (mk-button "STN" (fn [evt button] (add-neuron {:type :stn :color :red})))
	snr (mk-button "SNr" (fn [evt button] (add-neuron {:type :snr :color :green})))
        zoom-in (mk-button "+" (fn [evt button]
                                 ))
        zoom-out (mk-button "-" (fn [evt button]))]
    (toolbar :left [thal stn snr]
             :right [zoom-in zoom-out])))


(defn init-toolbar [toolbar]
  (let [thal (mk-button "Thalamus" (fn [evt button] (add-neuron {:type :thalamus :color :blue})))
	stn (mk-button "STN" (fn [evt button] (add-neuron {:type :stn :color :red})))
	snr (mk-button "SNr" (fn [evt button] (add-neuron {:type :snr :color :green})))
        zoom-in (mk-button "+" (fn [evt button]
                                 ))
        zoom-out (mk-button "-" (fn [evt button]))]
    (.addComponentToLeft toolbar thal)
    (.addComponentToLeft toolbar stn)
    (.addComponentToLeft toolbar snr)
    (.addComponentToRight toolbar zoom-in)
    (.addComponentToRight toolbar zoom-out)))

;; Controls

(defn mk-slider [opts]
  (let [direction (get opts :direction JSlider/VERTICAL)
	on-change (get opts :on-change (fn [evt slider]))
	min (get opts :min 0)
	max (get opts :max 10)
	steps (get opts :steps 1)
	start-value (get opts :start-value 0)
	focusable (get opts :focusable true)
	slider (JSlider. direction)]
    (doto slider
      (.setMaximum max)
      (.setMinimum min)
      (.setValue start-value)
      (.setFocusable true)
      (.addChangeListener 
       (proxy [ChangeListener] [] 
	 (stateChanged [ce] (on-change ce slider)))))))

(defn mk-depth-panel []
  (let [panel (JPanel.)
	slider (mk-slider 
		{:max 20000
		 :min -10000
		 :start-value 20000
		 :on-change #(set-depth (/ (.getValue %2) 1000.0))})]
    (.setLayout panel (BorderLayout.))
    (.setBackground panel (Color/WHITE))
    (.add panel slider BorderLayout/CENTER)
    panel))

(defn mk-canvas []
  (let [canvas (PCanvas.)
	node-drag-handler (proxy [PDragSequenceEventHandler] []
			    (drag [evt]
				  (let [node (.getPickedNode evt)
					node (loop [n (.getParent node)]
					       (if (nil? n)
						 nil
						 (if (@*paths-to-neuron* n)
						   (:path (@*paths-to-neuron* n))
						   (recur (.getParent n)))))]
				    (when node
				      (.translate node (.width (.getDelta evt)) (.height (.getDelta evt)))
				      (.setHandled evt true)))))
        bev (BirdsEyeView.)]
    (.connect bev canvas (into-array PLayer [(.getLayer canvas)]))
    (.setOffset (.getCamera bev) 10 10)
    (.addChild (.getCamera canvas) (.getCamera bev))
    (.setZoomEventHandler canvas (mk-zoomhandler))
    (.setAnimatingRenderQuality canvas PPaintContext/HIGH_QUALITY_RENDERING)
    (.setInteractingRenderQuality canvas PPaintContext/HIGH_QUALITY_RENDERING)
    (.addInputEventListener (.getLayer canvas) node-drag-handler)
    (.setViewOffset (.getCamera canvas) 1000/2 700/2)
    canvas))

(def *canvas* (mk-canvas))

(defn add-to-canvas [path]
  (.addChild (.getLayer *canvas*) path))

(defn add-neuron [attrs]
  (let [neuron (mk-neuron attrs)]
    (swap! *paths-to-neuron* assoc (:path neuron) neuron)
    (add-to-canvas (:path neuron))))

(defn mk-bottom-bar []
  (let [depth-label (JLabel. "0.00 mm")
	noise-slider (mk-slider {:direction JSlider/HORIZONTAL
				 :max 100
				 :on-change #(set-noise (/ (.getValue %2) 100.0))})
	noise-label (JLabel. "Noise")]
    (watch-depth 
     (fn [new-depth]
       (.setText depth-label (format "%+2.2f mm" new-depth))))
    (.setFont depth-label (Font. "Arial" Font/PLAIN 30))
    (.getComponent
     (wd/bottom-bar :size :large
                    :right [noise-label noise-slider]
                    :center [depth-label]))))

(defn mk-frame []
  (wd/mac-frame :toolbar (mk-toolbar)
                :bottombar (mk-bottom-bar)
                :east (mk-depth-panel)
                :center *canvas*))

(defn show [frame]
  (.setVisible frame true)
  frame)

(do
  (def *canvas* (mk-canvas))
  (clear-depth-callbacks)
  (add-to-canvas (mk-probe))
  (show (mk-frame))
  (set-depth 20.0))

#_(.setDefaultCloseOperation frame JFrame/EXIT_ON_CLOSE)
#_(defn mk-frame []
  (let [frame (JFrame.)
	toolbar (mk-toolbar)]
    (MacUtils/makeWindowLeopardStyle (.getRootPane frame))
    (.setSize frame 1000 700)
    (.add frame (.getComponent toolbar) BorderLayout/NORTH)
    (.installWindowDraggerOnWindow toolbar frame)
    (.setLocationRelativeTo frame nil)
    (.add frame (mk-depth-panel) BorderLayout/EAST)
    (.add frame *canvas* BorderLayout/CENTER)
    (.add frame (mk-bottom-bar) BorderLayout/SOUTH)
    frame))


#_(defn mk-bottom-bar []
  (let [bar (BottomBar. BottomBarSize/LARGE)
	depth-label (JLabel. "0.00 mm")
	noise-slider (mk-slider {:direction JSlider/HORIZONTAL
				 :max 100
				 :on-change #(set-noise (/ (.getValue %2) 100.0))})
	noise-label (JLabel. "Noise")]
    (watch-depth 
     (fn [new-depth]
       (.setText depth-label (format "%+2.2f mm" new-depth))))
    (.setFont depth-label (Font. "Arial" Font/PLAIN 30))
    (.addComponentToCenter bar depth-label)
    (.addComponentToRight bar noise-label)
    (.addComponentToRight bar noise-slider)
    (.getComponent bar)))

