(ns mockdbs-clj.core
  (:import [javax.swing JLabel JPanel JFrame JTextField]
	   [java.awt.event ActionListener]
	   [javax.swing.event DocumentListener ChangeListener]
	   [java.awt GridBagLayout GridBagConstraints Insets]
	   [java.awt BorderLayout Color Font] 
	   [javax.swing JFrame JButton JSlider JPanel JLabel]
	   [com.explodingpixels.macwidgets MacUtils UnifiedToolBar MacWidgetFactory BottomBarSize BottomBar]
	   [edu.umd.cs.piccolo PCanvas PLayer PNode]
	   [edu.umd.cs.piccolo.nodes PPath]
	   [edu.umd.cs.piccolo.event PZoomEventHandler PInputEvent PDragSequenceEventHandler]
	   [edu.umd.cs.piccolo.util PPaintContext]))

;;import edu.umd.cs.piccolo.event.PBasicInputEventHandler;
;;import edu.umd.cs.piccolo.event.PInputEvent;
;;import edu.umd.cs.piccolo.event.PInputEventListener;
;;import edu.umd.cs.piccolo.util.PPaintContext;

(def *neurons* (ref #{}))
(def *depth* (atom 20.0))
(def *depth-callbacks* (ref []))
(def *canvas* (mk-canvas))

(defn register-depth-callback [f]
  (dosync
   (alter *depth-callbacks* conj f)))

(defn update-depth [new-depth]
  (reset! *depth* new-depth)
  (doseq [f @*depth-callbacks*]
    (f new-depth))
  @*depth*)

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
	   (.scaleViewAboutPoint 
	    camera 
	    scale-delta 
	    (.getX zoom-point) 
	    (.getY zoom-point))))))))

(defn init-toolbar [toolbar]
  (let [thal (mk-button "Thalamus" (fn [evt button] (.addChild (.getLayer *canvas*) (mk-neuron {:color Color/BLUE}))))
	stn (mk-button "STN" (fn [evt button] (.addChild (.getLayer *canvas*) (mk-neuron {:color Color/RED}))))
	snr (mk-button "SNr" (fn [evt button] (.addChild (.getLayer *canvas*) (mk-neuron {:color Color/GREEN}))))]
    (.addComponentToRight toolbar thal)
    (.addComponentToRight toolbar stn)
    (.addComponentToRight toolbar snr)))

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
		 :on-change #(update-depth (/ (.getValue %2) 1000.0))})]
    (.setLayout panel (BorderLayout.))
    (.setBackground panel (Color/WHITE))
    (.add panel slider BorderLayout/CENTER)
    panel))

(defn mk-neuron [attrs]
  (let [attrs (merge {:diameter 25
		      :color Color/WHITE} attrs)
	diameter (:diameter attrs)
	color (:color attrs)
	neuron (PPath/createEllipse diameter diameter (/ diameter 2) (/ diameter 2))]
    (.setPaint neuron color)
    neuron))

(defn mk-canvas []
  (let [canvas (PCanvas.)
	node-drag-handler (proxy [PDragSequenceEventHandler] []
			    (drag [evt]
				  (let [node (.getPickedNode evt)]
				    (.translate node (.width (.getDelta evt)) (.height (.getDelta evt))))))]
    (.setZoomEventHandler canvas (mk-zoomhandler))
    (.setAnimatingRenderQuality canvas PPaintContext/HIGH_QUALITY_RENDERING)
    (.setInteractingRenderQuality canvas PPaintContext/HIGH_QUALITY_RENDERING)
    (.addInputEventListener (.getLayer canvas) node-drag-handler)
    (.setMarksAcceptedEventsAsHandled (.getEventFilter node-drag-handler) true)
    canvas))

(defn mk-bottom-bar []
  (let [bar (BottomBar. BottomBarSize/LARGE)
	depth-label (JLabel. "0.00 mm")
	noise-slider (mk-slider {:direction JSlider/HORIZONTAL
				 :max 100})
	noise-label (JLabel. "Noise")]
    (register-depth-callback 
     (fn [new-depth]
       (.setText depth-label (format "%+2.2f mm" new-depth))))
    (.setFont depth-label (Font. "Arial" Font/PLAIN 30))
    (.addComponentToCenter bar depth-label)
    (.addComponentToRight bar noise-label)
    (.addComponentToRight bar noise-slider)
    (.getComponent bar)))

(defn mk-frame []
  (let [frame (JFrame.)
	toolbar (UnifiedToolBar.)]
    (MacUtils/makeWindowLeopardStyle (.getRootPane frame))
    (.setSize frame 800 600)
    (.add frame (.getComponent toolbar) BorderLayout/NORTH)
    (.installWindowDraggerOnWindow toolbar frame)
    (.setLocationRelativeTo frame nil)
    (init-toolbar toolbar)
    (.add frame (mk-depth-panel) BorderLayout/EAST)
    (.add frame *canvas* BorderLayout/CENTER)
    (.add frame (mk-bottom-bar) BorderLayout/SOUTH)
    frame))

(defn show [frame]
  (.setVisible frame true)
  frame)

(do
  (def *canvas* (mk-canvas))
  (dosync
   (ref-set *depth-callbacks* []))
  (show (mk-frame))
  (update-depth 20.0))
(def fr (show (mk-frame)))

#_(.setDefaultCloseOperation frame JFrame/EXIT_ON_CLOSE)