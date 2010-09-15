(ns mockdbs-clj.core
  (:import [javax.swing JLabel JPanel JFrame JTextField]
	   [javax.swing.event DocumentListener]
	   [java.awt GridBagLayout GridBagConstraints Insets]
	   [java.awt BorderLayout Color Font] 
	   [javax.swing JFrame JButton JSlider JPanel JLabel]
	   [com.explodingpixels.macwidgets MacUtils UnifiedToolBar MacWidgetFactory BottomBarSize BottomBar]
	   [edu.umd.cs.piccolo PCanvas PLayer PNode]
	   [edu.umd.cs.piccolo.nodes PPath]
	   [edu.umd.cs.piccolo.event PZoomEventHandler PInputEvent]
	   [edu.umd.cs.piccolo.util PPaintContext]))

;;import edu.umd.cs.piccolo.event.PBasicInputEventHandler;
;;import edu.umd.cs.piccolo.event.PInputEvent;
;;import edu.umd.cs.piccolo.event.PInputEventListener;
;;import edu.umd.cs.piccolo.util.PPaintContext;

(def neurons (ref #{}))

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

(defn get-neurons [] @neurons)

(defn add-neuron [n]
  (dosync
   (alter neurons conj n)))

(defn remove-neuron [n]
  (dosync
   (alter neurons disj n)))

(defn clear-neurons []
  (dosync
   (ref-set neurons #{})))


(defn mk-button [text]
  (doto (JButton. text)
    (.putClientProperty "JButton.buttonType" "textured")))

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
  (let [thal (mk-button "Thalamus")
	stn (mk-button "STN")
	snr (mk-button "SNr")]
    (.addComponentToRight toolbar thal)
    (.addComponentToRight toolbar stn)
    (.addComponentToRight toolbar snr)))

(defn mk-depth-panel []
  (let [panel (JPanel.)
	slider (JSlider. JSlider/VERTICAL)]
    (.setLayout panel (BorderLayout.))
    (.setBackground panel (Color/WHITE))
    (.add panel slider BorderLayout/CENTER)
    (.setMaximum slider 20000)
    (.setMinimum slider -10000)
    (.setValue slider 20000)
    (.setFocusable slider true)

    panel))

(defn mk-neuron []
  (PPath/createEllipse 25 25 12.5 12.5))

(defn mk-canvas []
  (let [canvas (PCanvas.)]
    (.setZoomEventHandler canvas (mk-zoomhandler))
    (.addChild (.getLayer canvas) (mk-neuron))
    (.setAnimatingRenderQuality canvas PPaintContext/HIGH_QUALITY_RENDERING)
    (.setInteractingRenderQuality canvas PPaintContext/HIGH_QUALITY_RENDERING)
    canvas))

(defn mk-bottom-bar []
  (let [bar (BottomBar. BottomBarSize/LARGE)
	depth (JLabel. "0.00 mm")
	noise-slider (JSlider. JSlider/HORIZONTAL)
	noise-label (JLabel. "Noise")]
    (.setFont depth (Font. "Arial" Font/PLAIN 30))
    (.addComponentToCenter bar depth)
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
    (.add frame (mk-canvas) BorderLayout/CENTER)
    (.add frame (mk-bottom-bar) BorderLayout/SOUTH)
    frame))

(defn show [frame]
  (.setVisible frame true)
  frame)

(def fr (show (mk-frame)))

#_(.setDefaultCloseOperation frame JFrame/EXIT_ON_CLOSE)