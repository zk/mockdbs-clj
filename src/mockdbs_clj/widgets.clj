(ns mockdbs-clj.widgets
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
	   [edu.umd.cs.piccolo.util PPaintContext]))

(def frame-defaults
  {:size [1000 700]})

(defn mac-frame [& opts]
  (let [opts (merge (apply hash-map opts)
                    frame-defaults)
        {:keys [size
                toolbar
                bottombar
                east
                center
                west]} opts
        frame (JFrame.)]
    (MacUtils/makeWindowLeopardStyle (.getRootPane frame))
    (.setSize frame (first size) (second size))
    (.setLocationRelativeTo frame nil)
    (when toolbar
      (.add frame (.getComponent toolbar) BorderLayout/NORTH)
      (.installWindowDraggerOnWindow toolbar frame))
    (when bottombar
      (.add frame bottombar BorderLayout/SOUTH))
    (when east
      (.add frame east BorderLayout/EAST))
    (when center
      (.add frame center BorderLayout/CENTER))
    (when west
      (.add frame west BorderLayout/WEST))
    frame))

(defn bottom-bar [& opts]
  (let [opts (apply hash-map opts)
        bar (BottomBar. (if (= :large (:size opts))
                          BottomBarSize/LARGE
                          BottomBarSize/SMALL))
        {:keys [left center right]} opts]
    (doseq [l left]
      (.addComponentToLeft bar l))
    (doseq [c center]
      (.addComponentToCenter bar c))
    (doseq [r right]
      (.addComponentToRight bar r))
    bar))
