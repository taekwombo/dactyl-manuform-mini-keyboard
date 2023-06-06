(ns dactyl-keyboard.dactyl
  (:refer-clojure :exclude [use import])
  (:require [clojure.core.matrix :refer [array matrix mmul]]
            [scad-clj.scad :refer :all]
            [scad-clj.model :refer :all]
            [unicode-math.core :refer :all]))

(defn deg2rad [degrees]
  (* (/ degrees 180) pi))

;;;;;;;;;;;;;;;;;;;;;;
;; Shape parameters ;;
;;;;;;;;;;;;;;;;;;;;;;

(def nrows 5)
(def ncols 6)

(def α (/ π 36))                        ; curvature of the columns
(def β (/ π 52))                        ; curvature of the rows
(def centerrow (- nrows 2))             ; controls front-back tilt
(def centercol 0.5)                     ; controls left-right tilt / tenting (higher number is more tenting)
(def tenting-angle (/ π 24))            ; or, change this for more precise tenting control
; (def column-style
;   (if (> nrows 5) :orthographic :standard))  ; options include :standard, :orthographic, and :fixed
(def column-style :orthographic)
(def pinky-15u false)

(defn column-offset [column] (cond
                               (= column 2) [0 2.82 0]
                               (>= column 4) [0 -14 0]            ; original [0 -5.8 5.64]
                               :else [0 0 0]))

;; Move thumb cluster outside of the center of keyboard.
;; Try to introduce another bottom level key for index finger.
;; Reduce the angle of column - make it more flat like lily.
(def thumb-offsets [-15 -3 7])

(def keyboard-z-offset 9)               ; controls overall height; original=9 with centercol=3; use 16 for centercol=2

(def extra-width 2.5)                   ; extra space between the base of keys; original= 2
(def extra-height 1.0)                  ; original= 0.5

(def wall-z-offset -5)                 ; original=-15 length of the first downward-sloping part of the wall (negative)
(def wall-xy-offset 5)                  ; offset in the x and/or y direction for the first downward-sloping part of the wall (negative)
(def wall-thickness 2)                  ; wall thickness parameter; originally 5

;; Settings for column-style == :fixed
;; The defaults roughly match Maltron settings
;;   http://patentimages.storage.googleapis.com/EP0219944A2/imgf0002.png
;; Fixed-z overrides the z portion of the column ofsets above.
;; NOTE: THIS DOESN'T WORK QUITE LIKE I'D HOPED.
(def fixed-angles [(deg2rad 10) (deg2rad 10) 0 0 0 (deg2rad -15) (deg2rad -15)])
(def fixed-x [-41.5 -22.5 0 20.3 41.4 65.5 89.6])  ; relative to the middle finger
(def fixed-z [12.1    8.3 0  5   10.7 14.5 17.5])
(def fixed-tenting (deg2rad 0))

; If you use Cherry MX or Gateron switches, this can be turned on.
; If you use other switches such as Kailh, you should set this as false
(def create-side-nubs? true)

;;;;;;;;;;;;;;;;;;;;;;;
;; General variables ;;
;;;;;;;;;;;;;;;;;;;;;;;

(def lastrow (dec nrows))
(def cornerrow (dec lastrow))
(def lastcol (dec ncols))

;;;;;;;;;;;;;;;;;
;; Switch Hole ;;
;;;;;;;;;;;;;;;;;

(def keyswitch-height 14.2) ;; Was 14.1, then 14.25
(def keyswitch-width 14.2)

(def sa-profile-key-height 12.7)

(def plate-thickness 2)
(def side-nub-thickness 4)
(def retention-tab-thickness 1.5)
(def retention-tab-hole-thickness (- plate-thickness retention-tab-thickness))
(def mount-width (+ keyswitch-width 3))
(def mount-height (+ keyswitch-height 3))

(def single-plate
  (let [top-wall (->> (cube (+ keyswitch-width 3) 1.5 plate-thickness)
                      (translate [0
                                  (+ (/ 1.5 2) (/ keyswitch-height 2))
                                  (/ plate-thickness 2)]))
        left-wall (->> (cube 1.5 (+ keyswitch-height 3) plate-thickness)
                       (translate [(+ (/ 1.5 2) (/ keyswitch-width 2))
                                   0
                                   (/ plate-thickness 2)]))
        side-nub (->> (binding [*fn* 30] (cylinder 1 2.75))
                      (rotate (/ π 2) [1 0 0])
                      (translate [(+ (/ keyswitch-width 2)) 0 1])
                      (hull (->> (cube 1.5 2.75 side-nub-thickness)
                                 (translate [(+ (/ 1.5 2) (/ keyswitch-width 2))
                                             0
                                             (/ side-nub-thickness 2)])))
                      (translate [0 0 (- plate-thickness side-nub-thickness)]))
        plate-half (union top-wall left-wall (if create-side-nubs? (with-fn 100 side-nub)))
        top-nub (->> (cube 5 5 retention-tab-hole-thickness)
                     (translate [(+ (/ keyswitch-width 2)) 0 (/ retention-tab-hole-thickness 2)]))
        top-nub-pair (union top-nub
                            (->> top-nub
                                 (mirror [1 0 0])
                                 (mirror [0 1 0])))]
    (difference
     (union plate-half
            (->> plate-half
                 (mirror [1 0 0])
                 (mirror [0 1 0])))
     (->>
      top-nub-pair
      (rotate (/ π 2) [0 0 1])))))

;;;;;;;;;;;;;;;;
;; SA Keycaps ;;
;;;;;;;;;;;;;;;;

(def sa-length 18.25)
(def sa-double-length 37.5)
(def sa-cap {1 (let [bl2 (/ 18.5 2)
                     m (/ 17 2)
                     key-cap (hull (->> (polygon [[bl2 bl2] [bl2 (- bl2)] [(- bl2) (- bl2)] [(- bl2) bl2]])
                                        (extrude-linear {:height 0.1 :twist 0 :convexity 0})
                                        (translate [0 0 0.05]))
                                   (->> (polygon [[m m] [m (- m)] [(- m) (- m)] [(- m) m]])
                                        (extrude-linear {:height 0.1 :twist 0 :convexity 0})
                                        (translate [0 0 6]))
                                   (->> (polygon [[6 6] [6 -6] [-6 -6] [-6 6]])
                                        (extrude-linear {:height 0.1 :twist 0 :convexity 0})
                                        (translate [0 0 12])))]
                 (->> key-cap
                      (translate [0 0 (+ 5 plate-thickness)])
                      (color [220/255 163/255 163/255 1])))
             2 (let [bl2 sa-length
                     bw2 (/ 18.25 2)
                     key-cap (hull (->> (polygon [[bw2 bl2] [bw2 (- bl2)] [(- bw2) (- bl2)] [(- bw2) bl2]])
                                        (extrude-linear {:height 0.1 :twist 0 :convexity 0})
                                        (translate [0 0 0.05]))
                                   (->> (polygon [[6 16] [6 -16] [-6 -16] [-6 16]])
                                        (extrude-linear {:height 0.1 :twist 0 :convexity 0})
                                        (translate [0 0 12])))]
                 (->> key-cap
                      (translate [0 0 (+ 5 plate-thickness)])
                      (color [127/255 159/255 127/255 1])))
             1.5 (let [bl2 (/ 18.25 2)
                       bw2 (/ 27.94 2)
                       key-cap (hull (->> (polygon [[bw2 bl2] [bw2 (- bl2)] [(- bw2) (- bl2)] [(- bw2) bl2]])
                                          (extrude-linear {:height 0.1 :twist 0 :convexity 0})
                                          (translate [0 0 0.05]))
                                     (->> (polygon [[11 6] [-11 6] [-11 -6] [11 -6]])
                                          (extrude-linear {:height 0.1 :twist 0 :convexity 0})
                                          (translate [0 0 12])))]
                   (->> key-cap
                        (translate [0 0 (+ 5 plate-thickness)])
                        (color [240/255 223/255 175/255 1])))})

;;;;;;;;;;;;;;;;;;;;;;;;;
;; Placement Functions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(def columns (range 0 ncols))
(def rows (range 0 nrows))

(def cap-top-height (+ plate-thickness sa-profile-key-height))
(def row-radius (+ (/ (/ (+ mount-height extra-height) 2)
                      (Math/sin (/ α 2)))
                   cap-top-height))
(def column-radius (+ (/ (/ (+ mount-width extra-width) 2)
                         (Math/sin (/ β 2)))
                      cap-top-height))
(def column-x-delta (+ -1 (- (* column-radius (Math/sin β)))))

(defn offset-for-column [col]
  (if (and (true? pinky-15u) (= col lastcol)) 5.5 0))
(defn apply-key-geometry [translate-fn rotate-x-fn rotate-y-fn column row shape]
  (let [column-angle (* β (- centercol column))
        placed-shape (->> shape
                          (translate-fn [(offset-for-column column) 0 (- row-radius)])
                          (rotate-x-fn  (* α (- centerrow row)))
                          (translate-fn [0 0 row-radius])
                          (translate-fn [0 0 (- column-radius)])
                          (rotate-y-fn  column-angle)
                          (translate-fn [0 0 column-radius])
                          (translate-fn (column-offset column)))
        column-z-delta (* column-radius (- 1 (Math/cos column-angle)))
        placed-shape-ortho (->> shape
                                (translate-fn [0 0 (- row-radius)])
                                (rotate-x-fn  (* α (- centerrow row)))
                                (translate-fn [0 0 row-radius])
                                (rotate-y-fn  column-angle)
                                (translate-fn [(- (* (- column centercol) column-x-delta)) 0 column-z-delta])
                                (translate-fn (column-offset column)))
        placed-shape-fixed (->> shape
                                (rotate-y-fn  (nth fixed-angles column))
                                (translate-fn [(nth fixed-x column) 0 (nth fixed-z column)])
                                (translate-fn [0 0 (- (+ row-radius (nth fixed-z column)))])
                                (rotate-x-fn  (* α (- centerrow row)))
                                (translate-fn [0 0 (+ row-radius (nth fixed-z column))])
                                (rotate-y-fn  fixed-tenting)
                                (translate-fn [0 (second (column-offset column)) 0]))]
    (->> (case column-style
           :orthographic placed-shape-ortho
           :fixed        placed-shape-fixed
           placed-shape)
         (rotate-y-fn  tenting-angle)
         (translate-fn [0 0 keyboard-z-offset]))))

(defn key-place [column row shape]
  (apply-key-geometry translate
                      (fn [angle obj] (rotate angle [1 0 0] obj))
                      (fn [angle obj] (rotate angle [0 1 0] obj))
                      column row shape))

(defn rotate-around-x [angle position]
  (mmul
   [[1 0 0]
    [0 (Math/cos angle) (- (Math/sin angle))]
    [0 (Math/sin angle)    (Math/cos angle)]]
   position))

(defn rotate-around-y [angle position]
  (mmul
   [[(Math/cos angle)     0 (Math/sin angle)]
    [0                    1 0]
    [(- (Math/sin angle)) 0 (Math/cos angle)]]
   position))

(defn key-position [column row position]
  (apply-key-geometry (partial map +) rotate-around-x rotate-around-y column row position))

(def key-holes
  (apply union
         (for [column columns
               row rows
               :when (or (.contains [1 2 3] column) ; Place additional row.
                         (not= row lastrow))]
           (->> single-plate
                (key-place column row)))))

(def caps
  (apply union
         (for [column columns
               row rows
               :when (or (.contains [1 2 3] column)
                         (not= row lastrow))]
           (->> (sa-cap (if (and (true? pinky-15u) (= column lastcol)) 1.5 1))
                (key-place column row)))))

;;;;;;;;;;;;;;;;;;;;
;; Web Connectors ;;
;;;;;;;;;;;;;;;;;;;;

(def web-thickness 2)
(def post-size 0.1)
(def web-post (->> (cube post-size post-size web-thickness)
                   (translate [0 0 (+ (/ web-thickness -2)
                                      plate-thickness)])))

(def post-adj (/ post-size 2))
(def web-post-tr (translate [(- (/ mount-width 2) post-adj) (- (/ mount-height 2) post-adj) 0] web-post))
(def web-post-tl (translate [(+ (/ mount-width -2) post-adj) (- (/ mount-height 2) post-adj) 0] web-post))
(def web-post-bl (translate [(+ (/ mount-width -2) post-adj) (+ (/ mount-height -2) post-adj) 0] web-post))
(def web-post-br (translate [(- (/ mount-width 2) post-adj) (+ (/ mount-height -2) post-adj) 0] web-post))

; wide posts for 1.5u keys in the main cluster

(if (true? pinky-15u)
  (do (def wide-post-tr (translate [(- (/ mount-width 1.2) post-adj)  (- (/ mount-height  2) post-adj) 0] web-post))
      (def wide-post-tl (translate [(+ (/ mount-width -1.2) post-adj) (- (/ mount-height  2) post-adj) 0] web-post))
      (def wide-post-bl (translate [(+ (/ mount-width -1.2) post-adj) (+ (/ mount-height -2) post-adj) 0] web-post))
      (def wide-post-br (translate [(- (/ mount-width 1.2) post-adj)  (+ (/ mount-height -2) post-adj) 0] web-post)))
  (do (def wide-post-tr web-post-tr)
      (def wide-post-tl web-post-tl)
      (def wide-post-bl web-post-bl)
      (def wide-post-br web-post-br)))

(defn triangle-hulls [& shapes]
  (apply union
         (map (partial apply hull)
              (partition 3 1 shapes))))

(def connectors
  (apply union
         (concat
          ;; Row connections
          (for [column (range 0 (dec ncols))
                row (range 0 lastrow)]
            (triangle-hulls
             (key-place (inc column) row web-post-tl)
             (key-place column row web-post-tr)
             (key-place (inc column) row web-post-bl)
             (key-place column row web-post-br)))
          ;; Row 1 2
          (for [column (range 1 3)
                row (range 4 5)]
            (triangle-hulls
             (key-place (inc column) row web-post-tl)
             (key-place column row web-post-tr)
             (key-place (inc column) row web-post-bl)
             (key-place column row web-post-br)))

          ;; Column connections
          (for [column columns
                row (range 0 cornerrow)]
            (triangle-hulls
             (key-place column row web-post-bl)
             (key-place column row web-post-br)
             (key-place column (inc row) web-post-tl)
             (key-place column (inc row) web-post-tr)))
          ;; Column 1 2
          (for [column [1 2 3]
                row (range 3 4)]
            (triangle-hulls
             (key-place column row web-post-bl)
             (key-place column row web-post-br)
             (key-place column (inc row) web-post-tl)
             (key-place column (inc row) web-post-tr)))

          ;; Diagonals for additional rows
          (for [column (range 1 3)
                row (range 3 4)]
            (triangle-hulls
             (key-place column row web-post-br)
             (key-place column (inc row) web-post-tr)
             (key-place (inc column) row web-post-bl)
             (key-place (inc column) (inc row) web-post-tl)))

          ;; Diagonal connections
          (for [column (range 0 (dec ncols))
                row (range 0 cornerrow)]
            (triangle-hulls
             (key-place column row web-post-br)
             (key-place column (inc row) web-post-tr)
             (key-place (inc column) row web-post-bl)
             (key-place (inc column) (inc row) web-post-tl))))))

;;;;;;;;;;;;
;; Thumbs ;;
;;;;;;;;;;;;

(def thumborigin ; Moves thumb cluster origin.
  (map + (key-position 1 cornerrow [(/ mount-width 2) (- (/ mount-height 2)) 0])
       thumb-offsets))

;   TL  TM  TR
;           BR

(defn thumb-tr-place [shape]
  (->> shape
       (rotate (deg2rad  9) [1 0 0])
       (rotate (deg2rad -5) [0 1 0])
       (rotate (deg2rad  5) [0 0 1]) ; original 10
       (translate thumborigin)
       (translate [-12.5 -9 -6]))) ; original 1.5u  (translate [-12 -16 3])
(defn thumb-tm-place [shape]
  (->> shape
       (rotate (deg2rad  6) [1 0 0])
       (rotate (deg2rad 0) [0 1 0])
       (rotate (deg2rad  20) [0 0 1]) ; original 10
       (translate thumborigin)
       (translate [-34 -14 -7.5]))) ; original 1.5u (translate [-32 -15 -2])))
(defn thumb-tl-place [shape]
  (->> shape
       (rotate (deg2rad  6) [1 0 0])
       (rotate (deg2rad  5) [0 1 0])
       (rotate (deg2rad 35) [0 0 1])
       (translate thumborigin)
       (translate [-52 -27 -7]))) ;        (translate [-51 -25 -12])))

; Bottom Middle
(defn thumb-br-place [shape]
  (->> shape
       (rotate (deg2rad  5) [1 0 0])
       (rotate (deg2rad -7) [0 1 0])
       (rotate (deg2rad  5) [0 0 1])
       (translate thumborigin)
       (translate [4 -28 -7])))


(defn thumb-1x-layout [shape]
  (union
   (thumb-br-place shape)
   (thumb-tm-place shape)
   (thumb-tl-place shape)))

(defn thumb-15x-layout [shape]
  (union
   (thumb-tr-place shape)))

(def larger-plate
  (let [plate-height (- (/ (- sa-double-length mount-height) 3) 0.5)
        top-plate (->> (cube mount-width plate-height web-thickness)
                       (translate [0 (/ (+ plate-height mount-height) 2)
                                   (- plate-thickness (/ web-thickness 2))]))]
    (union top-plate (mirror [0 1 0] top-plate))))

(def thumbcaps
  (union
   (thumb-1x-layout (sa-cap 1))
   (thumb-15x-layout (rotate (/ π 2) [0 0 1] (sa-cap 1)))))

(def thumb
  (union
   (thumb-1x-layout single-plate)
   (thumb-15x-layout single-plate)
   ; (thumb-15x-layout larger-plate)
))

(def thumb-post-tr (translate [(- (/ mount-width 2) post-adj)  (- (/ mount-height  2) post-adj) 0] web-post))
(def thumb-post-tl (translate [(+ (/ mount-width -2) post-adj) (- (/ mount-height  2) post-adj) 0] web-post))
(def thumb-post-bl (translate [(+ (/ mount-width -2) post-adj) (+ (/ mount-height -2) post-adj) 0] web-post))
(def thumb-post-br (translate [(- (/ mount-width 2) post-adj)  (+ (/ mount-height -2) post-adj) 0] web-post))

(def thumb-connectors
  (union
    (triangle-hulls     ;; TR -> [Col:1 Row:5]
      (thumb-tr-place web-post-br)
      (thumb-tr-place web-post-tr)
      (key-place 1 lastrow web-post-bl)
      (key-place 1 lastrow web-post-tl))
    (triangle-hulls     ;; TR -> [Col:0 Row:4]
      (thumb-tr-place web-post-tl)
      (thumb-tr-place web-post-tr)
      (key-place 0 cornerrow web-post-bl)
      (key-place 0 cornerrow web-post-br))
    (triangle-hulls     ;; TR - TM - [Col:0,1 Row: 4]
      (thumb-tr-place web-post-tr)
      (key-place 1 lastrow web-post-tl)
      (key-place 0 cornerrow web-post-br)
      (key-place 1 cornerrow web-post-bl))
    (triangle-hulls     ;; TM -> TR
      (thumb-tm-place web-post-tr)
      (thumb-tm-place web-post-br)
      (thumb-tr-place thumb-post-tl)
      (thumb-tr-place thumb-post-bl))
    (triangle-hulls     ;; TL - TM
      (thumb-tl-place web-post-tr)
      (thumb-tl-place web-post-br)
      (thumb-tm-place web-post-tl)
      (thumb-tm-place web-post-bl))
    (triangle-hulls     ;; TR - BR - [Col:1 Row: 4]
      (thumb-tr-place (translate [(/ keyswitch-width -2) 0 0] web-post-br))
      (thumb-br-place web-post-tl)
      (thumb-tr-place web-post-br)
      (thumb-br-place web-post-tr)
      (key-place 1 lastrow web-post-bl)
      (key-place 1 lastrow web-post-br))
   (triangle-hulls
     (key-place 4 cornerrow web-post-bl)
     (key-place 3 cornerrow web-post-br)
     (key-place 3 lastrow web-post-tr))
   (triangle-hulls
    (key-place 1 cornerrow web-post-br)
    (key-place 2 lastrow web-post-tl)
    (key-place 2 cornerrow web-post-bl)
    (key-place 2 lastrow web-post-tr)
    (key-place 2 cornerrow web-post-br)
    (key-place 3 cornerrow web-post-bl))
   (triangle-hulls
    (key-place 3 lastrow web-post-tr)
    (key-place 3 lastrow web-post-br)
    (key-place 3 lastrow web-post-tr)
    (key-place 4 cornerrow web-post-bl))))

;;;;;;;;;;
;; Case ;;
;;;;;;;;;;

(defn bottom [height p]
  (->> (project p)
       (extrude-linear {:height height :twist 0 :convexity 0})
       (translate [0 0 (- (/ height 2) 10)])))

(defn bottom-hull [& p]
  (hull p (bottom 0.001 p)))

(def left-wall-x-offset 5) ; original 10
(def left-wall-z-offset  3) ; original 3

(defn left-key-position [row direction]
  (map - (key-position 0 row [(* mount-width -0.5) (* direction mount-height 0.5) 0]) [left-wall-x-offset 0 left-wall-z-offset]))

(defn left-key-place [row direction shape]
  (translate (left-key-position row direction) shape))

(defn wall-locate1 [dx dy] [(* dx wall-thickness) (* dy wall-thickness) -1])
(defn wall-locate2 [dx dy] [(* dx wall-xy-offset) (* dy wall-xy-offset) wall-z-offset])
(defn wall-locate3 [dx dy] [(* dx (+ wall-xy-offset wall-thickness)) (* dy (+ wall-xy-offset wall-thickness)) wall-z-offset])

(defn wall-brace [place1 dx1 dy1 post1 place2 dx2 dy2 post2]
  (union
   (hull
    (place1 post1)
    (place1 (translate (wall-locate1 dx1 dy1) post1))
    (place1 (translate (wall-locate2 dx1 dy1) post1))
    (place1 (translate (wall-locate3 dx1 dy1) post1))
    (place2 post2)
    (place2 (translate (wall-locate1 dx2 dy2) post2))
    (place2 (translate (wall-locate2 dx2 dy2) post2))
    (place2 (translate (wall-locate3 dx2 dy2) post2)))
   (bottom-hull
    (place1 (translate (wall-locate2 dx1 dy1) post1))
    (place1 (translate (wall-locate3 dx1 dy1) post1))
    (place2 (translate (wall-locate2 dx2 dy2) post2))
    (place2 (translate (wall-locate3 dx2 dy2) post2)))))

(defn key-wall-brace [x1 y1 dx1 dy1 post1 x2 y2 dx2 dy2 post2]
  (wall-brace (partial key-place x1 y1) dx1 dy1 post1
              (partial key-place x2 y2) dx2 dy2 post2))

(def right-wall
  (let [tr web-post-tr br web-post-br]
    (union (key-wall-brace lastcol 0 0 1 tr lastcol 0 1 0 tr)
           (for [y (range 0 lastrow)] (key-wall-brace lastcol y 1 0 tr lastcol y 1 0 br))
           (for [y (range 1 lastrow)] (key-wall-brace lastcol (dec y) 1 0 br lastcol y 1 0 tr))
           (key-wall-brace lastcol cornerrow 0 -1 br lastcol cornerrow 1 0 br))))

(def left-wall
  (let [tr web-post-tl br web-post-bl]
    (union 
      (key-wall-brace 0 0 0 1 tr 0 0 -1 0 tr)
      (for [y (range 0 lastrow)] (key-wall-brace 0 y -1 0 tr 0 y -1 0 br))
      (for [y (range 1 lastrow)] (key-wall-brace 0 (dec y) -1 0 br 0 y -1 0 tr)))))

(def thumb-key-top-left-walls
  (union
    ; Walls surrounding the key  
    (wall-brace thumb-tl-place 0 -1 web-post-bl thumb-tl-place 0 -1 web-post-br)
    (wall-brace thumb-tl-place -1 -1 web-post-bl thumb-tl-place 1 -1 web-post-bl)
    (wall-brace thumb-tl-place -1 1 web-post-tl thumb-tl-place -1 -1 web-post-bl)
    (wall-brace thumb-tl-place -1 1 web-post-tl thumb-tl-place 0 1 web-post-tr)
    ; Walls between TL and TM key
    (wall-brace thumb-tl-place 0 1 web-post-tr thumb-tm-place 0 1 web-post-tl)
    (wall-brace thumb-tl-place 0 -1 web-post-br thumb-tm-place 0 -1 web-post-bl)
))

(defn tm-join [pos]     ; Thumb Middle Top - between top right and top left.
  (translate [(/ keyswitch-width -2) 0 0] pos))

(def thumb-key-top-middle-walls
  (union
    ; Bottom walls
    (wall-brace thumb-tm-place 0 -1 web-post-bl thumb-tm-place 0 -1 web-post-br)
    ; Top walls
    (wall-brace thumb-tm-place 0 1 (tm-join web-post-tr) thumb-tm-place 0 1 web-post-tl)
    (bottom-hull
      (thumb-tm-place (translate (wall-locate3 0 1) (tm-join web-post-tr)))
      (thumb-tm-place (translate (wall-locate2 0 1) (tm-join web-post-tr)))
      (thumb-tm-place (translate (wall-locate2 1 1) (tm-join web-post-tr)))
      ((partial key-place 0 cornerrow) (translate (wall-locate2 -1 0) web-post-bl))
      ((partial key-place 0 cornerrow) (translate (wall-locate3 -1 0) web-post-bl))
      ; Could have been better.
      (key-place 0 cornerrow (translate (wall-locate3 -0.7 -0.2) web-post-bl))
    )
    (hull ; Connection with Left Wall
      (thumb-tm-place web-post-tr)
      (thumb-tm-place (tm-join web-post-tr))
      (thumb-tm-place (translate (wall-locate1 0 1) (tm-join web-post-tr)))
      (thumb-tm-place (translate (wall-locate2 0 1) (tm-join web-post-tr)))
      (thumb-tm-place (translate (wall-locate3 0 1) (tm-join web-post-tr)))
      ((partial key-place 0 cornerrow) (translate (wall-locate3 -1 0) web-post-bl))
      ((partial key-place 0 cornerrow) (translate (wall-locate2 -1 0) web-post-bl))
    )
    (hull
      (thumb-tm-place web-post-tr)
      ((partial key-place 0 cornerrow) (translate (wall-locate3 -1 0) web-post-bl))
      ((partial key-place 0 cornerrow) (translate (wall-locate2 -1 0) web-post-bl))
      ((partial key-place 0 cornerrow) (translate (wall-locate1 -1 0) web-post-bl))
      (key-place 0 cornerrow web-post-bl)
      (thumb-tr-place web-post-tl)
    )
))

(def thumb-key-top-right-walls
  (union
    ; Connect with Top Middle
    (wall-brace thumb-tm-place 0 -1 web-post-br thumb-tr-place 0 -1 web-post-bl)
    ; Connect with Bottom Right
    (wall-brace thumb-tr-place 0 -1 web-post-bl thumb-tr-place 0 -1 (translate [(/ keyswitch-width -2) 0 0] web-post-br))
    (wall-brace thumb-tr-place 0 -1 (translate [(/ keyswitch-width -2) 0 0] web-post-br)
                thumb-br-place -0.5 -1 web-post-tl)
))

(def thumb-key-bottom-right-walls
  (union
    (wall-brace thumb-br-place -0.5 -0.5 web-post-bl thumb-br-place -0.5 -1 web-post-tl)  ; Left
    (wall-brace thumb-br-place -0.5 -0.5 web-post-bl thumb-br-place 0.5 -0.5 web-post-br) ; Bottom
    (wall-brace thumb-br-place 0.5 -0.5 web-post-br thumb-br-place 0.5 -1 web-post-tr)    ; Right
))

(def case-walls
  (union
    right-wall
    left-wall
    ; back wall
    ; Before Ring Finger
    (for [x (range 0 4)] (key-wall-brace x 0 0 1 web-post-tl x       0 0 1 web-post-tr))
    (for [x (range 1 4)] (key-wall-brace x 0 0 1 web-post-tl (dec x) 0 0 1 web-post-tr))
    ; After Ring Finger
    (for [x (range 5 ncols)] (key-wall-brace x 0 0 1 web-post-tl x       0 0 1 web-post-tr))
    (for [x (range 5 ncols)] (key-wall-brace x 0 0 1 web-post-tl (dec x) 0 0 1 web-post-tr))
    ; Thicker Connection between pinky and ring finger.
    (key-wall-brace 4 0 0.5 1 web-post-tl 3 0 0 1 web-post-tr)
    (key-wall-brace 4 0 0.5 1 web-post-tl 4       0 0 1 web-post-tr)
    ; front wall
    (wall-brace
      thumb-br-place 0.5 -1 web-post-tr (partial key-place 1 lastrow) 0.5 -1 web-post-br)
    (key-wall-brace 1 lastrow 0.5 -1 web-post-br 2 lastrow 0.5 -1 web-post-bl)
    (key-wall-brace 2 lastrow 0 -1 web-post-br 2 lastrow 0.5 -1 web-post-bl)
    (key-wall-brace 2 lastrow 0 -1 web-post-br 3 lastrow -0.5 -1 web-post-bl)
    (key-wall-brace 3 lastrow -0.5 -1 web-post-bl 3 lastrow 0.5 -1 web-post-br)
    (key-wall-brace 3 lastrow 0.5 -1 web-post-br 4 cornerrow 0.5 -1 web-post-bl)
    (key-wall-brace 4 cornerrow 0.5 -1 web-post-bl 4 cornerrow 0 -1 web-post-br)
    (for [x (range 5 ncols)] (key-wall-brace x cornerrow 0 -1 web-post-bl x       cornerrow 0 -1 web-post-br))
    (for [x (range 5 ncols)] (key-wall-brace x cornerrow 0 -1 web-post-bl (dec x) cornerrow 0 -1 web-post-br))
    ; thumb walls
    thumb-key-top-left-walls
    thumb-key-top-middle-walls
    thumb-key-top-right-walls
    thumb-key-bottom-right-walls
  )
)

(def usb-holder-ref (key-position 0 0 (map - (wall-locate2  0  -1) [0 (/ mount-height 2) 0])))
(def usb-holder-position (map + [17 19.3 0] [(first usb-holder-ref) (second usb-holder-ref) 2]))
(def usb-holder-cube   (cube 15 12 2))
(def usb-holder-space  (translate (map + usb-holder-position [0 (* -1 wall-thickness) 1]) usb-holder-cube))
(def usb-holder-holder (translate usb-holder-position (cube 19 12 4)))

(def usb-jack (translate (map + usb-holder-position [0 10 3]) (cube 8.1 20 3.1)))

(def pro-micro-position (map + (key-position 0 1 (wall-locate3 -1 0)) [-6 2 -15]))
(def pro-micro-space-size [4 10 12]) ; z has no wall;
(def pro-micro-wall-thickness 2)
(def pro-micro-holder-size [(+ pro-micro-wall-thickness (first pro-micro-space-size)) (+ pro-micro-wall-thickness (second pro-micro-space-size)) (last pro-micro-space-size)])
(def pro-micro-space
  (->> (cube (first pro-micro-space-size) (second pro-micro-space-size) (last pro-micro-space-size))
       (translate [(- (first pro-micro-position) (/ pro-micro-wall-thickness 2)) (- (second pro-micro-position) (/ pro-micro-wall-thickness 2)) (last pro-micro-position)])))
(def pro-micro-holder
  (difference
   (->> (cube (first pro-micro-holder-size) (second pro-micro-holder-size) (last pro-micro-holder-size))
        (translate [(first pro-micro-position) (second pro-micro-position) (last pro-micro-position)]))
   pro-micro-space))

(def trrs-holder-size [6.2 10 2]) ; trrs jack PJ-320A
(def trrs-holder-hole-size [6.2 10 6]) ; trrs jack PJ-320A
(def trrs-holder-position  (map + usb-holder-position [-13.6 0 0]))
(def trrs-holder-thickness 2)
(def trrs-holder-thickness-2x (* 2 trrs-holder-thickness))
(def trrs-holder
  (union
   (->> (cube (+ (first trrs-holder-size) trrs-holder-thickness-2x) (+ trrs-holder-thickness (second trrs-holder-size)) (+ (last trrs-holder-size) trrs-holder-thickness))
        (translate [(first trrs-holder-position) (second trrs-holder-position) (/ (+ (last trrs-holder-size) trrs-holder-thickness) 2)]))))
(def trrs-holder-hole
  (union

  ; circle trrs hole
   (->>
    (->> (binding [*fn* 30] (cylinder 2.55 20))) ; 5mm trrs jack
    (rotate (deg2rad  90) [1 0 0])
    (translate [(first trrs-holder-position) (+ (second trrs-holder-position) (/ (+ (second trrs-holder-size) trrs-holder-thickness) 2)) (+ 3 (/ (+ (last trrs-holder-size) trrs-holder-thickness) 2))])) ;1.5 padding

  ; rectangular trrs holder
   (->> (apply cube trrs-holder-hole-size) (translate [(first trrs-holder-position) (+ (/ trrs-holder-thickness -2) (second trrs-holder-position)) (+ (/ (last trrs-holder-hole-size) 2) trrs-holder-thickness)]))))

(defn screw-insert-shape [bottom-radius top-radius height]
  (union
   (->> (binding [*fn* 30]
          (cylinder [bottom-radius top-radius] height)))
   (translate [0 0 (/ height 2)] (->> (binding [*fn* 30] (sphere top-radius))))))

(defn screw-insert [column row bottom-radius top-radius height offset]
  (let [shift-right   (= column lastcol)
        shift-left    (= column 0)
        shift-up      (and (not (or shift-right shift-left)) (= row 0))
        shift-down    (and (not (or shift-right shift-left)) (>= row lastrow))
        position      (if shift-up     (key-position column row (map + (wall-locate2  0  1) [0 (/ mount-height 2) 0]))
                          (if shift-down  (key-position column row (map - (wall-locate2  0 -1) [0 (/ mount-height 2) 0]))
                              (if shift-left (map + (left-key-position row 0) (wall-locate3 -1 0))
                                  (key-position column row (map + (wall-locate2  1  0) [(/ mount-width 2) 0 0])))))]
    (->> (screw-insert-shape bottom-radius top-radius height)
         (translate (map + offset [(first position) (second position) (/ height 2)])))))

(defn screw-insert-all-shapes [bottom-radius top-radius height]
  (union 
    (screw-insert 0 0                 bottom-radius top-radius height [9 8 0])
    (screw-insert 0 lastrow           bottom-radius top-radius height [8 12 0])
    (screw-insert lastcol lastrow     bottom-radius top-radius height [-3 12 0])
    (screw-insert lastcol 0           bottom-radius top-radius height [-3 6 0])
    (screw-insert 2 lastrow           bottom-radius top-radius height [-10 3 0])))

; Hole Depth Y: 4.4
(def screw-insert-height 4)

; Hole Diameter C: 4.1-4.4
(def screw-insert-bottom-radius (/ 4.4 2))
(def screw-insert-top-radius (/ 4.4 2))
(def screw-insert-holes  (screw-insert-all-shapes screw-insert-bottom-radius screw-insert-top-radius screw-insert-height))

; Wall Thickness W:\t1.65
(def screw-insert-outers (screw-insert-all-shapes (+ screw-insert-bottom-radius 1.65) (+ screw-insert-top-radius 1.65) (+ screw-insert-height 1.5)))
(def screw-insert-screw-holes  (screw-insert-all-shapes 1.7 1.7 350))

(def pinky-connectors
  (apply union
         (concat
          ;; Row connections
          (for [row (range 0 lastrow)]
            (triangle-hulls
             (key-place lastcol row web-post-tr)
             (key-place lastcol row wide-post-tr)
             (key-place lastcol row web-post-br)
             (key-place lastcol row wide-post-br)))

          ;; Column connections
          (for [row (range 0 cornerrow)]
            (triangle-hulls
             (key-place lastcol row web-post-br)
             (key-place lastcol row wide-post-br)
             (key-place lastcol (inc row) web-post-tr)
             (key-place lastcol (inc row) wide-post-tr)))
          ;;
)))

(def pinky-walls
  (union
   (key-wall-brace lastcol cornerrow 0 -1 web-post-br lastcol cornerrow 0 -1 wide-post-br)
   (key-wall-brace lastcol 0 0 1 web-post-tr lastcol 0 0 1 wide-post-tr)))

(def model-right
  (difference
    (union
      key-holes
      connectors
      thumb
      thumb-connectors
      (difference
        (union
          case-walls
          screw-insert-outers
        )
        usb-jack
        screw-insert-holes
      )
    )
    (translate [0 0 -20] (cube 350 350 40))))

(spit "things/right.scad"
      (write-scad model-right))

(spit "things/left.scad"
      (write-scad (mirror [-1 0 0] model-right)))

(spit "things/right-test.scad"
      (write-scad
       (difference
        (union
         key-holes
         pinky-connectors
         connectors
         thumb
         thumb-connectors
         case-walls
         thumbcaps
         caps)

        (translate [0 0 -20] (cube 350 350 40)))))

(spit "things/right-plate-cut.scad"
  (write-scad
    (cut
      (translate [0 0 -0.1]
        (difference
          (union
            case-walls
            screw-insert-outers
          )
          (translate [0 0 -10] screw-insert-screw-holes)
        )
      )
    )
  )
)

(def filled-plate
  (->> (cube mount-height mount-width plate-thickness)
       (translate [0 0 (/ plate-thickness 2)])
  )
)

(def key-fills
  (apply union
         (for [column columns
               row rows
               :when (or (.contains [1 2 3] column) ; Place additional row.
                         (not= row lastrow))]
           (->> filled-plate
                (key-place column row)))))

(def thumb-fills
  (union
    (thumb-tl-place filled-plate)
    (thumb-tm-place filled-plate)
    (thumb-tr-place filled-plate)
    (thumb-br-place filled-plate)
  )
)

(spit "things/right-plate.scad"
  (write-scad
    (project
      (translate [0 0 -0.1]
        (difference
          (union
            case-walls
            key-holes
            key-fills
            thumb-fills
            connectors
            thumb
            thumb-connectors
            screw-insert-outers
          )
          (translate [0 0 -10] screw-insert-screw-holes)
        )
      )
    )
  )
)

(defn -main [dum] 1)  ; dummy to make it easier to batch
