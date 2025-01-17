;; Things to hmm about:
;; - increase size of reset switch hole

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

(def α (/ π 32))                        ; curvature of the columns
(def β (/ π 52))                        ; curvature of the rows
(def centerrow (- nrows 2))             ; controls front-back tilt
(def centercol 0.5)                     ; controls left-right tilt / tenting (higher number is more tenting)
(def tenting-angle (/ π 24))            ; or, change this for more precise tenting control
(def column-style :orthographic)
(defn column-offset [column] (cond
                               (= column 2) [0 2.82 0]
                               (>= column 4) [0 -12 1]
                               :else [0 0 0]))

;; Move thumb cluster outside of the center of keyboard.
;; Try to introduce another bottom level key for index finger.
;; Reduce the angle of column - make it more flat like lily.
(def thumb-offsets [-16 -3 7])

(def keyboard-z-offset 10)               ; controls overall height; original=9 with centercol=3; use 16 for centercol=2

(def extra-width 1.0)                   ; extra space between the base of keys; original= 2
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

(def keyswitch-height 14.0)
(def keyswitch-width 14.0)

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
  (if (= col lastcol) 5.5 0))
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
               :when (or (.contains [2 3] column)
                         (not= row lastrow))]
           (->> single-plate
                (key-place column row)))))

(def caps
  (apply union
         (for [column columns
               row rows
               :when (or (.contains [2 3] column)
                         (not= row lastrow))]
           (->> (sa-cap 1)
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

(do (def wide-post-tr web-post-tr)
    (def wide-post-tl web-post-tl)
    (def wide-post-bl web-post-bl)
    (def wide-post-br web-post-br))

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
          (for [column (range 2 3) row (range 4 5)]
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
          (for [column [2 3]
                row (range 3 4)]
            (triangle-hulls
             (key-place column row web-post-bl)
             (key-place column row web-post-br)
             (key-place column (inc row) web-post-tl)
             (key-place column (inc row) web-post-tr)))

          ;; Diagonals for additional rows
          (for [column (range 2 3)
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

(defn thumb-tm-place [shape]
  (->> shape
       (rotate (deg2rad 12) [1 0 0])
       (rotate (deg2rad  0) [0 1 0])
       (rotate (deg2rad  0) [0 0 1])
       (translate thumborigin)
       (translate [-12.5 -9 -6])))
(defn thumb-tl-place [shape]
  (->> shape
       (rotate (deg2rad 12) [1 0 0])
       (rotate (deg2rad 5) [0 1 0])
       (rotate (deg2rad  10) [0 0 1])
       (translate thumborigin)
       (translate [-34 -18 -6])))
(defn thumb-tr-place [shape]
  (->> shape
       (rotate (deg2rad 12) [1 0 0])
       (rotate (deg2rad -3) [0 1 0])
       (rotate (deg2rad  0) [0 0 1])
       (translate thumborigin)
       (translate [8 -9 -6])))

(defn thumb-1x-layout [shape]
  (union
   (thumb-tr-place shape)
   (thumb-tm-place shape)
   (thumb-tl-place shape)))

(def thumbcaps
  (union
   (thumb-1x-layout (sa-cap 1))))

(def thumb
   (thumb-1x-layout single-plate)
)

(def thumb-post-tr (translate [(- (/ mount-width 2) post-adj)  (- (/ mount-height  2) post-adj) 0] web-post))
(def thumb-post-tl (translate [(+ (/ mount-width -2) post-adj) (- (/ mount-height  2) post-adj) 0] web-post))
(def thumb-post-bl (translate [(+ (/ mount-width -2) post-adj) (+ (/ mount-height -2) post-adj) 0] web-post))
(def thumb-post-br (translate [(- (/ mount-width 2) post-adj)  (+ (/ mount-height -2) post-adj) 0] web-post))

(def thumb-connectors
  (union
    ;; TL -> TR
    (triangle-hulls
      (thumb-tl-place web-post-tr)
      (thumb-tl-place web-post-br)
      (thumb-tm-place thumb-post-tl)
      (thumb-tm-place thumb-post-bl)
    )
    ;; TM -> Right
    (triangle-hulls
      (thumb-tm-place web-post-tr)
      (thumb-tm-place web-post-br)
      (thumb-tr-place web-post-tl)
      (thumb-tr-place web-post-bl)
    )
    ;; TM -> Up
    (triangle-hulls
      (thumb-tm-place web-post-tl)
      (thumb-tm-place web-post-tr)
      (key-place 0 cornerrow web-post-bl)
      (key-place 0 cornerrow web-post-br)
    )
    ;; TM -> Up Right
    (triangle-hulls
      (thumb-tm-place web-post-tr)
      (thumb-tr-place web-post-tl)
      (key-place 0 cornerrow web-post-br)
      (key-place 1 cornerrow web-post-bl)
    )
    ;; TR -> Right
    (triangle-hulls
      (thumb-tr-place web-post-br)
      (thumb-tr-place web-post-tr)
      (key-place 2 lastrow web-post-bl)
      (key-place 2 lastrow web-post-tl)
    )
    ;; TR -> Up
    (triangle-hulls
      (thumb-tr-place web-post-tl)
      (thumb-tr-place web-post-tr)
      (key-place 1 cornerrow web-post-bl)
      (key-place 1 cornerrow web-post-br)
    )
    ;; TR -> Up Right
    (triangle-hulls
      (thumb-tr-place web-post-tr)
      (key-place 1 cornerrow web-post-br)
      (key-place 2 lastrow web-post-tl)
      (key-place 2 cornerrow web-post-bl)
    )
    (triangle-hulls
      (key-place 4 cornerrow web-post-bl)
      (key-place 3 cornerrow web-post-br)
      (key-place 3 lastrow web-post-tr)
    )
    (triangle-hulls
      (key-place 3 lastrow web-post-tr)
      (key-place 3 lastrow web-post-br)
      (key-place 3 lastrow web-post-tr)
      (key-place 4 cornerrow web-post-bl)
    )
  )
)

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
      (key-wall-brace 0 0 0 1 tr 0 0 -0.5 1 tr)
      (key-wall-brace 0 0 -0.5 1 tr 0 0 -1 0 tr)
      (for [y (range 0 lastrow)] (key-wall-brace 0 y -1 0 tr 0 y -1 0 br))
      (for [y (range 1 lastrow)] (key-wall-brace 0 (dec y) -1 0 br 0 y -1 0 tr)))))

(defn tm-join [pos]     ; Thumb Middle Top - between top right and top left.
  (translate [(/ keyswitch-width -2) 0 0] pos))

(def thumb-key-top-left-walls
  (union
    ; Bottom walls
    (wall-brace thumb-tl-place 0 -1 web-post-bl thumb-tl-place 0 -1 web-post-br)
    (wall-brace thumb-tl-place -1 0 web-post-bl thumb-tl-place 0 -1 web-post-bl)
    ; Left walls
    (wall-brace thumb-tl-place -1 0 web-post-tl thumb-tl-place -1 0 web-post-bl)
    (wall-brace thumb-tl-place 0 1 web-post-tl thumb-tl-place -1 0 web-post-tl)
    ; Top walls
    (wall-brace thumb-tl-place 0 1 (tm-join web-post-tr) thumb-tl-place 0 1 web-post-tl)
    (bottom-hull
      (thumb-tl-place (translate (wall-locate3 0 1) (tm-join web-post-tr)))
      (thumb-tl-place (translate (wall-locate2 0 1) (tm-join web-post-tr)))
      (thumb-tl-place (translate (wall-locate2 0 1) (tm-join web-post-tr)))
      ((partial key-place 0 cornerrow) (translate (wall-locate2 -1 0) web-post-bl))
      ((partial key-place 0 cornerrow) (translate (wall-locate3 -1 0) web-post-bl))
    )
    (hull ; Connection with Left Wall
      (thumb-tl-place web-post-tr)
      (thumb-tl-place (tm-join web-post-tr))
      (thumb-tl-place (translate (wall-locate1 0 1) (tm-join web-post-tr)))
      (thumb-tl-place (translate (wall-locate2 0 1) (tm-join web-post-tr)))
      (thumb-tl-place (translate (wall-locate3 0 1) (tm-join web-post-tr)))
      ((partial key-place 0 cornerrow) (translate (wall-locate3 -1 0) web-post-bl))
      ((partial key-place 0 cornerrow) (translate (wall-locate2 -1 0) web-post-bl))
    )
    (hull
      (thumb-tl-place web-post-tr)
      ((partial key-place 0 cornerrow) (translate (wall-locate3 -1 0) web-post-bl))
      ((partial key-place 0 cornerrow) (translate (wall-locate2 -1 0) web-post-bl))
      ((partial key-place 0 cornerrow) (translate (wall-locate1 -1 0) web-post-bl))
      (key-place 0 cornerrow web-post-bl)
      (thumb-tm-place web-post-tl)
    )
))

(def thumb-key-top-right-walls
  (union
    (wall-brace thumb-tr-place 0 -1 web-post-bl thumb-tr-place 0 -1 web-post-br)
    (wall-brace thumb-tr-place 0 -1 web-post-br thumb-tr-place 0 -1 web-post-bl)
    (wall-brace thumb-tr-place 0 -1 web-post-br (partial key-place 2 lastrow) 0.5 -1 web-post-bl)
))

(def thumb-key-top-middle-walls
  (union
    (wall-brace thumb-tm-place 0.5 -1 web-post-bl thumb-tl-place 0 -1 web-post-br)
    (wall-brace thumb-tm-place 0.5 -1 web-post-bl thumb-tm-place 0 -1 web-post-br)
    (wall-brace thumb-tm-place 0 -1 web-post-br thumb-tr-place 0 -1 web-post-bl)
))

(def case-walls
  (union
    right-wall
    left-wall
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
    (key-wall-brace 2 lastrow -0.5 -1 web-post-br 2 lastrow 0.5 -1 web-post-bl)
    (key-wall-brace 2 lastrow -0.5 -1 web-post-br 3 lastrow 0 -1 web-post-bl)
    (key-wall-brace 3 lastrow 0 -1 web-post-bl 3 lastrow 0.5 -1 web-post-br)
    (key-wall-brace 3 lastrow 0.5 -1 web-post-br 4 cornerrow 0.5 -1 web-post-bl)
    (key-wall-brace 4 cornerrow 0.5 -1 web-post-bl 4 cornerrow 0 -1 web-post-br)
    (for [x (range 5 ncols)] (key-wall-brace x cornerrow 0 -1 web-post-bl x       cornerrow 0 -1 web-post-br))
    (for [x (range 5 ncols)] (key-wall-brace x cornerrow 0 -1 web-post-bl (dec x) cornerrow 0 -1 web-post-br))
    ; thumb walls
    thumb-key-top-left-walls
    thumb-key-top-middle-walls
    thumb-key-top-right-walls
  )
)

(defn screw-insert-shape [bottom-radius top-radius height]
  (union
    (->>
      (binding [*fn* 30]
        (with-fn 90
          (cylinder [bottom-radius top-radius] height)
        )
      )
   )
   (translate [0 0 (/ height 2)] (->> (binding [*fn* 30] (sphere top-radius))))))

(defn screw-insert-place [column row offset]
  (let [shift-right   (= column lastcol)
        shift-left    (= column 0)
        shift-up      (and (not (or shift-right shift-left)) (= row 0))
        shift-down    (and (not (or shift-right shift-left)) (>= row lastrow))
        position      (if shift-up     (key-position column row (map + (wall-locate2  0  1) [0 (/ mount-height 2) 0]))
                          (if shift-down  (key-position column row (map - (wall-locate2  0 -1) [0 (/ mount-height 2) 0]))
                              (if shift-left (map + (left-key-position row 0) (wall-locate3 -1 0))
                                  (key-position column row (map + (wall-locate2  1  0) [(/ mount-width 2) 0 0])))))]
   (map + offset [(first position) (second position) 0]))
)

(defn screw-insert [bottom-radius top-radius height offset]
    (->> (screw-insert-shape bottom-radius top-radius height)
         (translate [0 0 (/ height 2)])
         (translate offset)))

(def screw-1-pos (screw-insert-place 1 0                  [13 -1 0]))
(def screw-2-pos (screw-insert-place 0 lastrow            [9 11 0]))
(def screw-3-pos (screw-insert-place lastcol lastrow      [-3.5 13 0]))
(def screw-4-pos (screw-insert-place lastcol 0            [-3.5 6 0]))
(def screw-5-pos (screw-insert-place 2 lastrow            [-9 2 0]))

(defn screw-insert-all-shapes [bottom-radius top-radius height]
  (union 
    (screw-insert bottom-radius top-radius height       screw-1-pos)
    (screw-insert bottom-radius top-radius (+ 1 height) screw-2-pos)
    (screw-insert bottom-radius top-radius height       screw-3-pos)
    (screw-insert bottom-radius top-radius height       screw-4-pos)
    (screw-insert bottom-radius top-radius (- height 1) screw-5-pos)))

(def screw-insert-height 4)
(def screw-insert-bottom-radius (/ 4.1 2))
(def screw-insert-top-radius (/ 4.1 2))
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

; Gateron Low Profile Switch
(def switch
  (union
    ; Stem
    (->> (union
      (cylinder (/ (+ 5.75 2) 2) 3)
      (cube 10 5.75 3)
      )
      (translate [0 0 (+ 3.35 1.5)])
    )
    ; Top
    (translate [0 0 (/ 3.35 2)] (cube (- keyswitch-width 1) (- keyswitch-height 1) 3.35))
    ; Bottom
    (translate [0 0 (/ 2.50 -2)] (cube (- keyswitch-width 2) (- keyswitch-height 2) 2.50))
    ; Pins
    (translate [0 0 (- (/ 2.80 -2) 2.50)] (with-fn 48 (cylinder (/ 5.25 2) 2.80)))
    (translate [-4.75 -2.60 (- (/ 2.60 -2) 2.50)] (with-fn 48 (cylinder 1 2.60)))
  )
)

(def switch-fills
  (apply union
         (for [column columns
               row rows
               :when (or (.contains [2 3] column)
                         (not= row lastrow))]
           (->> switch
                (key-place column row)))))



(load "usb-c")
(load "battery-switch")

(defn place-nice!nano [shape]
  ; Origin should be at the start of the board at the center of usb port
  (->> shape
    (translate [0 -1 nano-y-offset])
    (translate [(- (* (- centercol) column-x-delta)) 0 0])                  ; Place it at first column
    (translate [(- 9 (/ keyswitch-width 2)) 0 0]) ; Left nano border to the left border of column
    (translate [0 (* (+ centerrow 1) keyswitch-height) 0])
    (translate [0 (+ extra-height wall-thickness wall-xy-offset 6) 0])
    (translate [4 1.6 0])
  )
)

(def slide-clearance 0.1)

(def holder
  (place-nice!nano
    (difference
      (nice!nano-holder 0 0)
      usb-c-connector
      usb-c-cutout
    )
  )
)

(def holder-thicker-walls
  (place-nice!nano (union
    (translate [14 -0.5 0] (front-wall 0 slide-clearance))
    (translate [-4.2 -0.5 0] (front-wall 0 slide-clearance))
  ))
)

(def holder-hole
  (place-nice!nano (union
    (nice!nano-holder slide-clearance slide-clearance)
    (translate [0 -0.5 0] (front-wall slide-clearance slide-clearance))
  ))
)

(def all-walls-and-screws
  (union
    (difference
      (union
        case-walls
        screw-insert-outers
        holder-thicker-walls
      )
      holder-hole
      screw-insert-holes
      power-switch
      reset-switch
    )
    ; Uncomment to print holder with the case.
    ; Update slide-clearance accordingly.
    ; holder
  )
)

(def model-right
  (difference
    (union
      key-holes
      connectors
      thumb
      thumb-connectors
      all-walls-and-screws
    )
    (translate [0 0 -20] (cube 350 350 40))))


(spit "things/left.scad"
      (write-scad (mirror [-1 0 0] model-right)))

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
  )
)


(def test-objects
  (difference
    (union
      switch-fills
      (thumb-tl-place switch)
      (thumb-tm-place switch)
      (thumb-tr-place switch)
      (place-battery battery)
      (place-battery battery-holder)
      caps
      thumbcaps
      power-switch
      reset-switch
      holder
    )
    (translate [0 0 -20] (cube 350 350 40))
  )
)

(def plate-projection
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

(def plate-extruded
  (difference
    (union
      (translate [0 0 (/ plate-thickness 2)]
        (extrude-linear
          {:height plate-thickness :twist 0 :convexity 0 }
          plate-projection
        )
      )
      (translate [0 0 plate-thickness] (place-battery battery-holder))
    )
    (screw-head-hole screw-1-pos)
    (screw-head-hole screw-2-pos)
    (screw-head-hole screw-3-pos)
    (screw-head-hole screw-4-pos)
    (screw-head-hole screw-5-pos)
  )
)

(spit "things/right.scad"
  (write-scad model-right)
)

(spit "things/right-test.scad"
  (write-scad
    (union
      (translate [0 0 (+ plate-thickness 1)]
        (union model-right test-objects)
      )
      ; plate-extruded
    )
  )
)

(spit "things/nano-holder.scad"
  (write-scad
    holder
  )
)

(spit "things/right-plate-cut.scad"
  (write-scad
    plate-projection
  )
)

(spit "things/right-plate.scad"
  (write-scad
    plate-extruded
  )
)

(spit "things/left-plate.scad"
  (write-scad
    (mirror [-1 0 0] plate-extruded)
  )
)

(defn -main [dum] 1)  ; dummy to make it easier to batch
