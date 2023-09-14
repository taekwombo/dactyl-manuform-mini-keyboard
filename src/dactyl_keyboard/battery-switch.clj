(ns dactyl-keyboard.dactyl
  (:refer-clojure :exclude [use import])
  (:require [clojure.core.matrix :refer [array matrix mmul]]
            [scad-clj.scad :refer :all]
            [scad-clj.model :refer :all]
            [unicode-math.core :refer :all]))

; Battery
; Length: 40mm (+3mm connectors)
; Width: 30mm
(def battery-width 30)
(def battery-length 43)
(def battery-height 3.5)

(def battery
  (->>
    (cube battery-width battery-length battery-height)
    (translate [0 0 (/ battery-height 2)])
    (color [0.5 0.5 0.5])
  )
)

(def battery-holder
  (union
    ; Bottom
    (translate [(- (/ battery-width 2) 4) (- (/ battery-length -2) 2) (/ (+ battery-height 2) 2)]
      (union
        (cube 8 3 (+ battery-height 2))
        (translate [0 1 (+ (/ battery-height 2) 1)] (cube 8 5 2))
      )
    )
    (translate [(- (/ battery-width -2) 2) 14 (/ (+ battery-height 2) 2)]
      (cube 3 8 (+ battery-height 2))
      (translate [1 0 (+ (/ battery-height 2) 1)] (cube 5 8 2))
    )
  )
)

(defn place-battery [shape]
  (->> shape
    (translate [87 22 0])    
  )
)

; Power Switch
(def power-switch
  (->>
    (union
      (cube 3.5 9 3.5)
      (translate [0 0 (/ 3.5 2)] (cube 1.6 1.5 2))
    )
    (rotate (/ π 2) [0 1 0])
    (rotate (/ π 2) [0 0 1])
    (translate [80 61 (+ (/ 3.5 2) 5)])
  )
)

; Reset Switch
(def reset-switch
  (->>
    (union
      (cube 3 6 5)
      (translate [0 0 2.5] (cube 1.6 1.5 2))
    )
    (rotate (/ π 2) [0 -1 0])
    (translate [1.5 0 2.5])
    (translate [(- (* (- centercol) column-x-delta)) 0 0])                  ; Place it at first column
    (translate [0 (* (+ (- 3) centerrow) (+ mount-height extra-height)) 0]) ; Place it at 4th keycap side
    (translate [0 (- (/ keyswitch-height 2) 3) 0])
    (translate [(- (+ (/ mount-width 2) wall-xy-offset wall-thickness)) 0 0])
    (translate [0 0 2])
  )
)

; Screw head
(def screw-head-radius 3)
(def screw-radius 1.5)
(def screw-height 6)
(def screw-head-height 2)
(def screw-head-until 0.6)

(defn screw-head-hole [position]
  (->>
    (with-fn 90
      (union
        ; Base
        (translate [0 0 (/ (* screw-head-height screw-head-until) 2)]
          (cylinder
            [
              (+ screw-head-radius 0.5)
              (+ 
                (* screw-head-radius (- 1.0 screw-head-until))
                (* screw-radius screw-head-until)
              )
            ]
            (* screw-head-height screw-head-until)
          )
        )
        (translate [0 0 (+ (* screw-head-height screw-head-until 0.5) (/ screw-head-height 2))]
          (cylinder
            (+ 
              (* screw-head-radius (- 1.0 screw-head-until))
              (* screw-radius screw-head-until)
            )
            screw-head-height
          )
        )
      )
    )
    (translate position)
    (color [1 0 0])
  )
)
