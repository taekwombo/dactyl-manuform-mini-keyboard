(ns dactyl-keyboard.dactyl
  (:refer-clojure :exclude [use import])
  (:require [clojure.core.matrix :refer [array matrix mmul]]
            [scad-clj.scad :refer :all]
            [scad-clj.model :refer :all]
            [unicode-math.core :refer :all]))

(def usb-c-width 9)
(def usb-c-height 3.26)
(def usb-c-connector
  (->>
    (cube usb-c-width 6 usb-c-height)
    ; Max Y = 0; Min Z = 0
    (translate [0 -3 (/ usb-c-height 2)])
    (color [1 0 0])
  )
)

;     ┌───+───┐   ─↓ 1mm   
; ┌───┴───o───┴─b─┐
; │ L           L │
; │               │
; │               │
; │               len
; │               │
; │               │
; │               │
; │               │
; └───────w───────┘
; o - origin 
; len - 33.5mm
; w - 18mm
; +X - 9mm
; +Y extends - 1mm
; b - space between board side border and usb port border - 2mm (to the leg of the port); 4mm (to the port) (4.5 to be exact)
; Holder for the sides of the port 2x2 - start of the board to the first pin holder = 2.5mm
; Pin:
;   - plastic 2.5mm
;   - leg     1.5mm
; Space between plate and bottom of the nano board: 4mm (pin plastic + leg)

(def nano-width 18)
(def nano-length 33.5)
(def nano-height 1.6)
(def pin-height 2.5)
(def pin-width 2.5)
(def pin-length (- nano-length 2))
(def pin-leg-h 1.8)
(def pin-leg-w 9)
(def nano-y-offset (+ pin-height pin-leg-h))

(def nice!nano
  (let [
    board (->>
      (union
        (cube nano-width nano-length nano-height)
        (translate [0 2 1] (cube (/ nano-width 2) (- nano-length 4) 1))
      )
      ; Max Y = 0; Min Z = 0
      (translate [0 (/ nano-length -2) (/ nano-height 2)])
      (color [0.7 0.7 0.7])
    )
    pin-plastic (->>
      (cube pin-width pin-length pin-height)
      ; Max Y = 0; Max Z = 0
      (translate [0 (/ pin-length -2) (/ pin-height -2)])
      ; Align to the end of the board
      (translate [0 (- pin-length nano-length) 0])
      (color [0.4 0.4 0.8])
    )
    pin-legs (->>
      (cube pin-leg-w pin-length pin-leg-h)
      ; Max Y = 0; Max Z = - pin-height
      (translate [
        0
        (/ pin-length -2)
        (- (+ pin-height (/ pin-leg-h 2)))
      ])
      ; Align to the end of the board
      (translate [0 (- pin-length nano-length) 0])
      (color [0.4 0.4 0.8])
    )
    usb (->>
      usb-c-connector
      (translate [0 1 0])
    )
    ]
    (->>
      (union
        board
        usb
        ; Left pins
        (translate [
          (+ (/ pin-width 2) (/ nano-width -2))
          0
          0
        ] pin-plastic)
        ; Left pin legs
        (translate [
          (+ (/ pin-leg-w -2) (/ nano-width -2) pin-width)
          0
          0
        ] pin-legs)
        ; Right pins
        (translate [
          (+ (/ pin-width -2) (/ nano-width 2))
          0
          0
        ] pin-plastic)
        ; Right pin legs
        (translate [
          (+ (/ pin-leg-w 2) (/ nano-width 2) (- pin-width))
          0
          0
        ] pin-legs)
      )
    )
  )
)

(def holder-thickness 3)
(def holder-space 0.2)
(def holder-wall 3)
(def front-wall-thickness 3)
(def front-wall-width
  (+ nano-width (* 2 holder-space) (* 2 holder-wall))
)
(def front-wall-height
  (* (+ 2 nano-y-offset) 2)
)

(def slide-post-xy (/ front-wall-thickness 3))

(defn slide-post [extra-width extra-height]
  (->>
    (cube (+ slide-post-xy extra-width) (+ slide-post-xy extra-width) (+ front-wall-height extra-height))
    ; Max Y = 0; Min Z = 0
    (translate [
      0
      (+ holder-space (/ (+ slide-post-xy extra-width) -2))
      (- (/ front-wall-height 2) nano-y-offset)
    ])
  )
)

(defn slide-post-holder-l [extra-width extra-height]
  (->>
    (slide-post extra-width extra-height)
    (translate [
      (/ (- (- front-wall-width) slide-post-xy) 2)
      (/ (+ slide-post-xy front-wall-thickness extra-width) 2)
      0
    ])
  )
)

(defn slide-post-holder-r [extra-width extra-height]
  (->>
    (slide-post extra-width extra-height)
    (translate [
      (/ (+ front-wall-width slide-post-xy) 2)
      (/ (+ slide-post-xy front-wall-thickness extra-width) 2)
      0
    ])
  )
)

(def usb-c-cutout
  (union
    (translate [0 3 0] usb-c-connector)
    (->>
      (cube 14 6 (+ usb-c-height 4))
      ; Max Y = 0; Min Z = 0
      (translate [0 -3 3])
      (translate [0 (+ 6 2) -1.5])
    )
  )
)

(defn front-wall [extra-width extra-height]
  (->>
    (cube (+ front-wall-width extra-width) front-wall-thickness (+ front-wall-height extra-height))
    ; Min Y = 0
    (translate [
      0
      (+ (/ front-wall-thickness 2) holder-space)
      (- (/ front-wall-height 2) nano-y-offset)
    ])
  )
)

(defn nice!nano-holder [extra-slider-width front-wall-extra-size]
  (union
    (->> ; Bottom
      (cube (- nano-width (* 2 pin-width) 2) (+ nano-length (* 4 holder-space)) holder-thickness)
      ; Max Y = 0; Max Z = 0
      (translate [
        0
        (/ nano-length -2)
        (/ holder-thickness -2)
      ])
    )
    (->> ; Back Leg
      (cube (- nano-width (* 2 pin-width) 2) holder-thickness nano-y-offset)
      ; Max Y = (- -nano-length - holder-thickness); Max Z = 0
      (translate [
        0
        (- (- nano-length) (/ holder-space 2) (/ holder-thickness 2))
        (/ nano-y-offset -2)
      ])
    )
    (->> ; Back Guard
      (cube (+ nano-width (* (+ holder-wall holder-space) 2)) holder-thickness nano-y-offset)
      ; Max Y = (- -nano-length - holder-thickness); Min Z = 0
      (translate [
        0
        (- (- nano-length) (/ holder-space 2) (/ holder-thickness 2))
        (- (/ nano-y-offset 2) pin-leg-h)
        ; (/ (+ nano-y-offset (- pin-leg-h)) 2) ; Slightly to higher
      ])
    )
    (let [
      wall (->>
        (cube holder-wall (+ nano-length (* 4 holder-space)) nano-y-offset)
        ; Max Y = 0;
        (translate [
          0
          (+ (/ nano-length -2) holder-space)
          (- (/ nano-y-offset 2) pin-leg-h)
          ; (/ (+ nano-y-offset (- pin-leg-h)) 2) ; Slightly higher version
        ])
      )
      ]
      (union
        (translate [
          (+ holder-space (/ holder-wall 2) (/ nano-width 2))
          0
          0
        ] wall)
        (translate [
          (- (/ holder-wall -2) (/ nano-width 2) holder-space)
          0
          0
        ] wall)
      )
    )
    ; Front Wall
    (front-wall front-wall-extra-size front-wall-extra-size)
    (slide-post-holder-l extra-slider-width front-wall-extra-size)
    (slide-post-holder-r extra-slider-width front-wall-extra-size)
  )
)
