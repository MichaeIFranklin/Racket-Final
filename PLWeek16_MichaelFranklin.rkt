#lang racket
(require racket/draw)  ; graphics library

;Geo Dim 2048 1152
;Alg Dim 1025 576

; ------------------ Constants -------------------------------------
(define imageWidth 2048)
(define imageHeight 1152)
(define target (make-bitmap imageWidth imageHeight)) ; A bitmap
(define dc (new bitmap-dc% [bitmap target])) ; a drawing context

; Default world center is screen center
(define WorldCenterX (exact->inexact (/ imageWidth 2)))
(define WorldCenterY (exact->inexact (/ imageHeight 2)))

; --------------- Helper Methods ----------------------------------
; builds a polygon object with x points
(define (BuildPoly Points)
  ; temp polygon holder
  (define newPoly (new dc-path%))
  ; switch sin for x and cos for y to rotate coords by 90 degrees, 0 - pointRadius to flip y
  (send newPoly move-to
        (exact->inexact (* (sin 0) (- 0 pointRadius)))
        (exact->inexact (* (cos 0) (- 0 pointRadius)))
        )
  ; for the rest of the points
  (for ([i Points])
    (send newPoly line-to
          (exact->inexact (* (sin (* (/ (* 2 pi) Points) i)) (- 0 pointRadius)))
          (exact->inexact (* (cos (* (/ (* 2 pi) Points) i)) (- 0 pointRadius)))
          )
    )
  (send newPoly close)
  ; return new poly
  newPoly
  )

; builds a star object with x points
(define (BuildStar Points CenterRadius)
  ; temp polygon holder
  (define newPoly (new dc-path%))
  ; switch sin for x and cos for y to rotate coords by 90 degrees, 0 - pointRadius to flip y
  (send newPoly move-to
        (exact->inexact (* (sin 0) (- 0 pointRadius)))
        (exact->inexact (* (cos 0) (- 0 pointRadius)))
        )
  ; for the rest of the points
  (for ([i (* Points 2)])
    ; shorten even points
    (if (= (modulo i 2) 1)
        ; shorten
        (send newPoly line-to
              (exact->inexact (* (sin (* (/ (* 2 pi) (* Points 2)) i)) (- 0 (* pointRadius CenterRadius))))
              (exact->inexact (* (cos (* (/ (* 2 pi) (* Points 2)) i)) (- 0 (* pointRadius CenterRadius))))
              )
        ; don't shorten
        (send newPoly line-to
              (exact->inexact (* (sin (* (/ (* 2 pi) (* Points 2)) i)) (- 0 pointRadius)))
              (exact->inexact (* (cos (* (/ (* 2 pi) (* Points 2)) i)) (- 0 pointRadius)))
              )
        )
    
    )
  (send newPoly close)
  ; return new poly
  newPoly
  )

; color input validation
(define (nice-make-color r g b)
  (make-color
   (if (< r 0)
       0
       (if (> r 255) 255 r))
   (if (< g 0)
       0
       (if (> g 255) 255 g))
   (if (< b 0)
       0
       (if (> b 255) 255 b))
   )
  )
(define (InvertColor Color)
  (make-color (- 255 (send Color red)) (- 255 (send Color green)) (- 255 (send Color blue)))
  )

; sets all pixels to passed color
(define (SetBackground r g b)
  (send dc set-pen (nice-make-color r g b) 1 'solid)
  (send dc set-brush (nice-make-color r g b) 'solid)
  (send dc draw-rectangle
        0 0   
        imageWidth imageHeight)
  )
;xs = (xw - xWorldMin) * (imageWidth / (xWorldMax – xWorldMin))

; converts from world space to screen space
(define (worldx-to-screenx x)
  (+ WorldCenterX x)
  )
(define (worldy-to-screeny y)
  (+ WorldCenterY y)
  )

; draws a given shape X and Y units away from WorldCenter with passed rotation, scale, and color
(define (drawShape shape PosX PosY Rot Scale Color)
  ; set outline to new color
  (send dc set-pen Color 1 'solid)
  ; rotate polygon at screen origin
  (send shape rotate Rot)
  ; scale then translate to world space
  (send shape scale Scale Scale)
  (send shape translate (worldx-to-screenx PosX) (worldy-to-screeny PosY))
  (send dc draw-path shape)
  ; translate then scale to get back to screen origin for next rotation
  (send shape translate (- 0 (+ PosX WorldCenterX)) (- 0 (+ PosY WorldCenterY)))
  (send shape scale (exact->inexact (/ 1 Scale))  (exact->inexact (/ 1 Scale)))
  ; rotate polygon back to normal
  (send shape rotate (- 0 Rot))
  )

; ---------------- Program Start ------------------------------------------------------

(SetBackground 0 0 128) ; deep blue background
(send dc set-brush (nice-make-color 0 0 0) 'transparent) ; transparent fill to start

; base poly attributes
(define pointRadius (exact->inexact (/ imageHeight 3))) ; distance to a point from center
(define BasePosX 0)
(define BasePosY 0)
(define BaseScale 0.6)
(define BaseRot 0)
(define BaseColor (nice-make-color 255 0 0)) ; line color
(define fractalStarCenterRadius 2.1) ; radius of the center of the star polygon, divide pointRadius by this 
(define fractalPointNum 6) ; number of points on the star polygon


; algorithms
; returns a color based on the current itteration compared to the total itterations
(define (GetColor i MaxI)
  (define ColorSegment (exact->inexact (* (/ i MaxI) 9))) ; segment of the rainbow
  (define SegmentPercent (exact->inexact (- ColorSegment (floor ColorSegment)))) ; percent completion through segment
  (cond
    ; red to orange
    [(< ColorSegment 1)
     (make-color 255 (exact-round (* 150 SegmentPercent)) 0)]
    ; orange to yellow
    [(< ColorSegment 2)
     (make-color 255 (+ 150 (exact-round (* 105 SegmentPercent))) 0)]
    ; yellow to green
    [(< ColorSegment 3)
     (make-color (- 255 (exact-round (* 255 SegmentPercent))) 255 0)]
    ; green to cyan
    [(< ColorSegment 4)
     (make-color 0 255 (exact-round (* 255 SegmentPercent)))]
    ; cyan to blue
    [(< ColorSegment 5)
     (make-color 0 (- 255 (exact-round (* 255 SegmentPercent))) 255)]
    ; blue to indigo
    [(< ColorSegment 6)
     (make-color (exact-round (* 150 SegmentPercent)) 0 255)]
    ; indigo to violet
    [(< ColorSegment 7)
     (make-color (+ 150 (exact-round (* 105 SegmentPercent))) 0 255)]
    ; violet to magenta
    [(< ColorSegment 8)
     (make-color 255 0 (- 255 (exact-round (* 105 SegmentPercent))))]
    ; magenta to red
    [(<= ColorSegment 9)
     (make-color 255 0 (- 150 (exact-round (* 150 SegmentPercent))))]
    ; base color (catch cause)
    [ BaseColor]
    )
  )

; (NOT FRACTAL - But looks cool)
; draw a shape at World Center and slowly turn shrink and move it GeosPerTunnel times
; rainbow color the tunnel also
(define GeosPerTunnel 500) ; how many polygons per tunnel?
(define tunnelStarPolygon (BuildStar fractalPointNum (/ 1 fractalStarCenterRadius)))
(define (DrawTunnel PosX PosY Rot Scale Color Count)
  (cond
    [(not (= Count 0))
     (drawShape tunnelStarPolygon PosX PosY Rot Scale Color)
     ; recurse to finish the tunnel, altering shape rotation position and scale, decrment count
     (DrawTunnel PosX
                 PosY
                 (+ Rot (exact->inexact (/ (* pi 2) GeosPerTunnel)))
                 (- Scale (exact->inexact (/ Scale GeosPerTunnel)))
                 (GetColor Count GeosPerTunnel)
                 (- Count 1))    
     ]
    )
  )

; ---------------- Geometric Fractal Time! -----------------------------------------------

; Fractal Vars
(define MaxFractals 500000) ; total amount of polys allowed
(define ColorLevelMax 20) ; amount of levels to do a full rainbow color rotation, default 9
(define RotateOnTips 1) ; 1 for yes 0 for no
(define Expansive 1) ; 1 to place new polys on edges of exsiting ones, -1 to place them within the edges of existing polys
(define ScaleFactor 2.5) ; how quickly polys shrink or grow: > 0 for shrink, < 0 for grow
(define BaseColorMod 0) ; starting color (higher values progress through the rainbow), valid values 0 - ColorLevelMax

(define fractalStarPolygon (BuildStar fractalPointNum (/ 1 fractalStarCenterRadius))) ; base star
(define basePolygon (BuildPoly fractalPointNum)) ; base polygon to go inside star
;(define TotalFractals 0) ; holds how many polys we have drawn - NOT USED

; check to see if the polygon will be on the screen
(define (CheckPolyVisible PosX PosY Scale ScaleMod)
  ; get the bounds of hte polygon
  (define Left (- (worldx-to-screenx PosX) (* pointRadius (* Scale ScaleMod))))
  (define Right (+ (worldx-to-screenx PosX) (* pointRadius (* Scale ScaleMod))))
  (define Up (- (worldy-to-screeny PosY) (* pointRadius (* Scale ScaleMod))))
  (define Down (+ (worldy-to-screeny PosY) (* pointRadius (* Scale ScaleMod))))
  ; check if shape off screen or so small it won't be visible
  (if (or (< Right 0) (< Down 0) (> Left imageWidth) (> Up imageHeight) (< (* pointRadius Scale ScaleMod) (* 1 .5)))
      ; off screen
      #f
      ; on screen
      #t
      )
  )


; draw polygon shape submethod
(define (drawFractalShape PosX PosY Rot Scale Color)
  ; draw the polygons that make up the shape
  (send dc set-brush (make-color 0 0 0) 'transparent) ; the outlines have no fill
  (drawShape fractalStarPolygon PosX PosY Rot (* Scale 1.5) Color)
  (drawShape basePolygon PosX PosY (+ Rot (exact->inexact (/ pi fractalPointNum))) (* (exact->inexact (/ Scale fractalStarCenterRadius)) 1.5) (InvertColor Color))
     
  (send dc set-brush Color 'solid) ; fill for the star shape
  (drawShape fractalStarPolygon PosX PosY Rot Scale Color)
  (send dc set-brush (InvertColor Color) 'solid) ; fill for the inner shape
  (drawShape basePolygon PosX PosY (+ Rot (exact->inexact (/ pi fractalPointNum))) (exact->inexact (/ Scale fractalStarCenterRadius)) (InvertColor Color))
  )


; draws a single star based rotational geometric fractal
(define (DrawFractal PosX PosY Rot Scale Count ColorModifer Log)
  ; get color for this level
  (define LevelColor (GetColor (modulo (+ Count ColorModifer) ColorLevelMax) ColorLevelMax))

  ; check if this shape will be on screen
  (cond [(CheckPolyVisible PosX PosY Scale 1.5)
         ; on screen
         ; draw the fractal shape
         (drawFractalShape PosX PosY Rot Scale LevelColor)
         ])
  ; check to see if the next level should be drawn
  (cond
    [;(< (exact->inexact (/ (- 1 (expt fractalPointNum (+ Count 1))) (- 1 fractalPointNum))) MaxFractals)
     (>= (* pointRadius (exact->inexact (/ Scale ScaleFactor))) (* (* 1 (expt 10 -12)) .5))         
     ; if this is the first iteration
     (if (= Count 0)
         ; draw all around the central star
         (for ([i fractalPointNum])
           ; check if this next poly will be visible
           ; allow the second chaild, i = 1, no matter what to allow zooming later on
           (cond  [(or (= i 1) (CheckPolyVisible (+ PosX (exact->inexact (* (sin (+ Rot (* (/ (* 2 pi) (* fractalPointNum 2)) (+ (* 2 (+ i 1)) (+ (modulo (+ fractalPointNum 1) 2) RotateOnTips))))) (+ (* pointRadius Scale) (* Expansive (* pointRadius (/ Scale ScaleFactor)))))))
                                                 (+ PosY (exact->inexact (* (cos (+ Rot (* (/ (* 2 pi) (* fractalPointNum 2)) (+ (* 2 (+ i 1)) (+ (modulo (+ fractalPointNum 1) 2) RotateOnTips))))) (+ (* pointRadius Scale) (* Expansive (* pointRadius (/ Scale ScaleFactor)))))))                                        
                                                 (exact->inexact (/ Scale ScaleFactor))
                                                 1.5))
                   ; log itteration and pass on flag if logging is turned on
                   (cond [(and (= i 0) (= Log 1)) (printf "iteration: ~v\n" Count)]
                         [(and (not (= i 0)) (= Log 1)) (set! Log 0)]) ; don't pass log flag other itterations           
                   (DrawFractal
                    (+ PosX (exact->inexact (* (sin (+ Rot (* (/ (* 2 pi) (* fractalPointNum 2)) (+ (* 2 (+ i 1)) (+ (modulo (+ fractalPointNum 1) 2) RotateOnTips))))) (+ (* pointRadius Scale) (* Expansive (* pointRadius (/ Scale ScaleFactor)))))))
                    (+ PosY (exact->inexact (* (cos (+ Rot (* (/ (* 2 pi) (* fractalPointNum 2)) (+ (* 2 (+ i 1)) (+ (modulo (+ fractalPointNum 1) 2) RotateOnTips))))) (+ (* pointRadius Scale) (* Expansive (* pointRadius (/ Scale ScaleFactor)))))))
                    (+ Rot (exact->inexact (* (/ (* 2 pi) (* fractalPointNum 2)) (+ (* 2 (- i 2)) (+ (modulo (+ fractalPointNum 1) 2) RotateOnTips)))))
                    (exact->inexact (/ Scale ScaleFactor))
                    (+ Count 1)
                    ColorModifer
                    Log)     
                   ])
           )
         ; draw a new poly for each outward facing star point
         (for ({i (floor (/ fractalPointNum 2))})
           ; check if this next poly will be visible
           ; allow the second chaild, i = 1, no matter what to allow zooming later on
           (cond  [(or (= i 1) (CheckPolyVisible (+ PosX (exact->inexact (* (sin (+ Rot (* (/ (* 2 pi) (* fractalPointNum 2)) (+ (* 2 (+ i 1)) (+ (modulo (+ fractalPointNum 1) 2) RotateOnTips))))) (+ (* pointRadius Scale) (* Expansive (* pointRadius (/ Scale ScaleFactor)))))))
                                                 (+ PosY (exact->inexact (* (cos (+ Rot (* (/ (* 2 pi) (* fractalPointNum 2)) (+ (* 2 (+ i 1)) (+ (modulo (+ fractalPointNum 1) 2) RotateOnTips))))) (+ (* pointRadius Scale) (* Expansive (* pointRadius (/ Scale ScaleFactor)))))))
                                                 (exact->inexact (/ Scale ScaleFactor))
                                                 1.5))
                   ; log itteration, if logging is turned on, and pass on flag 
                   (cond [(and (= i 0) (= Log 1)) (printf "iteration: ~v\n" Count)]
                         [(and (not (= i 0)) (= Log 1)) (set! Log 0)]) ; don't pass log flag other itterations
                   (DrawFractal
                    (+ PosX (exact->inexact (* (sin (+ Rot (* (/ (* 2 pi) (* fractalPointNum 2)) (+ (* 2 (+ i 1)) (+ (modulo (+ fractalPointNum 1) 2) RotateOnTips))))) (+ (* pointRadius Scale) (* Expansive (* pointRadius (/ Scale ScaleFactor)))))))
                    (+ PosY (exact->inexact (* (cos (+ Rot (* (/ (* 2 pi) (* fractalPointNum 2)) (+ (* 2 (+ i 1)) (+ (modulo (+ fractalPointNum 1) 2) RotateOnTips))))) (+ (* pointRadius Scale) (* Expansive (* pointRadius (/ Scale ScaleFactor)))))))
                    (+ Rot (exact->inexact (* (/ (* 2 pi) (* fractalPointNum 2)) (+ (* 2 (- i 2)) (+ (modulo (+ fractalPointNum 1) 2) RotateOnTips)))))
                    (exact->inexact (/ Scale ScaleFactor))
                    (+ Count 1)
                    ColorModifer
                    Log)
                   ])
           )
         )
     ]
    )
  )

; Geo Fractal pseudo "zooming"
(define (ZoomInFrames)
  (define ZoomScale BaseScale) ; holds the current zoom scale
  (define ZoomX BasePosX) ; holds the current zoom x position
  (define ZoomY BasePosY) ; holds the current zoom y position
  (define SlowedFramesMod 3) ; holds the frame multiplier when slowing down zoom
  (define FrameSpeedMod .2) ; how much the SlowedFrameMod is decresed per level
  (define FramesPerLevel 30) ; frames of interpolation between each level
  (define FrameIndex 0) ; holds current frame, needed for image labeling
  (for ([i 40]) ; 40 is the max level before floating point error ruins the zoom positioning
    (define Level (exact->inexact i)) ; level of zoom

    ; to properly interpolate between levels of zoom, we need the current level and next levels of zoom
    (define LevelScale (* BaseScale (expt ScaleFactor (floor Level))))
    (define NextLevelScale (* BaseScale (expt ScaleFactor (+ (floor Level) 1))))
    (define FramesForThisLevel (exact-round (* FramesPerLevel SlowedFramesMod)))

    ; interpolate to the next level
    (for ([b FramesForThisLevel])
      ; position and scale fractal for frame
      (set! ZoomScale (exact->inexact (+ LevelScale (* (+ (modulo (+ b (* i FramesForThisLevel)) FramesForThisLevel) 1) (/ (- NextLevelScale LevelScale) FramesForThisLevel)))))
      ;(printf "Frame: ~v\nTestScale: ~v\nLevelScale: ~v\nNext Level Scale: ~v\nScale Seg Percent: ~v\n" (+ b (* i FramesPerLevel) 1) TestScale LevelScale NextLevelScale (exact->inexact (/ (- NextLevelScale LevelScale) FramesPerLevel)))
      (set! ZoomX (- ZoomX (exact->inexact (/ (* (sin (+ BaseRot (* (/ (* 2 pi) (* fractalPointNum 2)) (+ (* 2 (+ 1 1)) (+ (modulo (+ fractalPointNum 1) 2) RotateOnTips))))) (+ (* pointRadius NextLevelScale) (* Expansive (* pointRadius (/ NextLevelScale ScaleFactor))))) FramesForThisLevel))))
      (set! ZoomY (- ZoomY (exact->inexact (/ (* (cos (+ BaseRot (* (/ (* 2 pi) (* fractalPointNum 2)) (+ (* 2 (+ 1 1)) (+ (modulo (+ fractalPointNum 1) 2) RotateOnTips))))) (+ (* pointRadius NextLevelScale) (* Expansive (* pointRadius (/ NextLevelScale ScaleFactor))))) FramesForThisLevel))))

      ; render frame to file
      (SetBackground 0 0 128) ; clear screen
      (DrawFractal ZoomX ZoomY BaseRot ZoomScale 0 BaseColorMod 0)
      (send target save-file (string-append "temp/ZoomTest" (format "~v.png" FrameIndex)) 'png) ; save image as png
      ; add one to frame index
      (set! FrameIndex (+ FrameIndex 1))
      )

    ; speed up the zoom for the next level
    (cond [(> SlowedFramesMod 1)
           ; decrease the Frames multiplier
           (set! SlowedFramesMod (- SlowedFramesMod FrameSpeedMod))
           ; set the multiplier to 1 if it is lower than 1
           (cond [(< SlowedFramesMod 1) (set! SlowedFramesMod 1)])
           ])
    )
  )


; -------------------- Algebraic Fractal Time -----------------------------------
(define CReal -.7139) ; Real Value for C, Change this to change around the generated image
(define CImaginary .226) ; Imaginary Value for C, Change this to change around the generated image
(define MaxIter 1000) ; Number of times to iterrate through the formula before testing the result
(define AlgScale .0025) ; Multiply the pixel coords by this before creating the imaginary number

(define (CalculateEscape real imaginary Iter)
  ; if we esceeded the max iterrations
  (if (> Iter MaxIter)
      ; we did not escape return MaxIter to show failure
      MaxIter
      ; keep testing for escape
      (if (>= (+ (* real real) (* imaginary imaginary)) 4.0)
          ; we have escaped, return the current iteration
          Iter
          ; we have not yet escaped, compute next values to be tested
          (let ([TempReal (- (* real real) (* imaginary imaginary))])
            (CalculateEscape (+ TempReal CReal)
                             (+ (* 2 real imaginary) CImaginary)
                             (+ Iter 1))
            )
          )
      )
  )


(define (ColorPixel X Y)
  (define EscapeIter (CalculateEscape (* X AlgScale) (* Y AlgScale) 0))
  ; color pixel based on whether this pixel is in the escape set     
  (if (= EscapeIter MaxIter)
      (make-color 0 0 0)
      (InvertColor (GetColor EscapeIter MaxIter))
      )
  )

(define (DrawAlgFractal)
  ; for each line of pixels
  (for ([col imageHeight])
    ; test each pixel
    (for ([row imageWidth])
      ; color pixel based on if it escapes or not
      (send dc set-pixel row col (ColorPixel (- row (round (/ imageWidth 2))) (- col (round (/ imageHeight 2)))))
      )
    )
  )

; -------------------- Code Snippets --------------------------------------------
;(+ PosX (exact->inexact (/ WorldCenterX GeosPerTunnel)))
;(+ PosY (exact->inexact (/ WorldCenterY GeosPerTunnel)))
;(- Rot (exact->inexact (/ (* 2 pi) 10)))
;(exact->inexact (/ Scale 3))
;(- PosY (exact->inexact (* (/ pointRadius 2) Scale)))

; legacy poly check
;(cond
;  have we not surpassed the poly limit?
;  [(< (exact->inexact (/ (- 1 (expt fractalPointNum Count)) (- 1 fractalPointNum))) MaxFractals)
;   (< TotalFractals MaxFractals)
;   ]
;  [ (printf "We are done: ~v\n" Count)]
;  )
; increment TotalFractals
;(set! TotalFractals (+ TotalFractals 1))


;; tiles fractals
;(define (MakeFractalArt PosX PosY Rot Scale)
;  ; tile horizonally
;  ; if we have passed the world edge
;  (if (or (> (worldx-to-screenx PosX) imageWidth) (< (worldx-to-screenx PosX) 0))
;      (printf "done")
;      ; we have room
;      (let ()
;        ; draw fractal
;        (DrawFractal PosX PosY Rot Scale 0 0)
;        ; recurse
;        (cond
;          ; tile left
;          [(< PosX 0)
;           (MakeFractalArt (- PosX (* 2 TileWidth))
;                           PosY
;                           Rot
;                           Scale)]
;          ; tile right
;          [(> PosX 0)
;           (MakeFractalArt (+ PosX (* 2 TileWidth))
;                           PosY
;                           Rot
;                           Scale)]
;          ; tile right and right
;          [(= PosX 0)
;           (MakeFractalArt (- PosX (* 2 TileWidth))
;                           PosY
;                           Rot
;                           Scale)
;           (MakeFractalArt (+ PosX (* 2 TileWidth))
;                           PosY
;                           Rot
;                           Scale)]
;          )
;        )
;      )
;  )
;(MakeFractalArt BasePosX BasePosY BaseRot BaseScale)

; color interpolation test
;(define (Loop i)
;  (if (= 0 i)
;      (printf "done\n")
;      (let ()
;        (let ([Color (GetColor (- ColorLevelMax (+ i 1)) ColorLevelMax)]
;              [Scale (exact->inexact (/ i ColorLevelMax))])
;          ; draw example colored shape
;          (drawShape tunnelStarPolygon BasePosX BasePosY 0 Scale Color)
;          ; render image (step testing)
;          ;(send target save-file (string-append "test" (format "00~v.png" i)) 'png) ; save image as png
;          ; redraw background to clear image
;          ;(SetBackground 0 0 128)
;          ; loopback
;          (Loop (- i 1))
;          )
;        )
;      )
;  )
;(Loop ColorLevelMax)

;(drawShape fractalStarPolygon BasePosX BasePosY 0 BaseScale BaseColor)
;(drawShape tunnelStarPolygon BasePosX BasePosY 0 BaseScale BaseColor)
;(drawShape basePolygon BasePosX BasePosY (exact->inexact (/ pi fractalPointNum)) (/ BaseScale fractalStarCenterRadius) (InvertColor BaseColor))

;(for ([i 200])
;  (DrawAlgFractal)
;  (send target save-file (string-append "AlgCValueTest" (format "~v-~v.png" (* CReal 100) (* CImaginary 100))) 'png) ; save image as png
;  (set! CReal (+ CReal .002))
;  (set! CImaginary (+ CImaginary .002))  
;  )

; color ripple
;(for ([i ColorLevelMax])
;  ; reset background
;  (SetBackground 0 0 128)
;  ; draw fractal
;  (DrawFractal BasePosX BasePosY BaseRot BaseScale 0 i 0)
;  ; save picture
;  (send target save-file (string-append "Test" (format "~v.png" i)) 'png) ; save image as png
;  )

; -------------- Main Calls --------------------------------------------------------
;(DrawTunnel BasePosX BasePosY BaseRot BaseScale BaseColor GeosPerTunnel)
(DrawFractal BasePosX BasePosY BaseRot BaseScale 0 BaseColorMod 0)
;(ZoomInFrames)

;(DrawAlgFractal)

;(send target save-file (string-append "Test" (format "00~v.png" 1)) 'png) ; save image as png
target ; print image