;;; 2D Conformal Transformation Tool
;;; This AutoLISP routine performs a 2D conformal transformation (scale, rotation, and translation)
;;; between two coordinate systems using corresponding point pairs.

(defun c:ConformalTransform (/ source-points target-points scale rotation-angle
                             translation-x translation-y ss)
  
  ;; Initialize variables
  (setq source-points nil)
  (setq target-points nil)
  
  ;; Step 1: Select source points
  (princ "\nSelect points in system 1 (press Enter when done):")
  (while (setq pt (getpoint "\nSelect point: "))
    (setq source-points (append source-points (list (list (car pt) (cadr pt)))))
    (princ (strcat "\nPoint added: (" (rtos (car pt) 2 4) ", " (rtos (cadr pt) 2 4) ")"))
  )
  
  ;; Check if we have enough points
  (if (< (length source-points) 2)
      (progn
        (princ "\nError: Need at least 2 points in system 1. Operation canceled.")
        (exit)
      )
  )
  
  (princ (strcat "\n" (itoa (length source-points)) " points selected in system 1."))
  
  ;; Step 2: Select target points
  (princ "\n\nSelect points in system 2 (press Enter when done):")
  (while (setq pt (getpoint "\nSelect point: "))
    (setq target-points (append target-points (list (list (car pt) (cadr pt)))))
    (princ (strcat "\nPoint added: (" (rtos (car pt) 2 4) ", " (rtos (cadr pt) 2 4) ")"))
  )
  
  ;; Check if we have enough points and same number
  (cond
    ((< (length target-points) 2)
     (princ "\nError: Need at least 2 points in system 2. Operation canceled.")
     (exit))
    ((/= (length source-points) (length target-points))
     (princ "\nError: Number of points in system 1 and system 2 must match. Operation canceled.")
     (princ (strcat "\nSystem 1: " (itoa (length source-points)) " points"))
     (princ (strcat "\nSystem 2: " (itoa (length target-points)) " points"))
     (exit))
  )
  
  (princ (strcat "\n" (itoa (length target-points)) " points selected in system 2."))
  
  ;; Step 3: Compute transformation parameters
  (compute-conformal-parameters source-points target-points)
  
  ;; Step 4: Select objects to transform
  (princ "\n\nSelect objects to transform (press Enter when done):")
  (setq ss (ssget))
  
  ;; Check if we have selected objects
  (if (not ss)
      (progn
        (princ "\nNo objects selected. Operation canceled.")
        (exit)
      )
  )
  
  ;; Step 5: Apply transformation
  (apply-conformal-transformation ss scale rotation-angle translation-x translation-y)
  
  (princ)
)

;; Compute conformal transformation parameters
(defun compute-conformal-parameters (src-pts tgt-pts / src-centroid tgt-centroid
                                     src-centered tgt-centered i src-pt tgt-pt
                                     src-dx src-dy tgt-dx tgt-dy numerator-r
                                     denominator-r numerator-s denominator-s 
                                     a b cos-theta sin-theta)
  ;; Calculate centroids
  (setq src-centroid (centroid src-pts))
  (setq tgt-centroid (centroid tgt-pts))
  
  ;; Center the points around their centroids
  (setq src-centered (mapcar '(lambda (pt) 
                                (list (- (car pt) (car src-centroid))
                                      (- (cadr pt) (cadr src-centroid))))
                             src-pts))
  
  (setq tgt-centered (mapcar '(lambda (pt)
                                (list (- (car pt) (car tgt-centroid))
                                      (- (cadr pt) (cadr tgt-centroid))))
                             tgt-pts))
  
  ;; Calculate parameters for transformation
  (setq numerator-r 0.0)   ;; For rotation
  (setq denominator-r 0.0)
  (setq numerator-s 0.0)   ;; For scale
  (setq denominator-s 0.0)
  
  (setq i 0)
  (repeat (length src-centered)
    (setq src-pt (nth i src-centered))
    (setq tgt-pt (nth i tgt-centered))
    
    (setq src-dx (car src-pt))
    (setq src-dy (cadr src-pt))
    (setq tgt-dx (car tgt-pt))
    (setq tgt-dy (cadr tgt-pt))
    
    ;; For rotation calculation
    (setq numerator-r (+ numerator-r (- (* src-dx tgt-dy) (* src-dy tgt-dx))))
    (setq denominator-r (+ denominator-r (+ (* src-dx tgt-dx) (* src-dy tgt-dy))))
    
    ;; For scale calculation
    (setq numerator-s (+ numerator-s (+ (* tgt-dx tgt-dx) (* tgt-dy tgt-dy))))
    (setq denominator-s (+ denominator-s (+ (* src-dx src-dx) (* src-dy src-dy))))
    
    (setq i (1+ i))
  )
  
  ;; Calculate scale factor
  (if (= denominator-s 0.0)
      (setq scale 1.0)
    (setq scale (sqrt (/ numerator-s denominator-s)))
  )
  
  ;; Calculate rotation parameters (a and b)
  (setq a denominator-r)
  (setq b numerator-r)
  
  ;; Normalize a and b by the scale factor
  (setq a (/ a (length src-pts)))
  (setq b (/ b (length src-pts)))
  
  ;; Calculate rotation angle
  (setq rotation-angle (atan b a))
  (setq cos-theta (cos rotation-angle))
  (setq sin-theta (sin rotation-angle))
  
  ;; Calculate translation
  (setq translation-x (- (car tgt-centroid)
                       (* scale (- (* (car src-centroid) cos-theta)
                                  (* (cadr src-centroid) sin-theta)))))
  
  (setq translation-y (- (cadr tgt-centroid)
                       (* scale (+ (* (car src-centroid) sin-theta)
                                  (* (cadr src-centroid) cos-theta)))))
  
  ;; Display parameters
  (princ "\n\nConformal transformation parameters computed:")
  (princ (strcat "\nScale factor: " (rtos scale 2 4)))
  (princ (strcat "\nRotation: " (rtos (* rotation-angle (/ 180.0 pi)) 2 4) " degrees"))
  (princ (strcat "\nTranslation X: " (rtos translation-x 2 4)))
  (princ (strcat "\nTranslation Y: " (rtos translation-y 2 4)))
)

;; Calculate centroid of points
(defun centroid (points / sum-x sum-y count pt)
  (setq sum-x 0.0)
  (setq sum-y 0.0)
  (setq count (length points))
  
  (foreach pt points
    (setq sum-x (+ sum-x (car pt)))
    (setq sum-y (+ sum-y (cadr pt)))
  )
  
  (list (/ sum-x count) (/ sum-y count))
)

;; Apply conformal transformation to objects
(defun apply-conformal-transformation (ss scale angle tx ty / num-objects base-pt angle-deg)
  (setq num-objects (sslength ss))
  (setq angle-deg (* angle (/ 180.0 pi)))
  
  ;; Start undo group
  (command "_.undo" "_begin")
  
  ;; Use a temporary base point of 0,0 for operations
  (setq base-pt '(0.0 0.0 0.0))
  
  ;; Apply scale
  (command "_scale" ss "" base-pt scale)
  
  ;; Apply rotation
  (command "_rotate" ss "" base-pt angle-deg)
  
  ;; Apply translation
  (command "_move" ss "" base-pt (list tx ty 0.0))
  
  ;; End undo group
  (command "_.undo" "_end")
  
  (princ (strcat "\n\nTransformation applied to " (itoa num-objects) " objects:"))
  (princ (strcat "\n- Scaled by factor: " (rtos scale 2 4)))
  (princ (strcat "\n- Rotated by " (rtos angle-deg 2 4) " degrees"))
  (princ (strcat "\n- Translated by X: " (rtos tx 2 4) ", Y: " (rtos ty 2 4)))
  (princ "\nCompleted successfully!")
)