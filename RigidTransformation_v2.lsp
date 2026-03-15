;;; Rigid Transformation Tool with Improved Residuals Display
;;; This AutoLISP routine performs rigid transformation (rotation + translation)
;;; between two coordinate systems and reports detailed residuals.

(defun c:RigidTransform (/ source-points target-points rotation-angle
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
  (compute-parameters source-points target-points)
  
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
  (apply-transformation ss rotation-angle translation-x translation-y)
  
  (princ)
)

;; Compute transformation parameters
(defun compute-parameters (src-pts tgt-pts / src-centroid tgt-centroid
                            i src-pt tgt-pt src-dx src-dy tgt-dx tgt-dy
                            numerator denominator cos-theta sin-theta
                            transformed-pts residuals total-residual rmse)
  ;; Calculate centroids
  (setq src-centroid (centroid src-pts))
  (setq tgt-centroid (centroid tgt-pts))
  
  ;; Calculate rotation parameters
  (setq numerator 0.0)
  (setq denominator 0.0)
  
  (setq i 0)
  (repeat (length src-pts)
    (setq src-pt (nth i src-pts))
    (setq tgt-pt (nth i tgt-pts))
    
    (setq src-dx (- (car src-pt) (car src-centroid)))
    (setq src-dy (- (cadr src-pt) (cadr src-centroid)))
    (setq tgt-dx (- (car tgt-pt) (car tgt-centroid)))
    (setq tgt-dy (- (cadr tgt-pt) (cadr tgt-centroid)))
    
    (setq numerator (+ numerator (- (* src-dx tgt-dy) (* src-dy tgt-dx))))
    (setq denominator (+ denominator (+ (* src-dx tgt-dx) (* src-dy tgt-dy))))
    
    (setq i (1+ i))
  )
  
  ;; Calculate transformation parameters
  (setq rotation-angle (atan numerator denominator))
  (setq cos-theta (cos rotation-angle))
  (setq sin-theta (sin rotation-angle))
  
  (setq translation-x (- (car tgt-centroid)
                       (- (* (car src-centroid) cos-theta)
                          (* (cadr src-centroid) sin-theta))))
  
  (setq translation-y (- (cadr tgt-centroid)
                       (+ (* (car src-centroid) sin-theta)
                          (* (cadr src-centroid) cos-theta))))
  
  ;; Display parameters
  (princ "\n\nRigid Transformation parameters computed:")
  (princ (strcat "\nRotation: " (rtos (* rotation-angle (/ 180.0 pi)) 2 7) " degrees"))
  (princ (strcat "\nTranslation X: " (rtos translation-x 2 4)))
  (princ (strcat "\nTranslation Y: " (rtos translation-y 2 4)))
  
  ;; Calculate residuals for each point
  (setq transformed-pts (mapcar '(lambda (pt) 
                                  (transform-point pt rotation-angle translation-x translation-y))
                               src-pts))
  
  (setq residuals nil)
  (setq total-residual 0.0)
  
  ;; Display detailed residuals with improved formatting
  (princ "\n\nPoint Residuals:")
  (princ "\n---------------------------------------------")
  (princ "\nPoint #     ΔX              ΔY              Distance")
  (princ "\n---------------------------------------------")
  
  (setq i 0)
  (repeat (length transformed-pts)
    (setq tgt-pt (nth i tgt-pts))
    (setq trans-pt (nth i transformed-pts))
    
    (setq dx (- (car tgt-pt) (car trans-pt)))
    (setq dy (- (cadr tgt-pt) (cadr trans-pt)))
    (setq dist (sqrt (+ (* dx dx) (* dy dy))))
    
    (setq residuals (append residuals (list dist)))
    (setq total-residual (+ total-residual dist))
    
    ;; Format each line with clear columns
    (princ (strcat "\n" (itoa (1+ i)) 
                  (if (< i 9) "           " "          ")  ;; Extra spacing for alignment
                  (rtos dx 2 6)
                  (if (>= dx 0.0) "       " "      ")  ;; Adjust for negative values
                  (rtos dy 2 6) 
                  (if (>= dy 0.0) "       " "      ")  ;; Adjust for negative values
                  (rtos dist 2 6)))
    
    (setq i (1+ i))
  )
  
  ;; Calculate and display statistics
  (setq rmse (sqrt (/ (apply '+ (mapcar '(lambda (r) (* r r)) residuals))
                     (length residuals))))
  
  (princ "\n---------------------------------------------")
  (princ (strcat "\nTotal Residual: " (rtos total-residual 2 6)))
  (princ (strcat "\nAverage Residual: " (rtos (/ total-residual (length residuals)) 2 6)))
  (princ (strcat "\nRMS Error: " (rtos rmse 2 6)))
  (princ (strcat "\nMax Residual: " (rtos (apply 'max residuals) 2 6)))
  (princ (strcat "\nMin Residual: " (rtos (apply 'min residuals) 2 6)))
)

;; Transform a single point using rigid transformation parameters
(defun transform-point (pt angle tx ty / x y cos-theta sin-theta new-x new-y)
  (setq x (car pt))
  (setq y (cadr pt))
  (setq cos-theta (cos angle))
  (setq sin-theta (sin angle))
  
  (setq new-x (+ (* x cos-theta) (* y (- sin-theta)) tx))
  (setq new-y (+ (* x sin-theta) (* y cos-theta) ty))
  
  (list new-x new-y)
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

;; Apply transformation to objects
(defun apply-transformation (ss angle tx ty / num-objects base-pt angle-deg)
  (setq num-objects (sslength ss))
  (setq angle-deg (* angle (/ 180.0 pi)))
  
  ;; Start undo group
  (command "_.undo" "_begin")
  
  ;; Use a temporary base point of 0,0 for operations
  (setq base-pt '(0.0 0.0 0.0))
  
  ;; First, perform rotation around origin
  (command "_rotate" ss "" base-pt angle-deg)
  
  ;; Then, perform translation
  (command "_move" ss "" base-pt (list tx ty 0.0))
  
  ;; End undo group
  (command "_.undo" "_end")
  
  (princ (strcat "\n\nTransformation applied to " (itoa num-objects) " objects:"))
  (princ (strcat "\n- Rotated by " (rtos angle-deg 2 7) " degrees"))  ;; Increased precision
  (princ (strcat "\n- Translated by X: " (rtos tx 2 4)))
  (princ (strcat "\n- Translated by Y: " (rtos ty 2 4)))
  (princ "\nCompleted successfully!")
)