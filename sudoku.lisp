(defparameter *tab* (make-array '(9 9)))
(defparameter *playerTab* (make-array '(9 9)))

;;VERSION RECURSIVE

;(defun drawXY (x y tab)
;  (if (< y 9)
;      (if (< x 9)
;	  (let ()
;	    (if (= (mod x 3) 0)
;		(format t " ~D " (aref tab x y))
;		(format t "~D " (aref tab x y)))
;	    (drawXY (+ x 1) y tab))
;	  (let ()
;	    (if (= (mod (+ 1 y) 3) 0)
;		(format t "~%~%")
;		(format t "~%"))
;	    (drawXY 0 (+ y 1) tab)))))

;(defun draw (tab)
;  (drawXY 0 0 tab))

(defun draw (tab)
  (dotimes (y 9)
    (dotimes (x 9)
      (if (= (mod x 3) 0)
	  (format t " ~D " (aref tab x y))
	  (format t "~D " (aref tab x y))))
    (if (= (mod (+ 1 y) 3) 0)
	(format t "~%~%")
        (format t "~%")))))
      
(defun generate ()
  (dotimes (y 9)
    (dotimes (x 9)
      (setf (aref *tab* x y) (+ 1 (random 9))))))

;;avoir une liste (1 à 9) correspondant à un des tableaux 3x3
;;parcourir en x (le tableau 9x9)
;;parcourir en y (le tableau 9x9)
;;en déduire une nouvelle liste qui exclu les valeurs impossibles
;;faire un random sur cette nouvelle liste
;;et ajouter la valeur correspondante
;;supprimer la valeur ajouter de la première liste
;;recommencer pour tout les tableaux 3x3

(defun erase (tab)
  (if (equal tab *tab*)
      (setf *tab* (make-array '(9 9)))
      (if (equal tab *playerTab*)
	  (setf *playerTab* (make-array '(9 9))))))
      
  
