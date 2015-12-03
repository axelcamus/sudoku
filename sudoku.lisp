(defvar *tab* (make-array '(9 9)))
(defvar *playerTab* (make-array '(9 9)))
(defvar *alpha* '(A B C D E F G H I))

;;VERSION RECURSIVE

;;(defun drawXY (x y tab)
;;  (if (< y 9)
;;      (let ()
;;	(if (= 0 x)
;;	    (format t "~D " (1+ y)))
;;	(if (< x 9)
;;	    (let ()
;;	      (if (= (mod x 3) 0)
;;		  (format t " ~D " (aref tab x y))
;;		  (format t "~D " (aref tab x y)))
;;	      (drawXY (+ x 1) y tab))
;;	    (let ()
;;	      (if (= (mod (+ 1 y) 3) 0)
;;		  (format t "~%~%")
;;		  (format t "~%"))
;;	      (drawXY 0 (+ y 1) tab))))))
;;
;;(defun draw (tab)
;;  (let ()
;;    (format t "  ")
;;    (draw-alpha-line *alpha* 0)
;;    (format t "~%~%")
;;    (drawXY 0 0 tab)))

(defun draw-alpha-line(l i)
  (if(< i 9)
     (let ()
       (if (= (mod i 3) 0)
	   (format t " ~A " (car l))
	   (format t "~A " (car l)))
       (draw-alpha-line (cdr l) (1+ i)))
     (format t "~%~%")))

(defun draw (tab)
  (format t "  ")
  (draw-alpha-line *alpha* 0)
  (dotimes (y 9)
    (let ()
      (format t "~D " (1+ y)))
    (dotimes (x 9)
      (if (= (mod x 3) 0)
	  (format t " ~D " (aref tab x y))
	  (format t "~D " (aref tab x y))))
    (if (= (mod (+ 1 y) 3) 0)
	(format t "~%~%")
	(format t "~%"))))))

(defun generate3x3 (x y)
  (let ((l '(1 2 3 4 5 6 7 8 9))
	(dotimes (y 3)
	  (dotimes (x 3)
	    (generatePoint x y l))))))

(defun generatePoint (x y l)
  (dotimes (y 9)
    (if (> 0 (aref *tab* 0 y))
	(remove (aref *tab* 0 y) l)))
  (dotimes (x 9)
    (if (> 0 (aref *tab* x 0))
	(remove (aref *tab* x 0) l)))
  (setf (aref *tab* x y) (nth (random (list-length l)) l)))
	  

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
      
  
