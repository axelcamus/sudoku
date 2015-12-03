(defparameter *smallTabSize* 3)
(defparameter *tabSize* (* *smallTabSize* *smallTabSize*))
(defparameter *tab* (make-array (make-list 2 :initial-element *tabSize*)))
(defparameter *playerTab* (make-array (make-list 2 :initial-element *tabSize*)))
(defparameter *alpha* '(A B C D E F G H I J K L M N O P K R S T U V W X Y Z))

;;############# DRAW ####################################

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
  (if(< i *tabSize*)
     (let ()
       (if (= (mod i *smallTabSize*) 0)
	   (format t " ~A " (car l))
	   (format t "~A " (car l)))
       (draw-alpha-line (cdr l) (1+ i)))
     (format t "~%~%")))

(defun draw (tab)
  (format t "  ")
  (draw-alpha-line *alpha* 0)
  (dotimes (y *tabSize*)
    (let ()
      (format t "~D " (1+ y)))
    (dotimes (x *tabSize*)
      (if (= (mod x *smallTabSize*) 0)
	  (format t " ~D " (aref tab x y))
	  (format t "~D " (aref tab x y))))
    (if (= (mod (+ 1 y) *smallTabSize*) 0)
	(format t "~%~%")
	(format t "~%"))))))

;;################# GENERATE ###############################

(defun generate ()
  (dotimes (y *smallTabSize*)
    (dotimes (x *smallTabSize*)
      (generateSmallTab (* x *smallTabSize*) (* y *smallTabSize*))))
  (generatePlayerTab))

(defun generateSmallTab (i j)
  (let ((l (makeTabList *tabSize* '())))
	(dotimes (y *smallTabSize*)
	  (dotimes (x *smallTabSize*)
	    (setf l (remove (generatePoint (+ x i) (+ y j) l) l))))))

(defun makeTabList (x l)
  (if (> x 0)
      (makeTabList (1- x) (cons x l))
      l))

(defun generatePoint (i j l)
  (dotimes (y *tabSize*)
    (if (> 0 (aref *tab* 0 y))
	(remove (aref *tab* 0 y) l)))
  (dotimes (x *tabSize*)
    (if (> 0 (aref *tab* x 0))
	(remove (aref *tab* x 0) l)))
  (setf (aref *tab* i j) (nth (random (list-length l)) l)))

(defun generatePlayerTab ()
  (erase *playerTab*)
  (dotimes (n 30)
    (let ((x (random *tabSize*))
	  (y (random *tabSize*)))
    (setf (aref *playerTab* x y) (aref *tab* x y))))) 
  



(defun erase (tab)
  (if (equal tab *tab*)
      (setf *tab* (make-array (make-list 2 :initial-element *tabSize*))))
      (if (equal tab *playerTab*)
	  (setf *playerTab* (make-array (make-list 2 :initial-element *tabSize*)))))))
      
  
