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
  (dotimes (x *tabSize*)
    (let ()
      (format t "~D " (1+ x)))
    (dotimes (y *tabSize*)
      (if (= (mod y *smallTabSize*) 0)
	  (format t " ~D " (aref tab x y))
	  (format t "~D " (aref tab x y))))
    (if (= (mod (+ 1 x) *smallTabSize*) 0)
	(format t "~%~%")
	(format t "~%"))))
      

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
	  (setf *playerTab* (make-array (make-list 2 :initial-element *tabSize*)))))
      

;;#################### GET ###########################

(defun getPos()
  (let ((l (string-to-list (read-line))))
    (if (member (car l) *alpha*)
	(if (<= (1+ (position (car l) *alpha*)) *tabSize*)
	    (if (and (> (car (cdr l)) 0) (<= (car (cdr l)) *tabSize*))
	    l)))))
       
(defun getVal()
  (let ((n (read)))
    (if (and (numberp n) (> n 0) (<= n *tabSize*))
	n)))

(defun setValueToPlayerTab (val l)
  (let ((x (position (car l) *alpha*))
	(y (1- (nth 1 l))))
      (setf (aref *playerTab* x y) val)))

(defun string-to-list (str)
  (if (not (streamp str))
      (string-to-list (make-string-input-stream str))
      (if (listen str)
	  (cons (read str) (string-to-list str))
	  nil)))

(defun file-strings (stream str)
  (if (listen stream)
      (file-strings stream (concatenate 'string str (read-line stream nil)))
      str))

(defun get-file (file)
  (with-open-file (stream file)
    (let ((str ""))
      (car (string-to-list (file-strings stream str))))))


(defun list-to-array (l)
  (make-array (list (length l) (length (first l))) :initial-contents l))

(defun sudoku (n)
  (let ((file nil)
	(grid nil))
    (if (<= n 0)
	(format t "(sudoku n) avec n > 0~%")
	(if (< n 10)
	    (setf file (open (concatenate 'string "./Grids/0" (write-to-string n) ".sudoku") :if-does-not-exist nil))
	    (setf file (open (concatenate 'string "./Grids/" (write-to-string n) ".sudoku") :if-does-not-exist nil))))
    (if (not (eql file nil))
	(let ()
	  (setf grid (list-to-array (get-file file)))
	  (close file)
	  (draw grid))
	(format t "numéro de grille invalide~%"))))




