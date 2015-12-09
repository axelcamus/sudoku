(defparameter *alpha* '(A B C D E F G H I J K L M N O P Q R S T U V W X Y Z))

;;############# DRAW ####################################

(defun draw-alpha-line(l i size)
  (if(< i size)
     (let ()
       (if (= (mod i (sqrt size)) 0)
	   (format t " ~A " (car l))
	   (format t "~A " (car l)))
       (draw-alpha-line (cdr l) (1+ i) size))
     (format t "~%~%")))

(defun draw (tab size)
  (format t "~c" #\tab)
  (draw-alpha-line *alpha* 0 size)
  (dotimes (x size)
    (let ()
      (format t "~D~c" (1+ x) #\tab))
    (dotimes (y size)
      (if (= (mod y (sqrt size)) 0)
	  (format t " ~D " (aref tab x y))
	  (format t "~D " (aref tab x y))))
    (if (= (mod (+ 1 x) (sqrt size)) 0)
	(format t "~%~%")
	(format t "~%"))))
      

;;#################### GET ###########################

(defun readPos()
  (format t "Colonne Ligne: ")
  (let ((pos (string-to-list (read-line))))
    (if (member (car pos) *alpha*)
	(if (numberp (car (cdr pos)))
	    pos
	    (format t "Lettre nombre   ex: A 3~%"))
	(format t "Lettre nombre   ex: A 3~%"))))
       
(defun readVal()
  (format t "Valeur: ")
  (let ((n (read)))
    (if (numberp n)
	n
	(format t "Chiffre entre 1 et 9~%"))))

(defun setValue (grid val pos)
  (let ((x (position (car pos) *alpha*))
	(y (1- (nth 1 pos))))
       (setf (aref grid y x) val)))

;;############# LOAD GRID #############################

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


(defun load-grid (file-name)
  (let ((file (open file-name :if-does-not-exist nil)))
    (if (not (eql file nil))
	(let ((grid (list-to-array (get-file file))))
	  (close file)
	  grid)
	(format t "~s : fichier inexistant~%" file-name))))

(defun load-grid-n (n)
  (if(< n 10)
     (load-grid (concatenate 'string "./Grids/0" (write-to-string n) ".sudoku"))
     (load-grid (concatenate 'string "./Grids/" (write-to-string n) ".sudoku"))))

(defun copy-grid (grid)
  (let*((dimensions (array-dimensions grid))
       (clean-grid (make-array dimensions)))
    (dotimes (i (car dimensions))
      (dotimes (j (cadr dimensions))
	(setf (aref clean-grid i j) (aref grid i j))))
    clean-grid))

;;##################### sudoku #######################

(defun play (grid size clean-grid)
  (draw grid size)
  (let((pos (readpos)))
    (if (or (equal pos nil) (>= (position (car pos) *alpha*) size) (> (car (cdr pos)) size) (<= (car (cdr pos)) 0))
	(format t "Lettre entre ~A et ~A ~%Nombre entre 1 et ~D~%" (car *alpha*) (nth (1- size) *alpha*) size)
	  (if (/= (aref clean-grid (- (cadr pos) 1) (position (car pos) *alpha*)) 0)
	      (format t "Case non modifiable.~%")
	      (let((val (readval)))
		(if (or (eql val nil) (> val size) (<= val 0))
		    (format t "Valeur entre 1 et ~D~%" size)
		    (setvalue grid val pos)))))
    (if (verify grid size)
	(format t "Vous avez gagné ! :)~%")
	(play grid size clean-grid))))

(defun sudoku (grid)
  (if (/= (car (array-dimensions grid)) (car (cdr (array-dimensions grid))))
      (format t "Dimensions invalides : x != y~%")
      (let ((size (car (array-dimensions grid))))
	(if (not (= (mod (sqrt size) 1) 0))
	    (format t "Dimensions invalides : la grille doit être divisible en x parties de x*x membres~%")
	    (if (> size (length *alpha*))
		(format t "Dimensions invalides : pas assez de lettres pour représenter la grille~%")
		(play grid size (copy-grid grid)))))))

(defun make-int-list (n)
  (if (< n 1)
      nil
      (cons n (make-int-list (1- n)))))

;;######################## VERIFY ####################################

(defun verify-line (grid x y l)
  (if (>= x 0)
      (verify-line grid (1- x) y (remove (aref grid y x) l))
      (= (length l) 0)))
	

(defun verify-column (grid x y l)
  (if (>= y 0)
      (verify-column grid x (1- y) (remove (aref grid y x) l))
      (= (length l) 0)))

(defun verify-block (grid i j x y l size)
  (if (>= x 0)
      (if (> y 0)
	  (verify-block grid i j x (- y 1) (remove (aref grid (+ (* i size) x) (+ (* j size) y)) l) size)
	  (verify-block grid i j (- x 1) (- size 1) (remove (aref grid (+ (* i size) x) (+ (* j size) y)) l) size))
      (= (length l) 0)))

(defun verify-lc (grid n l size)
  (if (> n 0)
      (and
       (verify-line grid size n l)
       (verify-column grid n size l)
       (verify-lc grid (1- n) l size))
      T))

(defun verify-blocks (grid i j l size)
  (if (verify-block grid i j (- (isqrt size) 1) (- (isqrt size) 1) l (isqrt size))
      (if (> i 0)
	  (if (> j 0)
	      (verify-blocks grid i (- j 1) l size)
	      (verify-blocks grid (- i 1) (- (isqrt size) 1) l size))
	  t)
      nil))

(defun verify (grid size)
  (let ((l (make-int-list size)))
    (and (verify-lc grid (- size 1) l (- size 1)) (verify-blocks grid (- (isqrt size) 1) (- (isqrt size) 1) l size))))

	  
      

