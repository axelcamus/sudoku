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
  (let ((pos (string-to-list (read-line))))
    (if (member (car pos) *alpha*)
	(if (numberp (car (cdr pos)))
	    pos
	    (format t "lettre nombre   ex: A 3~%"))
	(format t "lettre nombre   ex: A 3~%"))))
       
(defun readVal()
  (let ((n (read)))
    (if (numberp n)
	n
	(format t "chiffre entre 1 et 9~%"))))

(defun setValue (grid val pos)
  (let ((x (position (car pos) *alpha*))
	(y (1- (nth 1 pos))))
    (setf (aref grid x y) val)))

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

;;##################### sudoku #######################

(defun play (grid size)
  (draw grid size)
  (let((pos (readpos))
       (val (readval)))
    (if (not (eq (car pos) 'Q))
	(let()
	  (if (and (not (eq pos nil)) (not (eq val nil)))
	      (setvalue grid val pos))
	  (play grid size)))))

(defun sudoku (grid)
  (if (/= (car (array-dimensions grid)) (car (cdr (array-dimensions grid))))
      (format t "Dimensions invalides : x != y~%")
      (let ((size (car (array-dimensions grid))))
	(if (not (= (mod (sqrt size) 1) 0))
	    (format t "Dimensions invalides : la grille doit être divisible en x parties de x*x membres~%")
	    (if (> size (length *alpha*))
		(format t "Dimensions invalides : pas assez de lettres pour représenter la grille~%")
		(play grid size))))))
