;;; -----------------------------------------------------------------------------------------------------------
;;; SUDOKU
;;;
;;; by Axel Camus and Nicolas Etcheverry (A4)
;;; github : https://github.com/axelcamus/sudoku
;;;
;;; HOW TO USE
;;;
;;; User functions :
;;; Use (load-grid <file>) to load a grid file (examples are available in "Grids" folder)
;;; You can also use the shortcut (load-grid-n <number>) to load one of the examples
;;; Launch the sudoku with (sudoku <grid>) and follow the instructions
;;; ex : (sudoku (load-grid "./Grids/01.sudoku"))
;;;   or (sudoku (load-grid-n 1))
;;;
;;; STRATEGIES
;;; Use : (solve-grid-random <grid>) to try the random strategy (not very efficient)
;;; Or try a more efficient one : (init-solve-grid-smart <grid>) that works everytime except for the 16th grid
;;;
;;; TOURNAMENT
;;; (init-standalone <grid>)
;;; (main-standalone)
;;; -----------------------------------------------------------------------------------------------------------



;; List of characters to represent the columns of the grid
;; The implementation is dynamic : this list can be modified and expanded
(defparameter *alpha* '(A B C D E F G H I J K L M N O P Q R S T U V W X Y Z))

;;;--------------------------- DRAW ----------------------------

;;draws the first line of the sudoku with the characters in *alpha*
(defun draw-alpha-line(l i size)
  (if(< i size)
     (let ()
       (if (= (mod i (sqrt size)) 0)
	   (format t " ~A " (car l))
	   (format t "~A " (car l)))
       (draw-alpha-line (cdr l) (1+ i) size))
     (format t "~%~%")))

;;draw the rest of the grid
;;the blocs are separated by a blank space
(defun draw (tab size)
  (format t "~c" #\tab)
  (draw-alpha-line *alpha* 0 size)
  (dotimes (x size)
    (format t "~D~c" (1+ x) #\tab)
    (dotimes (y size)
      (if (zerop (mod y (isqrt size)))
	  (format t " ~D " (aref tab x y))
	  (format t "~D " (aref tab x y))))
    (if (zerop (mod (+ 1 x) (isqrt size)))
	(format t "~%~%")
	(format t "~%"))))
      
;;-----------------------------------------------------------------

;;converts a string to a list
;;every string separated by a space becomes an atom of the new list
;;the parenthesis of the string are used to determine those of the new list
(defun string-to-list (str)
  (if (not (streamp str))
      (string-to-list (make-string-input-stream str))
      (if (listen str)
	  (cons (read str) (string-to-list str))
	  nil)))

;;-----------------------------------------------------------------

;;asks the user for coordinates
;;the input must be as follow : "<character> <integer>"
;;ex : "A 1", "B 6", "G 8", "K 7"
(defun readPos()
  (format t "Column Line: ")
  (let ((pos (string-to-list (read-line))))
    (if (member (car pos) *alpha*)
	(if (numberp (car (cdr pos)))
	    pos
	    (format t "Letter number   ex: A 3~%"))
	(format t "Letter number   ex: A 3~%"))))

;;ask the user for an integer value
;;input must be an integer
(defun readVal(size)
  (format t "Value: ")
  (let ((n (read)))
    (if (numberp n)
	n
	(format t "Number between 1 and ~d%" size))))

;;sets the position pos of the grid to val
(defun setValue (grid val pos)
  (let ((x (position (car pos) *alpha*))
	(y (1- (nth 1 pos))))
       (setf (aref grid y x) val)))

;;;------------------------- LOAD GRID -------------------------


;;converts a whole file stream to a single string
(defun file-strings (stream str)
  (if (listen stream)
      (file-strings stream (concatenate 'string str (read-line stream nil)))
      str))

;;converts a file to a list
;;the file must be edited as a lisp list
;;ex : ((1 2 3 4 5) (6 7 8 9 10) (1 5 (6 5 4)))
(defun get-file (file)
  (with-open-file (stream file)
    (let ((str ""))
      (car (string-to-list (file-strings stream str))))))


;;converts a 2 dimensions list to an array
(defun list-to-array (l)
  (make-array (list (length l) (length (first l))) :initial-contents l))

;;returns the array of the grid from the file name or nil if the file is incorrect
;;the file must be edited as a lisp list
(defun load-grid (file-name)
  (let ((file (open file-name :if-does-not-exist nil)))
    (if (not (eql file nil))
	(let ((grid (list-to-array (get-file file))))
	  (close file)
	  grid)
	(format t "~s : no such file~%" file-name))))

;;shortcut to load the nth preset sudoku in the Grids folder
;;to add a new preset, rename your sudoku file "<number>.sudoku" in the Grids folder
(defun load-grid-n (n)
  (if(< n 10)
     (load-grid (concatenate 'string "./Grids/0" (write-to-string n) ".sudoku"))
     (load-grid (concatenate 'string "./Grids/" (write-to-string n) ".sudoku"))))

;;returns a copy of the grid
(defun copy-grid (grid)
  (let*((dimensions (array-dimensions grid))
       (clean-grid (make-array dimensions)))
    (dotimes (i (car dimensions))
      (dotimes (j (cadr dimensions))
	(setf (aref clean-grid i j) (aref grid i j))))
    clean-grid))

 ;;;--------------------------- POSSIBLE VALUES ------------------------------

;;returns all the possibilities on the line y
;;x must be initialized to (size - 1)
;;l is a list of all the possible elements in the grid
(defun possible-values-line(grid x y l)
  (if(>= x 0)
     (possible-values-line grid (1- x) y (remove (aref grid y x) l))
     l))

;;returns all the possibilities on the column x
;;y must be initialized to (size - 1)
;;l is a list of all the possible elements in the grid
(defun possible-values-column(grid x y l)
  (if (>= y 0)
      (possible-values-column grid x (1- y) (remove (aref grid y x) l))
      l))

;;returns all the possible values in a square
;;i and j are the coordinates of a block in the sudoku
;;x and y must be initialized to (isqrt size) which is the length of a block
;;size must be initialized to the length of a block
;;m is a list of all the possible elements in the grid
;;-------------------
;;|     |     |
;;|(0,0)|(1,0)|(2,0)
;;|     |     |
;;|------------------
;;|     |     |
;;|(0,1)|(1,1)|(2,1)
;;|     |     |
;;     ...
;;-------------------
(defun possible-values-block(grid i j x y size l)
  (if (>= x 0)
      (if (> y 0)
	  (possible-values-block grid i j x (- y 1) size (remove (aref grid (+ (* j size) y) (+ (* i size) x)) l))
	  (possible-values-block grid i j (- x 1) (- size 1) size (remove (aref grid (+ (* j size) y) (+ (* i size) x)) l)))
      l))

;;returns all the possible values the square x y could be in a list
(defun possible-values-square(grid i j size)
  (let((small-size (isqrt size))
       (l (make-int-list size)))
    (possible-values-block grid (floor i (isqrt size)) (floor j (isqrt size)) (- small-size 1) (- small-size 1) small-size
			   (possible-values-column grid i (- size 1)
						   (possible-values-line grid (- size 1) j l)))))


;;;-------------------------------- MAIN ------------------------------------

;;draw the grid
;;call (readpos) and (readval) and test if the values are corrects
;;then set the value to the grid
;;verify if the player as won
;;if not the function recall itself to continue to play
(defun play (grid size clean-grid)
  (draw grid size)
  (let((pos (readpos)))
    (if (or (equal pos nil) (>= (position (car pos) *alpha*) size) (> (car (cdr pos)) size) (<= (car (cdr pos)) 0))
	(format t "Letter between ~A and ~A ~%Number between 1 and ~D~%" (car *alpha*) (nth (1- size) *alpha*) size)
	  (if (/= (aref clean-grid (- (cadr pos) 1) (position (car pos) *alpha*)) 0)
	      (format t "Locked square~%")
	      (let((val (readval size)))
		(if (or (eql val nil) (> val size) (<= val 0))
		    (format t "Value between 1 and ~D~%" size)
		    (setvalue grid val pos)))))
    (if (verify grid size)
	(format t "You won ! GG WP :)~%")
	(play grid size clean-grid))))

;;load the grid and check its dimensions
;;save the size and call function play
(defun sudoku (grid)
  (if (/= (car (array-dimensions grid)) (car (cdr (array-dimensions grid))))
      (format t "Invalid dimensions : x != y~%")
      (let ((size (car (array-dimensions grid))))
	(if (not (= (mod (sqrt size) 1) 0))
	    (format t "Invalid dimensions : grid must be divisible by x parts of x*x~%")
	    (if (> size (length *alpha*))
		(format t "Invalid dimensions : not enough letters in the alphabet.~%Try expanding *alpha* in sudoku.lisp with your own letters !~%")
		(play grid size (copy-grid grid)))))))

;;make a list from n to 1
(defun make-int-list (n)
  (if (< n 1)
      nil
      (cons n (make-int-list (1- n)))))

	  
;;;---------------------------------- VERIFY ------------------------------------

;;returns true if all the lines and columns of the grid are correct, nil otherwise
(defun verify-lc (grid n l size)
  (if (> n 0)
      (and
       (zerop (length (possible-values-line grid size n l)))
       (zerop (length (possible-values-column grid n size l)))
       (verify-lc grid (1- n) l size))
      T))

;;returns true if all the blocks of the grid are correct, nil otherwise
(defun verify-blocks (grid i j l size)
  (if (zerop (length (possible-values-block grid i j (- (isqrt size) 1) (- (isqrt size) 1) (isqrt size) l)))
      (if (> i 0)
	  (if (> j 0)
	      (verify-blocks grid i (- j 1) l size)
	      (verify-blocks grid (- i 1) (- (isqrt size) 1) l size))
	  t)
      nil))

;;returns true if the grid is completed and correct
(defun verify  (grid size)
  (let ((l (make-int-list size)))
    (and (verify-lc grid (- size 1) l (- size 1)) (verify-blocks grid (- (isqrt size) 1) (- (isqrt size) 1) l size))))

;;;-------------------------------- STRATEGIES ------------------------------------

;;returns a random value of the list l
(defun random-value-list(l)
  (nth (random (length l)) l))

;;Completes the grid randomly depending on the possible values for each square.
;;This strategy is flawed and will mostly fail.
(defun solve-grid-random (grid)
  (if (/= (car (array-dimensions grid)) (car (cdr (array-dimensions grid))))
      (format t "Invalid dimensions : x != y~%")
      (let ((size (car (array-dimensions grid)))
	    (zero NIL))
	(dotimes (i size)
	  (dotimes (j size)
	    (if(zerop (aref grid j i))
	       (let((possible-values (possible-values-square grid i j size)))
		 (if (eql possible-values NIL)
		     (setf zero t)
		     (setf (aref grid j i) (random-value-list possible-values)))))))
	(draw grid size)
        (if(eql zero T)
	   (format t "Strategy failed :(~%")
	   (format t "Strategy succeded ! :)~%")))))

;;the solved grid 
(defparameter solution nil)

;;writes a list of the options that the square can take in options-grid
;;returns true if no options are possible (then the game is lost)
;;returns false otherwise
(defun list-options(grid options-grid size)
  (let ((stop T))
    (dotimes (x size)
      (dotimes (y size)
	(if(zerop (aref grid y x))
	   (let ((l (possible-values-square grid x y size)))
	     (if (eql l NIL)
		 (setf stop NIL)
		 (setf (aref options-grid y x) l))))))
    stop))

;;returns the smallest length of an option list from option-grid and its position in a list (pos x y)
(defun smallest-length (options-grid size)
  (let ((min-pos (list size NIL NIL)))
    (dotimes (x size)
      (dotimes (y size)
	(when (and (not (eql (aref options-grid y x) NIL)) (< (length (aref options-grid y x)) (car min-pos)))
	  (setf (car min-pos) (length (aref options-grid y x)))
	  (setf (nth 1 min-pos) x)
	  (setf (nth 2 min-pos) y))))
    min-pos))

;;Writes the lone options of options-grid in grid
(defun add-unique-options(grid options-grid size)
  (let ((continue NIL))
    (dotimes (x size)
      (dotimes (y size)
	(when(= (length (aref options-grid y x)) 1)
	  (setf continue T)
	  (setf (aref grid y x) (car (aref options-grid y x)))
	  (setf (aref options-grid y x) NIL))))
    continue))


;;main function of the strategy
;;by default, this function calls "list-options" and "add-unique-options" to write every lone possibility
;;if the grid has no more lone options, the function will try every option of the lowest length option in the grid
;;the first one to complete its grid copies it in "solution"
(defun solve-grid-smart (grid options-grid size)
  (if (list-options grid options-grid size)
      (if (add-unique-options grid options-grid size)
	  (solve-grid-smart grid options-grid size)
	  (let((min-pos (smallest-length options-grid size)))
	    (if (= (car min-pos) size)
	        (setf solution grid)
		(dotimes (i (car min-pos))
		  (let ((copy (copy-grid grid))
			(options-copy (copy-grid options-grid)))
		    (setf (aref copy (nth 2 min-pos) (nth 1 min-pos)) (nth i (aref options-copy (nth 2 min-pos) (nth 1 min-pos))))
		    (setf (aref options-copy (nth 2 min-pos) (nth 1 min-pos)) NIL)
		    (solve-grid-smart copy options-copy size))))))))

;;solves the sudoku and saves it in "solution" 
;;init the grid and call main function 
(defun init-solve-grid-smart (grid)
  (let ((options-grid (make-array (array-dimensions grid) :initial-element NIL))
	(size (car (array-dimensions grid))))
    (solve-grid-smart grid options-grid size)
    solution))


;;------------------------------ TOURNAMENT --------------------------------


(defparameter x 0)
(defparameter y 0)
(defparameter backup nil)
(defparameter fail nil)
(defparameter size_standalone nil)

(defun init-standalone(grid)
  (setf backup (copy-grid grid))
  (setf x 0)
  (setf y 0)
  (setf size_standalone (car (array-dimensions grid)))
  (init-solve-grid-smart grid)
  (if(not (verify solution size_standalone))
     (setf fail t)))

(defun main-standalone()
  (if(eql fail t)
     NIL
     (if(< y size_standalone)
	(let((ret-val (aref solution y x))
	     (ret-x x)
	     (ret-y y))
	  (unless(>= x size_standalone)
	    (setf x (+ x 1)))
	  (when(>= x size_standalone)
	    (setf x 0)
	    (setf y (+ y 1)))
	  (if(zerop (aref backup ret-y ret-x))
	     (values ret-x ret-y ret-val)
	     (main-standalone)))
	nil)))
