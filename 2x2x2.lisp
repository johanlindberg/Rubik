;;;; Rubik's 2x2x2 Cube

(defpackage :rubik
  (:use :cl)
  (:export #:make-cube #:scramble #:do-moves #:undo-moves #:match
	   #:U #:Ui #:D #:Di #:F #:Fi #:B #:Bi #:L #:Li #:R #:Ri #:X #:Y #:Z))
(in-package :rubik)

(defvar *moves* (list '(U . Ui) '(Ui . U)
		      '(D . Di) '(Di . D)
		      '(F . Fi) '(Fi . F)
		      '(B . Bi) '(Bi . B)
		      '(L . Li) '(Li . L)
		      '(R . Ri) '(Ri . R)))

;; Public
(defun make-cube (&key (size 2))
  "Returns an array representing a (solved) Rubik's cube of size <size>."
  (let ((result '()))
    (dotimes (f 6) ; face
      (let ((rows '()))
	(dotimes (r size) ; row
	  (let ((row '()))
	    (dotimes (c size) ; column
	      (push f row))
	    (push row rows)))
	(push rows result)))

    (make-array (list 6 size size) :initial-contents (reverse result))))

(defun solvedp (cube)
  "Returns T if <cube> is solved, otherwise NIL."
  (let ((dimensions (array-dimensions cube)))
    (dotimes (f (car dimensions)) ; face
      (let ((color (aref cube f 0 0)))
	(dotimes (r (cadr dimensions)) ; row
	  (dotimes (c (caddr dimensions)) ; column
	    (unless (eq (aref cube f r c) color)
	      (return-from solvedp))))))
    t))

;; Public
(defun scramble (cube &key (n 10))
  "Scrambles <cube> using a random sequence of length <n>."
  (do-moves cube (generate-random-sequence n)))

(defun generate-random-sequence (length)
  (let ((result '())
	(moves (mapcar #'car *moves*)))
    (tagbody
     gen-seq
       (dotimes (i length)
	 ;; available-moves contains all moves that won't undo the last one
	 (let ((available-moves (remove (cdr (assoc (car result) *moves*)) moves)))
	   (push (nth (random (length available-moves)) available-moves) result)))

       ;; if the sequence produces a solved cube we try again
       (when (solvedp (do-moves (make-cube) (reverse result)))
	 (go gen-seq)))

    (reverse result)))

;; Public
(defun do-moves (cube sequence)
  (if (eq sequence '())
      cube
      (do-moves (funcall (car sequence) cube) (cdr sequence))))

;; Public
(defun undo-moves (cube sequence)
  (do-moves cube (reverse (reverse-moves sequence))))

(defun reverse-moves (sequence)
  (if (eq sequence '())
      '()
      (cons (cdr (assoc (car sequence) *moves*))
	    (reverse-moves (cdr sequence)))))

;; Public	
(defun match (cube state)
  (assert (equal (array-dimensions cube)
                 (array-dimensions state)))
  (let ((dimensions (array-dimensions state)))
    (dotimes (f (car dimensions)) ; face
      (dotimes (r (cadr dimensions)) ; row
        (dotimes (c (caddr dimensions)) ;column

          (unless (eq (aref cube f r c)
                      (aref state f r c))
            (when (numberp (aref state f r c))
              (return-from match)))))))
  t)

;; Moves	
(defmacro define-move (name &rest transformations)
  (let ((result '()))
    (dolist (transformation transformations)
      (let ((temp '())
	    (previous '()))
	(dolist (position transformation)
	  (when previous
	    (push `(setf (aref cube ,@previous) (aref cube ,@position)) temp))
	  (setf previous position))
	(push `(let ((temp (aref cube ,@(nth 0 transformation))))
		 ,@(reverse temp)
		 (setf (aref cube ,@(nth (- (length transformation) 1) transformation)) temp))
	      result)))

    `(defun ,name (cube)
       ,@(reverse result)
       (values cube (solvedp cube)))))

(define-move U  ((1 0 0) (5 0 0) (3 0 0) (4 0 0))
                ((1 0 1) (5 0 1) (3 0 1) (4 0 1))
		((0 0 0) (0 1 0) (0 1 1) (0 0 1)))
(define-move Ui ((1 0 0) (4 0 0) (3 0 0) (5 0 0))
                ((1 0 1) (4 0 1) (3 0 1) (5 0 1))
		((0 0 0) (0 0 1) (0 1 1) (0 1 0)))

(define-move D  ((1 1 0) (4 1 0) (3 1 0) (5 1 0))
                ((1 1 1) (4 1 1) (3 1 1) (5 1 1))
		((2 0 0) (2 1 0) (2 1 1) (2 0 1)))
(define-move Di ((1 1 0) (5 1 0) (3 1 0) (4 1 0))
                ((1 1 1) (5 1 1) (3 1 1) (4 1 1))
		((2 0 0) (2 0 1) (2 1 1) (2 1 0)))

(define-move F  ((0 1 0) (4 1 1) (2 0 1) (5 0 0))
                ((0 1 1) (4 0 1) (2 0 0) (5 1 0))
		((1 0 0) (1 1 0) (1 1 1) (1 0 1)))
(define-move Fi ((0 1 0) (5 0 0) (2 0 1) (4 1 1))
                ((0 1 1) (5 1 0) (2 0 0) (4 0 1))
		((1 0 0) (1 0 1) (1 1 1) (1 1 0)))

(define-move B  ((0 0 0) (5 0 1) (2 1 1) (4 1 0))
                ((0 0 1) (5 1 1) (2 1 0) (4 0 0))
		((3 0 0) (3 1 0) (3 1 1) (3 0 1)))
(define-move Bi ((0 0 0) (4 1 0) (2 1 1) (5 0 1))
                ((0 0 1) (4 0 0) (2 1 0) (5 1 1))
		((3 0 0) (3 0 1) (3 1 1) (3 1 0)))

(define-move L  ((0 0 0) (3 1 1) (2 0 0) (1 0 0))
                ((0 1 0) (3 0 1) (2 1 0) (1 1 0))
		((4 0 0) (4 1 0) (4 1 1) (4 0 1)))
(define-move Li ((0 0 0) (1 0 0) (2 0 0) (3 1 1))
                ((0 1 0) (1 1 0) (2 1 0) (3 0 1))
		((4 0 0) (4 0 1) (4 1 1) (4 1 0)))

(define-move R  ((0 0 1) (1 0 1) (2 0 1) (3 1 0))
                ((0 1 1) (1 1 1) (2 1 1) (3 0 0))
		((5 0 0) (5 1 0) (5 1 1) (5 0 1)))
(define-move Ri ((0 0 1) (3 1 0) (2 0 1) (1 0 1))
                ((0 1 1) (3 0 0) (2 1 1) (1 1 1))
		((5 0 0) (5 0 1) (5 1 1) (5 1 0)))

(defun X (cube)
  (L cube)
  (Ri cube))
(defun Xi (cube)
  (Li cube)
  (R cube))

(defun Y (cube)
  (U cube)
  (Di cube))
(defun Yi (cube)
  (Ui cube)
  (D cube))

(defun Z (cube)
  (F cube)
  (Bi cube))
(defun Zi (cube)
  (Fi cube)
  (B cube))

