(defpackage :rubik-test
  (:use :cl :lisp-unit :rubik))
(in-package :rubik-test)

(define-test make-cube
  (let ((cube (rubik:make-cube :size 2)))
    ;; The function make-cube returns a 3d array with 6 faces, 2 rows and 2
    ;; columns.
    (assert-equalp
     (array-dimensions cube)
     '(6 2 2))

    ;; A freshly made cube should also be solved.
    (assert-true (rubik::solvedp cube))))

(define-test scramble
  ;; Scramble must not return a solved cube.
  (dotimes (i 20)
    (dolist (n '(2 4 6 8 10 15 20))
      (let ((cube (rubik:make-cube :size 2)))
	(assert-false (rubik::solvedp (rubik:scramble cube :n n)))))))

(define-test do-moves
  ;; Passing a sequence of moves to do-moves should have the same effect as
  ;; applying each move function in the sequence.
  (let ((cube1 (rubik:make-cube :size 2))
	(cube2 (rubik:make-cube :size 2)))
    (progn
      (U cube1)
      (Di cube1)
      (F cube1)
      (Bi cube1)
      (L cube1)
      (Ri cube1))
    (assert-equalp cube1
     (rubik:do-moves cube2 '(U Di F Bi L Ri)))))

(define-test undo-moves
  (dotimes (i 20)
    (dolist (n '(2 4 6 8 10 15 20))
      (let ((cube (rubik:make-cube :size 2))
	    (seq (rubik::generate-random-sequence n)))
	(rubik:do-moves cube seq)
	(rubik:undo-moves cube seq)
	(unless (assert-true (rubik::solvedp cube))
	  (format t "~&Sequence: ~{~A ~}" seq))))))
    
(define-test match
  (let ((cube nil)
        (state #3A(((t t) (t t))
                   ((t t) (t t)) 
                   ((t t) (t t)) 
                   ((t t) (t t))
                   ((t t) (t t))
                   ((t t) (t t)))))
    (dotimes (i 20)
      (setf cube (rubik:scramble (rubik:make-cube :size 2) :n 10))
      (assert-true (rubik:match cube state)))

    (setf cube (rubik:make-cube :size 2))
    (setf state #3A(((0 0) (0 0))
                    ((t t) (t t)) 
                    ((t t) (t t)) 
                    ((t t) (t t))
                    ((t t) (t t))
                    ((t t) (t t))))
    (assert-true (rubik:match cube state))
    (setf cube (rubik:make-cube :size 2))
    (setf state #3A(((1 1) (1 1))
                    ((t t) (t t)) 
                    ((t t) (t t)) 
                    ((t t) (t t))
                    ((t t) (t t))
                    ((t t) (t t))))
    (assert-false (rubik:match cube state))))