;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname sudoku) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
#|

sudoku app by nora (noramkl) 

what it can do (so far):
- convert list representation of sudoku board to graphics
  - dimensions based around small selection of measurements; easily scaled
- users able to click on box & type in or a delete a number
- all numbers on the board turn red once an incorrect number is inputted
  - with each input, the program checks for if a solution to this board exists

what i still want to implement (for now):
- a backlog of sudoku puzzles organized by difficulty rating
- user ability to choose a level of difficulty and receive a random puzzle
- maximum number of incorrect inputs before they must restart/quit puzzle
- option to show the solution after failing a puzzle

|#

(require spd/tags)
(require 2htdp/image)
(require 2htdp/universe)
(require racket/list)

;; ========= CONSTANTS

(define BG-LENGTH 700)

(define BD-LENGTH 600)
(define C-OUTLINE 1.5)
(define U-OUTLINE (* 2.5 C-OUTLINE))

;---DO NOT ALTER THE CONSTANTS BELOW, just change the dimensions at the top---
(define C-LENGTH (/ (- BD-LENGTH (+ (* C-OUTLINE 6) (* U-OUTLINE 4))) 9))
(define FONT (round (* 0.65 C-LENGTH)))

(define CELL (square C-LENGTH "solid" "white"))
(define WCGAP (rectangle C-OUTLINE 1 "solid" "black"))
(define HCGAP (rectangle 1 C-OUTLINE "solid" "black"))

(define UNIT (above (beside CELL WCGAP CELL WCGAP CELL)
                    HCGAP
                    (beside CELL WCGAP CELL WCGAP CELL)
                    HCGAP
                    (beside CELL WCGAP CELL WCGAP CELL)))
(define WUGAP (rectangle U-OUTLINE 1 "solid" "black"))
(define HUGAP (rectangle 1 U-OUTLINE "solid" "black"))

(define MTS
  (overlay (above HUGAP
                  (beside WUGAP UNIT WUGAP UNIT WUGAP UNIT WUGAP)
                  HUGAP
                  (beside WUGAP UNIT WUGAP UNIT WUGAP UNIT WUGAP)
                  HUGAP
                  (beside WUGAP UNIT WUGAP UNIT WUGAP UNIT WUGAP)
                  HUGAP)
           (square BD-LENGTH "solid" "black")
           (empty-scene BG-LENGTH BG-LENGTH)))

(define START (+ (/ (- BG-LENGTH BD-LENGTH) 2) U-OUTLINE (/ C-LENGTH 2)))
(define START-BD (+ (/ (- BG-LENGTH BD-LENGTH) 2) U-OUTLINE))
;---DO NOT ALTER THE CONSTANTS ABOVE, just change the dimensions at the top---


;; ========= DATA DEFINITIONS

(@htdd Val)
;; Val is one of:
;;  - false
;;  - Natural
;; CONSTRAINT: if Natural is in [0,9]
;; interp. an empty spot, or a number

(define F false)

(@htdd Pos)
;; Pos is Natural
;; CONSTRAINT: Natural is in [0, 80]
;; interp. a position on the board
;;         the row is (quotient p 9)
;;         the column is (remainder p 9)

(define POS
  (list 0  1  2  3  4  5  6  7  8
        9  10 11 12 13 14 15 16 17
        18 19 20 21 22 23 24 25 26
        27 28 29 30 31 32 33 34 35
        36 37 38 39 40 41 42 43 44
        45 46 47 48 49 50 51 52 53
        54 55 56 57 58 59 60 61 62
        63 64 65 66 67 68 69 70 71
        72 73 74 75 76 77 78 79 80))

(@htdd Board)
(define-struct board (vals select))
;; Board is (make-board (listof Val) Pos)
;; CONSTRAINT: length of vals is 81
;; interp. each element in the list corresponds to the value at that position
;;         & the position most recently selected

(define BD0
  (make-board (list F F F F F F F F F
                    F F F F F F F F F
                    F F F F F F F F F
                    F F F F F F F F F
                    F F F F F F F F F
                    F F F F F F F F F
                    F F F F F F F F F
                    F F F F F F F F F
                    F F F F F F F F F) 0))
(define BD1
  (make-board (list 1 F F F F F F F F
                    F F F F 2 6 F F F
                    F F F F F F F F F
                    F F F F F F F 8 F
                    F F F F F F F F F
                    F F 3 F F F F F F
                    F F F F F F F F F
                    F F F F F F F F F
                    F F F F F F F F F) 14))

(define BD2                ;easy
  (make-board (list 2 7 4 F 9 1 F F 5
                    1 F F 5 F F F 9 F
                    6 F F F F 3 2 8 F
                    F F 1 9 F F F F 8
                    F F 5 1 F F 6 F F
                    7 F F F 8 F F F 3
                    4 F 2 F F F F F 9
                    F F F F F F F 7 F
                    8 F F 3 4 9 F F F) 12))

(define BD2s               ;solution to 2
  (make-board (list 2 7 4 8 9 1 3 6 5
                    1 3 8 5 2 6 4 9 7
                    6 5 9 4 7 3 2 8 1
                    3 2 1 9 6 4 7 5 8
                    9 8 5 1 3 7 6 4 2
                    7 4 6 2 8 5 9 1 3
                    4 6 2 7 5 8 1 3 9
                    5 9 3 6 1 2 8 7 4
                    8 1 7 3 4 9 5 2 6) 12))

(define BD3                ;hard
  (make-board (list 5 F F F F 4 F 7 F
                    F 1 F F 5 F 6 F F
                    F F 4 9 F F F F F
                    F 9 F F F 7 5 F F
                    1 8 F 2 F F F F F 
                    F F F F F 6 F F F 
                    F F 3 F F F F F 8
                    F 6 F F 8 F F F 9
                    F F 8 F 7 F F 3 1) 32))

(define BD3s               ;solution to 3
  (make-board (list 5 3 9 1 6 4 8 7 2
                    8 1 2 7 5 3 6 9 4
                    6 7 4 9 2 8 3 1 5
                    2 9 6 4 1 7 5 8 3
                    1 8 7 2 3 5 9 4 6
                    3 4 5 8 9 6 1 2 7
                    9 2 3 5 4 1 7 6 8
                    7 6 1 3 8 2 4 5 9
                    4 5 8 6 7 9 2 3 1) 32))

(define BD4                ;hardest ever? (Dr Arto Inkala)
  (make-board (list F F 5 3 F F F F F 
                    8 F F F F F F 2 F
                    F 7 F F 1 F 5 F F 
                    4 F F F F 5 3 F F
                    F 1 F F 7 F F F 6
                    F F 3 2 F F F 8 F
                    F 6 F 5 F F F F 9
                    F F 4 F F F F 3 F
                    F F F F F 9 7 F F) 29))

(define BD5                 ; no solution 
  (make-board (list 1 2 3 4 5 6 7 8 F 
                    F F F F F F F F 2 
                    F F F F F F F F 3 
                    F F F F F F F F 4 
                    F F F F F F F F 5
                    F F F F F F F F 6
                    F F F F F F F F 7
                    F F F F F F F F 8
                    F F F F F F F F 9) 6))

(define BD6                 ; invalid board
  (make-board (list 1 2 3 4 5 6 7 8 F 
                    F F F F F F F F 2 
                    F F F F F F F 7 3 
                    F F F F F F F F 4 
                    F F F F F F F F 5
                    F F F F F F F F 6
                    F F F F F F F F 7
                    F F F F F F F F 8
                    F F F F F F F F 9) 6))


;; ========= FUNCTIONS

(@htdf main)
(@signature Board -> Board)
;; start the world with (main ...)

(define (main bd)
  (big-bang bd
    (to-draw  render-bd)
    (on-mouse select-pos)
    (on-key   update-pos)))


(@htdf select-pos)
(@signature Board Integer Integer MouseEvent -> Board)
;; updates the selected position based on the location of the mouse press

(@template-origin MouseEvent)

(define (select-pos bd x y me)
  (local [(define (get-group n)
            (cond [(or (< n START-BD) (> n (- BG-LENGTH START-BD))) false]
                  [(<= n (+ START-BD C-LENGTH C-OUTLINE)) 0]
                  [(<= n (+ START-BD (* C-LENGTH 2)
                            (* C-OUTLINE 2))) 1]
                  [(<= n (+ START-BD (* C-LENGTH 3)
                            (* C-OUTLINE 2) U-OUTLINE)) 2]
                  [(<= n (+ START-BD (* C-LENGTH 4)
                            (* C-OUTLINE 3) U-OUTLINE)) 3]
                  [(<= n (+ START-BD (* C-LENGTH 5)
                            (* C-OUTLINE 4) U-OUTLINE)) 4]
                  [(<= n (+ START-BD (* C-LENGTH 6)
                            (* C-OUTLINE 4) (* U-OUTLINE 2))) 5]
                  [(<= n (+ START-BD (* C-LENGTH 7)
                            (* C-OUTLINE 5) (* U-OUTLINE 2))) 6]
                  [(<= n (+ START-BD (* C-LENGTH 8)
                            (* C-OUTLINE 6) (* U-OUTLINE 2))) 7]
                  [else 8]))
          (define col (get-group x))
          (define row (get-group y))]
    (cond [(and (mouse=? me "button-down")
                (not (false? col))
                (not (false? row)))
           (make-board (board-vals bd) (+ (* row 9) col))]
          [else bd])))


(@htdf update-pos)
(@signature Board KeyEvent -> Board)
;; updates the value at the selected position based on keypad input
(check-expect (update-pos BD0 "2")
              (make-board (list 2 F F F F F F F F
                                F F F F F F F F F
                                F F F F F F F F F
                                F F F F F F F F F
                                F F F F F F F F F
                                F F F F F F F F F
                                F F F F F F F F F
                                F F F F F F F F F
                                F F F F F F F F F) 0))
(check-expect (update-pos BD0 "\b")
              (make-board (list F F F F F F F F F
                                F F F F F F F F F
                                F F F F F F F F F
                                F F F F F F F F F
                                F F F F F F F F F
                                F F F F F F F F F
                                F F F F F F F F F
                                F F F F F F F F F
                                F F F F F F F F F) 0))
(check-expect (update-pos BD1 "9")
              (make-board (list 1 F F F F F F F F
                                F F F F 2 9 F F F
                                F F F F F F F F F
                                F F F F F F F 8 F
                                F F F F F F F F F
                                F F 3 F F F F F F
                                F F F F F F F F F
                                F F F F F F F F F
                                F F F F F F F F F) 14))
(check-expect (update-pos BD1 "\b")
              (make-board (list 1 F F F F F F F F
                                F F F F 2 F F F F
                                F F F F F F F F F
                                F F F F F F F 8 F
                                F F F F F F F F F
                                F F 3 F F F F F F
                                F F F F F F F F F
                                F F F F F F F F F
                                F F F F F F F F F) 14))
(check-expect (update-pos BD1 "s")
              (make-board (list 1 F F F F F F F F
                                F F F F 2 6 F F F
                                F F F F F F F F F
                                F F F F F F F 8 F
                                F F F F F F F F F
                                F F 3 F F F F F F
                                F F F F F F F F F
                                F F F F F F F F F
                                F F F F F F F F F) 14))

(@template-origin KeyEvent)

(define (update-pos bd ke)
  (local [(define pos (board-select bd))
          (define brd (board-vals bd))]
    (cond [(key=? ke "\b") (make-board (append (take brd pos)
                                               (list false)
                                               (rest (drop brd pos))) pos)]
          [(not (false? (string->number ke)))
           (make-board (append (take brd pos)
                               (list (string->number ke))
                               (rest (drop brd pos))) pos)]
          [else bd])))


(@htdf solve-bd)
(@signature Board -> Board or false)
;; produces the solution to the board, false if it has none
(check-expect (solve-bd BD2) BD2s)
(check-expect (solve-bd BD3s) BD3s)
(check-expect (solve-bd BD5) false)

(@template-origin Board (listof Board) encapsulated try-catch)

(define (solve-bd bd)
  (local [(define (fn-for-bd bd)
            (if (viable? bd)
                (if (solved? bd)
                    bd
                    (fn-for-lobd (next-bds bd)))
                false))

          (define (fn-for-lobd lobd)
            (cond [(empty? lobd) false]
                  [else
                   (local [(define try (fn-for-bd (first lobd)))]
                     (if (not (false? try))
                         try
                         (fn-for-lobd (rest lobd))))]))]
    (fn-for-bd bd)))


(@htdf viable?)
(@signature Board -> Boolean)
;; produce true if the given board doesn't violate any sudoku rules
(check-expect (viable? BD3) true)
(check-expect (viable? BD2s) true)
(check-expect (viable? BD6) false)

(@template-origin use-abstract-fn)

(define (viable? bd)
  (local [(define (viable lou)
            (cond [(empty? lou) true]
                  [else
                   (and (false? (check-duplicates
                                 (remove-all F (map (λ (u) (first u))
                                                    (first lou)))))
                        (viable (rest lou)))]))

          (define brd
            (build-list 81 (λ (n) (list (list-ref (board-vals bd) n) n))))

          (define rows (group-by (λ (vp) (quotient (second vp) 9)) brd))

          (define cols (group-by (λ (vp) (remainder (second vp) 9)) brd))

          (define grps
            (group-by (λ (vp) (+ (* 10 (quotient (remainder (second vp) 9) 3))
                                 (quotient (quotient (second vp) 9) 3))) brd))]
    
    (and (viable rows) (viable cols)  (viable grps))))


(@htdf next-bds)
(@signature Board -> (listof Board))
;; generate a list of boards with the first false replaced with 0-9
(check-expect (next-bds BD1)
              (list (make-board (list 1 1 F F F F F F F F F F F 2 6 F F F F F F
                                      F F F F F F F F F F F F F 8 F F F F F F F
                                      F F F F F 3 F F F F F F F F F F F F F F F
                                      F F F F F F F F F F F F F F F F F F) 14)
                    (make-board (list 1 2 F F F F F F F F F F F 2 6 F F F F F F
                                      F F F F F F F F F F F F F 8 F F F F F F F
                                      F F F F F 3 F F F F F F F F F F F F F F F
                                      F F F F F F F F F F F F F F F F F F) 14)
                    (make-board (list 1 3 F F F F F F F F F F F 2 6 F F F F F F
                                      F F F F F F F F F F F F F 8 F F F F F F F
                                      F F F F F 3 F F F F F F F F F F F F F F F
                                      F F F F F F F F F F F F F F F F F F) 14)
                    (make-board (list 1 4 F F F F F F F F F F F 2 6 F F F F F F
                                      F F F F F F F F F F F F F 8 F F F F F F F
                                      F F F F F 3 F F F F F F F F F F F F F F F
                                      F F F F F F F F F F F F F F F F F F) 14)
                    (make-board (list 1 5 F F F F F F F F F F F 2 6 F F F F F F
                                      F F F F F F F F F F F F F 8 F F F F F F F
                                      F F F F F 3 F F F F F F F F F F F F F F F
                                      F F F F F F F F F F F F F F F F F F) 14)
                    (make-board (list 1 6 F F F F F F F F F F F 2 6 F F F F F F
                                      F F F F F F F F F F F F F 8 F F F F F F F
                                      F F F F F 3 F F F F F F F F F F F F F F F
                                      F F F F F F F F F F F F F F F F F F) 14)
                    (make-board (list 1 7 F F F F F F F F F F F 2 6 F F F F F F
                                      F F F F F F F F F F F F F 8 F F F F F F F
                                      F F F F F 3 F F F F F F F F F F F F F F F
                                      F F F F F F F F F F F F F F F F F F) 14)
                    (make-board (list 1 8 F F F F F F F F F F F 2 6 F F F F F F
                                      F F F F F F F F F F F F F 8 F F F F F F F
                                      F F F F F 3 F F F F F F F F F F F F F F F
                                      F F F F F F F F F F F F F F F F F F) 14)
                    (make-board (list 1 9 F F F F F F F F F F F 2 6 F F F F F F
                                      F F F F F F F F F F F F F 8 F F F F F F F
                                      F F F F F 3 F F F F F F F F F F F F F F F
                                      F F F F F F F F F F F F F F F F F F) 14)))

(@template-origin genrec accumulator use-abstract-fn)

(define (next-bds bd0)
  (local [(define (next-bds pre p post)
            (if (false? p)
                (build-list 9 (λ (n) (make-board (append pre
                                                         (list (add1 n))
                                                         post)
                                                  (board-select bd0))))
                (next-bds (append pre (list p)) (first post) (rest post))))]
    (next-bds empty (first (board-vals bd0)) (rest (board-vals bd0)))))


(@htdf solved?)
(@signature Board -> Boolean)
;; produces true if the given board is solved
(check-expect (solved? BD2) false)
(check-expect (solved? BD2s) true)
(check-expect (solved? BD3) false)
(check-expect (solved? BD3s) true)

(@template-origin use-abstract-fn)

(define (solved? bd)
  (not (ormap false? (board-vals bd))))


(@htdf render-bd)
(@signature Board -> Image)
;; renders the image of the sudoku board
(check-expect (render-bd BD0) MTS)

(@template-origin genrec accumulator use-abstract-fn)

(define (render-bd bd0)
  ;; n is Natural
  ;; INVARIANT: the position on the board of the value being dealt with
  (local [(define txt-col (if (false? (solve-bd bd0))
                              "red"
                              "black"))
          
          (define (render-bd bd n)
            (cond [(= n 81) (foldr (λ (i rnr) (place-image (first i)
                                                           (second i)
                                                           (third i)
                                                           rnr)) MTS bd)]
                  [else
                   (local [(define row (quotient n 9))
                           (define col (remainder n 9))]
                     (render-bd (append (rest bd)
                                        (list
                                         (list (text (if (false? (first bd))
                                                         ""
                                                         (number->string
                                                          (first bd)))
                                                     FONT txt-col)
                                               (+ START (* col C-LENGTH)
                                                  (* (- col (quotient col 3))
                                                     C-OUTLINE)
                                                  (* (quotient col 3)
                                                     U-OUTLINE))
                                               (+ START (* row C-LENGTH)
                                                  (* (- row (quotient row 3))
                                                     C-OUTLINE)
                                                  (* (quotient row 3)
                                                     U-OUTLINE)))))
                                (add1 n)))]))]
    
    (render-bd (board-vals bd0) 0)))