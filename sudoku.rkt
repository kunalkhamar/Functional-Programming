;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname sudoku) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; A SudokuDigit is one of:
;; * '?
;; * 1 <= Nat <= 9

;; A Puzzle is a (listof (listof SudokuDigit))
;; requires: the list and all sublists have a length of 9

;; A Solution is a Puzzle
;; requires: none of the SudokuDigits are '?
;;           the puzzle satisfies the number placement 
;;             rules of sudoku

;; A SudokuIndex is a Nat
;; requires: 1 <= Nat <= 9

;; A SubGrid is a (listof (listof SudokuDigit))
;; requires: the list and all sublists have a length of 3
;; (Note: these are the 9 SubGrids on each Puzzle)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sample sudoku puzzles

(define veryeasy
  '((? 4 5 8 9 3 7 1 6)
    (8 1 3 5 7 6 9 2 4)
    (7 6 9 2 1 4 5 3 8)
    (5 3 6 9 8 7 1 4 2)
    (4 9 2 1 6 5 8 7 3)
    (1 7 8 4 3 2 6 5 9)
    (6 8 4 7 2 1 3 9 5)
    (3 2 1 6 5 9 4 8 7)
    (9 5 7 3 4 8 2 6 1)))

(define veryeasy-solved
  '((2 4 5 8 9 3 7 1 6)
    (8 1 3 5 7 6 9 2 4)
    (7 6 9 2 1 4 5 3 8)
    (5 3 6 9 8 7 1 4 2)
    (4 9 2 1 6 5 8 7 3)
    (1 7 8 4 3 2 6 5 9)
    (6 8 4 7 2 1 3 9 5)
    (3 2 1 6 5 9 4 8 7)
    (9 5 7 3 4 8 2 6 1)))

;; the above puzzle with more blanks:
(define easy
  '((? 4 5 8 ? 3 7 1 ?)
    (8 1 ? ? ? ? ? 2 4)
    (7 ? 9 ? ? ? 5 ? 8)
    (? ? ? 9 ? 7 ? ? ?)
    (? ? ? ? 6 ? ? ? ?)
    (? ? ? 4 ? 2 ? ? ?)
    (6 ? 4 ? ? ? 3 ? 5)
    (3 2 ? ? ? ? ? 8 7)
    (? 5 7 3 ? 8 2 6 ?)))

;; the puzzle listed on wikipedia:
(define wikipedia '((5 3 ? ? 7 ? ? ? ?)
                    (6 ? ? 1 9 5 ? ? ?)
                    (? 9 8 ? ? ? ? 6 ?)
                    (8 ? ? ? 6 ? ? ? 3)
                    (4 ? ? 8 ? 3 ? ? 1)
                    (7 ? ? ? 2 ? ? ? 6)
                    (? 6 ? ? ? ? 2 8 ?)
                    (? ? ? 4 1 9 ? ? 5)
                    (? ? ? ? 8 ? ? 7 9)))

;; A blank puzzle template:
(define blank '((? ? ? ? ? ? ? ? ?)
                (? ? ? ? ? ? ? ? ?)
                (? ? ? ? ? ? ? ? ?)
                (? ? ? ? ? ? ? ? ?)
                (? ? ? ? ? ? ? ? ?)
                (? ? ? ? ? ? ? ? ?)
                (? ? ? ? ? ? ? ? ?)
                (? ? ? ? ? ? ? ? ?)
                (? ? ? ? ? ? ? ? ?)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;; Global constants
(define quest '?)
(define all-moves '(1 2 3 4 5 6 7 8 9))


;; Examples:
(check-expect (solved? veryeasy-solved) true)
(check-expect (solved? veryeasy) false)

(define solved?
  (lambda (board)
    (andmap (lambda (lst)
              (andmap number? lst)) 
            board)))

;; Tests:
(check-expect (solved? easy) false)
(check-expect (solved? wikipedia) false)


;; (sudoku puzzle)
;;   Produces solution of given sudoku puzzle, or false
;;   if none exists
;; sudoku: Puzzle -> (anyof Solution false)
;; Examples:
(check-expect (sudoku veryeasy) veryeasy-solved)
(check-expect (sudoku easy) veryeasy-solved)

(define (sudoku puzzle)
  (local
    [;; (neighbours state)
     ;;   Produces all possible 'neighbours', i.e.
     ;;   puzzle states that can be obtained after a
     ;;   valid move on 'state'
     ;;   (with respect to ONE undetermined cell, i.e., there can
     ;;    be no more than 9 neighbours)
     ;; neighbours: Puzzle -> (listof Puzzle)
     (define (neighbours state)
       (local
         [;; (first-n list n)
          ;;   Produces the first n elements of the
          ;;   given list
          ;; first-n: (listof Any) Nat -> (listof Any)
          (define (first-n list n)
            (cond
              [(or (= n 0) (empty? list)) empty]
              [else
               (cons (first list) (first-n (rest list) (sub1 n)))]))
          
          ;; (rest-n list n)
          ;;   Produces the elements of the list following
          ;;   the n-th element. In other words, discards
          ;;   the first n elements from the given list
          ;;   and returns the result
          ;; rest-n: (listof Any) Nat -> (listof Any)
          (define (rest-n list n)
            (cond
              [(or (= n 0) (empty? list)) list]
              [else
               (rest-n (rest list) (sub1 n))]))
          
          ;; (dedup alon)
          ;;   Produces a list with only one occurrence of each
          ;;   element of alon
          ;; dedup: (listof Num) -> (listof Num)
          ;; (note: code reused from previous assignment)
          (define (dedup alon)
            (foldr (lambda (num lon)
                     (cond [(empty? lon) (list num)]
                           [(member? num lon) lon]
                           [else (cons num lon)]))
                   empty alon))
          
          ;; (get-row board index)
          ;;    Gets the row of the board at position index
          ;; get-row: Puzzle SudokuIndex -> (listof SudokuDigit)
          (define (get-row board index)
            (first (rest-n board (sub1 index))))
          
          ;; (get-col board index)
          ;;    Gets the column of the board at position index
          ;; get-col: Puzzle SudokuIndex -> (listof SudokuDigit)
          (define (get-col board index)
            (map (lambda (lst)
                   (first (rest-n lst (sub1 index))))
                 board))
          
          ;; (get-sublist-start index)
          ;;    Determines the index of the start of the sublist
          ;;    to which index belongs
          ;;    e.g. 3 returns 1, and 4 returns 4
          ;; get-sublist-start: SudokuIndex -> (anyof 1 4 7)
          (define (get-sublist-start index)
            (add1 (* 3 (quotient (sub1 index) 3))))
          
          ;; (subgrid board r c)
          ;;    Produces the SubGrid on board that contains
          ;;    the cell (r,c)
          ;; subgrid: Puzzle SudokuIndex SudokuIndex -> SubGrid
          (define (subgrid board r c)
            (local
              [(define (bottom-left-subgrid board)
                 (map (lambda (lst)
                        (first-n lst 3)) 
                      (first-n board 3)))]
              (bottom-left-subgrid 
               (map (lambda (lst)
                      (rest-n lst (sub1 c)))
                    (rest-n board (sub1 r))))))
          
          ;; (find-next-quest state cur-row)
          ;;   Determines a cell on the board containing a '?
          ;; find-next-quest: Puzzle SudokuIndex -> 
          ;;                    (list SudokuIndex SudokuIndex)
          ;; requires: at least one such cell exists in state
          (define (find-next-quest state cur-row)
            (cond
              [(member? quest (first state))
               (local
                 [(define (get-index elem lst cur-ind)
                    (cond
                      [(equal? (first lst) elem) cur-ind]
                      [else (get-index elem (rest lst) (add1 cur-ind))]))]
                 (list cur-row (get-index quest (first state) 1)))]
              [else
               (find-next-quest (rest state) (add1 cur-row))]))
          
          ;; (replace new-elem lst index)
          ;;    Replaces the element of lst at index with new-elem
          ;; replace: Any (listof Any) SudokuIndex -> (listof Any)
          (define (replace new-elem lst index)
            (cond
              [(= index 1) (cons new-elem (rest lst))]
              [else (cons (first lst) 
                          (replace new-elem (rest lst) (sub1 index)))]))
          
          ;; (make-move move r c board)
          ;;    Performs the 'move', i.e. puts move on cell
          ;;    (r,c) of board
          ;; make-move: SudokuDigit SudokuIndex SudokuIndex Puzzle -> Puzzle
          (define (make-move move r c board)
            (replace (replace move (get-row board r) c) 
                     board r))
          
          ;; (generate-legal-moves loc subgrid)
          ;;    Produces a list of all possible SudokuIndexes
          ;;    that can be placed in loc based on subgrid
          ;;    and state
          ;; generate-legal-moves: (list SudokuIndex SudokuIndex) SubGrid
          ;;                         -> (listof SudokuIndex)
          (define (generate-legal-moves loc subgrid)
            (local
              [(define illegal-moves
                 (dedup 
                  (filter (lambda (elem)
                            (not (equal? elem quest)))
                          (append (get-row state (first loc))
                                  (get-col state (second loc))
                                  (first cur-subgrid) 
                                  (second cur-subgrid) 
                                  (third cur-subgrid)))))]
              (filter (lambda (elem)
                        (not (member? elem illegal-moves)))
                      all-moves)))
          
          
          ;; Local constants
          (define loc (find-next-quest state 1))
          (define cur-subgrid (subgrid state 
                                       (get-sublist-start (first loc)) 
                                       (get-sublist-start (second loc))))
          (define legal-moves (generate-legal-moves loc cur-subgrid))]
         
         (cond
           [(empty? legal-moves) empty]
           [else
            (map (lambda (move)
                   (make-move move (first loc) (second loc) state))
                 legal-moves)])))]
    
    (find-final puzzle neighbours solved?)))

;; Tests:
(check-expect (sudoku wikipedia) 
              (list (list 5 3 4 6 7 8 9 1 2)
                    (list 6 7 2 1 9 5 3 4 8)
                    (list 1 9 8 3 4 2 5 6 7)
                    (list 8 5 9 7 6 1 4 2 3)
                    (list 4 2 6 8 5 3 7 9 1)
                    (list 7 1 3 9 2 4 8 5 6)
                    (list 9 6 1 5 3 7 2 8 4)
                    (list 2 8 7 4 1 9 6 3 5)
                    (list 3 4 5 2 8 6 1 7 9)))
(check-expect (sudoku blank) 
              (list (list 1 2 3 4 5 6 7 8 9)
                    (list 4 5 6 7 8 9 1 2 3)
                    (list 7 8 9 1 2 3 4 5 6)
                    (list 2 1 4 3 6 5 8 9 7)
                    (list 3 6 5 8 9 7 2 1 4)
                    (list 8 9 7 2 1 4 3 6 5)
                    (list 5 3 1 6 4 2 9 7 8)
                    (list 6 4 2 9 7 8 5 3 1)
                    (list 9 7 8 5 3 1 6 4 2)))
(check-expect (sudoku
               '((1 2 3 4 5 6 7 8 ?)
                 (? ? ? ? ? ? ? ? 2)
                 (? ? ? ? ? ? ? ? 3)
                 (? ? ? ? ? ? ? ? 4)
                 (? ? ? ? ? ? ? ? 5)
                 (? ? ? ? ? ? ? ? 6)
                 (? ? ? ? ? ? ? ? 7)
                 (? ? ? ? ? ? ? ? 8)
                 (? ? ? ? ? ? ? ? 9))) 
              false)


;; (find-final initial-state neighbours solved?) backtracking search algorithm
;;   produces a solution to a (implicit) graph search, 
;;   or false if no solution exists
;;   solved? determines if a state is a solution.
;;   neighbours produces a list of legal next states from a given state.
;;   X is the State
;; find-final: X (X -> (listof X)) (X -> Bool) -> (anyof false X)
(define (find-final initial-state neighbours solved?)
  (local
    [;; (find-final/single state) searches outward from state 
     ;;   looking for a solution
     ;; find-final/single: X -> (anyof false X)
     (define (find-final/single state)
       (cond [(solved? state) state]
             [else (find-final/list (neighbours state))]))
     
     ;; (find-final/list lostate) searches from every state in lostate
     ;;   looking for a solution
     ;; find-final/list: (listof X) -> (anyof false X)
     (define (find-final/list lostate)
       (cond [(empty? lostate) false]
             [else
              (local [(define fresult (find-final/single (first lostate)))]
                (cond [(false? fresult) (find-final/list (rest lostate))]
                      [else fresult]))]))]
    
    (find-final/single initial-state)))