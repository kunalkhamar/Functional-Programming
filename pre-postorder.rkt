;; Given a pre-order traversal of a rooted tree of nodes with
;; an arbitrary number of children, construct the tree in
;; memory and print its post-order traversal

;; Example:
;; Input nodes are given as "node-key [space] #-of-children"
;; Input:
;; 1 2
;; 2 1
;; 3 0
;; 4 3
;; 5 0
;; 6 0
;; 7 0
;; 
;; This corresponds to tree
;;   1
;;  / \
;; 2   4
;; |  /|\
;; 3 5 6 7
;; 
;; Output:
;; 3 0
;; 2 1
;; 5 0
;; 6 0
;; 7 0
;; 4 3
;; 1 2

#lang racket

;; Tree node with arbitrary number of children
(struct node (value child-count children) #:transparent)

;; Construct a node from input
;; Void -> Node
(define (construct-root)
  (define ints (map (lambda (x) (string->number x)) (string-split (read-line))))
  (define child-count (second ints))
  (node (first ints) child-count (construct-tree child-count)))

;; Construct n siblings of a tree which are at
;; the same depth
;; Nat -> (listof Node)
(define (construct-tree n)
  (cond
    [(= n 0) empty]
    [else (cons (construct-root)
                (construct-tree (sub1 n)))]))

;; Post-order traversal of a Node
;; Node -> Void
(define (visit-node node)
  (visit-list (node-children node))
  (printf "~a ~a\n" (node-value node) (node-child-count node)))

;; Post-order traversal of a (listof Node)
;; (listof Node) -> Void
(define (visit-list lonode)
  (cond
    [(empty? lonode) (void)]
    [else
     (visit-node (first lonode))
     (visit-list (rest lonode))]))

(define root (construct-root))
(visit-node root)
