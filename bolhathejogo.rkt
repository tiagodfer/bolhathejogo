;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |20190626|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

(define ALTURA 400)
(define LARGURA 400)

;; um bola
;; (make-bola (x y ix iy r c))
(define-struct bola (x y ix iy c r))

(define B1 (make-bola 0 0 1 1 "blue" 10))

;; um quadrado
;; (make-quadrado (x y c l))
(define-struct quadrado (x y c l))

(define Q1 (make-quadrado 100 100 "pink" 10))


;; desenha-b
(define (desenha-b b)
  (place-image
   (circle (bola-r b) "solid" (bola-c b)) (bola-x b) (bola-y b) (empty-scene ALTURA LARGURA)))

;; desenha-q
(define (desenha-q q)
  (place-image
   (square (quadrado-l q) "solid" (quadrado-c q)) (quadrado-x q) (quadrado-y q)
   (empty-scene ALTURA LARGURA)))

;; fora
(define (fora b)
  (not (and (<= 0 (bola-x b) LARGURA) (<= 0 (bola-y b) ALTURA))))

;; bateu
(define (bateu b q)
  (and (= (bola-x b) (quadrado-x q)) (= (bola-y b) (quadrado-y q))))

;; move
(define (move b)
  (make-bola (+ (bola-x b) (bola-ix b)) (+ (bola-y b) (bola-iy b)) (bola-ix b) (bola-iy b) (bola-c b)
             (bola-r b)))

;; move-ate
(define (move-ate b q)
  (cond
    [(fora b) empty]
    [(bateu b q) (cons (desenha-b (make-bola (quadrado-x q)
                                             (quadrado-y q)
                                             (- (bola-ix b))
                                             (- (bola-iy b))
                                             (bola-c b)
                                             (bola-r b)))
                       (cons (desenha-q q)(move-ate (move (make-bola (quadrado-x q)
                                                                     (quadrado-y q)
                                                                     (- (bola-ix b))
                                                                     (- (bola-iy b))
                                                                     (bola-c b)
                                                                     (bola-r b))) q)))]
    [else (cons (desenha-b b) (cons (desenha-q q)(move-ate (move b) q)))]))

;; run
(run-movie 0.001 (move-ate B1 Q1))
