(require 2htdp/image)
(require 2htdp/universe)

(define ALTURA 400)
(define LARGURA 400)

;; um elemento bolha de Bolha eh um :
;; (make-bolha (x y ix iy r c d)), onde :
;; - x : Number, coordenada X
;; - y : Number, coordenada Y
;; - ix : Number, incrementos na coordenada X
;; - iy : Number, incrementos na coordenada Y
;; - r : Number, raio de bolha
;; - c : Symbol, cor de bolha
;; - d : Boolean, true se duro, false se mole
(define-struct bolha (x y ix iy cor raio duro))

(define B1 (make-bolha 0 0 1 1 'blue 10 true))

;; um elemento quadrado de Quadrado eh um :
;; (make-quadrado (x y c l)), onde :
;; - x : Number, coordenada X
;; - y : Number, coordenada Y
;; - c : Symbol, cor de quadrado
;; - l : Number, lado de quadrado
(define-struct quadrado (x y c l))

(define Q1 (make-quadrado 100 100 'pink 10))


;; um Cena eh :
;; - ou empty
;; - ou (cons bolha quadrado), onde :
;;     - bolha : Bolha
;;     - quadrado : Quadrado


;; desenha-bolha : Bolha -> Cena
(define (desenha-bolha b)
  (place-image
   (circle (bolha-raio b) 'solid (bolha-cor b)) (bolha-x b) (bolha-y b) (empty-scene ALTURA LARGURA)))


;; desenha-quadrado : Quadrado -> Cena
(define (desenha-quadrado q)
  (place-image
   (square (quadrado-l q) 'solid (quadrado-c q)) (quadrado-x q) (quadrado-y q)
   (empty-scene ALTURA LARGURA)))


;; fora : Bolha -> Boolean
(define (fora b)
  (not (and (<= 0 (bolha-x b) LARGURA) (<= 0 (bolha-y b) ALTURA))))


;; bateu : Bolha Quadrado -> Boolean
(define (bateu b q)
  (and (= (bolha-x b) (quadrado-x q)) (= (bolha-y b) (quadrado-y q))))


;; move : Bolha -> Bolha
(define (move b)
  (make-bolha (+ (bolha-x b) (bolha-ix b)) (+ (bolha-y b) (bolha-iy b)) (bolha-ix b) (bolha-iy b) (bolha-cor b)
             (bolha-raio b) (bolha-duro b)))

;; move-ate : Bolha Quadrado -> Cena
(define (move-ate b q)
  (cond
    [(fora b) empty]
    [(bateu b q) (cons (desenha-bolha (make-bolha (quadrado-x q)
                                             (quadrado-y q)
                                             (- (bolha-ix b))
                                             (- (bolha-iy b))
                                             (bolha-cor b)
                                             (bolha-raio b)
                                             (bolha-duro b)))
                       (cons (desenha-quadrado q)(move-ate (move (make-bolha (quadrado-x q)
                                                                     (quadrado-y q)
                                                                     (- (bolha-ix b))
                                                                     (- (bolha-iy b))
                                                                     (bolha-cor b)
                                                                     (bolha-raio b)
                                                                     (bolha-duro b))) q)))]
    [else (cons (desenha-bolha b) (cons (desenha-quadrado q)(move-ate (move b) q)))]))


;; run
(run-movie 0.001 (move-ate B1 Q1))
