(require 2htdp/image)
(require 2htdp/universe)

;; altura da tela
(define ALTURA 400)
;; largura da tela
(define LARGURA 400)
;; margem do canhao em relação a borda
(define MARGEM 50)

;; um elemento bolha do conjunto Bolha é
;; (make-bolha x y ix iy c r d i), onde:
;; - x: Number, coordenada X
;; - y: Number, coordenada Y
;; - ix: Number, valor absoluto dos incrementos na coordenada X
;; - iy: Number, valor absoluto dos incrementos na coordenada Y
;; - c: String, cor de bolha
;; - r: Number, raio de bolha
;; - d: Boolean, true se duro, false se mole
;; - i: Boolean, verifica se a bolha não estourou
(define-struct bolha (x y ix iy cor raio dura inteira))
;; exemplos:
(define B1 (make-bolha 0 0 1 1 "blue" 10 true true))
(define B2 (make-bolha 0 0 2 2 "medium cyan" 12 false true))
(define B3 (make-bolha 0 0 2 2 "medium cyan" 12 false false))
(define B4 (make-bolha 401 401 1 1 "blue" 10 false true))


;; um elemento quadrado do conjunto Quadrado é:
;; (make-quadrado x y c l d), onde:
;; - x: Number, coordenada X
;; - y: Number, coordenada Y
;; - c: String, cor de quadrado
;; - l: Number, lado de quadrado
;; - d: Boolean, false se mole, true se duro.
(define-struct quadrado (x y c l duro))
;; exemplos:
(define Q1 (make-quadrado 100 100 "pink" 10 false))
(define Q2 (make-quadrado 120 290 "black" 10 false))
(define Q3 (make-quadrado 155 65 "red" 10 false))


;; um elemento canhão do conjunto Canhão é:
;; (make-canhao p), onde:
;; - p: Symbol, 'SD para superior direito, 'ID para inferior direito,
;;     'SE para superior esquerdo, 'IE para inferior esquerdo.
(define-struct canhao (posicao))
;; exemplos :
(define C1 (make-canhao 'SE))
(define C2 (make-canhao 'IE))
(define C3 (make-canhao 'ID))
(define C4 (make-canhao 'SD))


;; uma ListaDeBolhas é:
;; - ou empty
;; - ou (cons b l), onde:
;;     - b: Bolha
;;     - l: ListaDeBolhas
(define LB1 (cons B1 (cons B2 empty)))
(define LB2 empty)
(define LB3 (cons B3 empty))
(define LB4 (cons B4 empty))
(define LB5 (cons B1 empty))


;; uma ListaDeQuadrados é:
;; - ou empty
;; - ou (cons q l), onde:
;;     - q: Quadrados
;;     - l: ListaDeQuadrados
(define LQ1 (cons Q1 (cons Q2 (cons Q3 empty))))
(define LQ2 empty)


;; uma ListaDeCenas é:
;; - ou empty
;; - ou (cons c l), onde:
;;     - c: Cena
;;     - l: ListaDeCenas
(define LC1 empty)


;; bolhas-estouradas: ListaDeBolhas -> Boolean
;; objetivo: Se todas as bolhas estouraram, retorna true, se ainda existe alguma bolha, retorna false.
;; exemplos:
;; (bolhas-estouradas LB1) = false
;; (bolhas-estouradas LB2) = true
;; (bolhas-estouradas LB3) = true
(define (bolhas-estouradas ldb)
  (cond
    [(empty? ldb) true]
    [(bolha-inteira (first ldb)) false]
    [else (bolhas-estouradas (rest ldb))]))

(check-expect (bolhas-estouradas LB1) false)
(check-expect (bolhas-estouradas LB2) true)
(check-expect (bolhas-estouradas LB3) true)


;; estoura-bolha: Bolha -> Bolha
;; objetivo: Estoura a bolha informada.
;; exemplos:
;; (estoura-bolha B2) = B3
;; (estoura-bolha B3) = B3
(define (estoura-bolha b)
  (make-bolha (bolha-x b) (bolha-y b) (bolha-ix b) (bolha-iy b) (bolha-cor b) (bolha-raio b)
              (bolha-dura b) false))

(check-expect (estoura-bolha B2) B3)
(check-expect (estoura-bolha B3) B3)


;; move-bolha: Bolha -> Bolha
;; objetivo: Move a bolha informada.
;; exemplos:
;; (move-bolha B1) = (make-bolha 1 1 1 1 "blue" 10 true true)
(define (move-bolha b)
  (make-bolha (+ (bolha-x b) (bolha-ix b)) (+ (bolha-y b) (bolha-iy b)) (bolha-ix b) (bolha-iy b)
              (bolha-cor b) (bolha-raio b) (bolha-dura b) (bolha-inteira b)))

(check-expect (move-bolha B1) (make-bolha 1 1 1 1 "blue" 10 true true))


;; inverte-sentido: Bolha -> Bolha
;; objetivo: Inverte o sentido da bolha informada.
;; exemplos:
;; (inverte-sentido B1) = (make-bolha 0 0 -1 -1 "blue" 10 true true)
(define (inverte-sentido b)
  (make-bolha (bolha-x b) (bolha-y b) (- (bolha-ix b)) (- (bolha-iy b)) (bolha-cor b) (bolha-raio b)
              (bolha-dura b) (bolha-inteira b)))

(check-expect (inverte-sentido B1) (make-bolha 0 0 -1 -1 "blue" 10 true true))


;; colidindo: Bolha ListaDeQuadrados -> Boolean
;; objetivo: Retorna true se a bolha colide com algum quadrado da lista, senão retorna false.
;; exemplos:
;; (colidindo B1 LQ1) = false
;; (colidindo B1 LQ2) = false
(define (colidindo b ldq)
  (cond
    [(empty? ldq) false]
    [(and (> (bolha-x b) (- (quadrado-x (first ldq)) (/ (quadrado-l (first ldq)) 2)))
          (< (bolha-x b)(+ (quadrado-x (first ldq)) (/ (quadrado-l (first ldq)) 2)))
          (> (bolha-y b) (- (quadrado-y (first ldq)) (/ (quadrado-l (first ldq)) 2)))
          (< (bolha-y b) (+ (quadrado-y (first ldq)) (/ (quadrado-l (first ldq)) 2)))) true]
    [else (colidindo b (rest ldq))]))

(check-expect (colidindo B1 LQ1) false)
(check-expect (colidindo B1 LQ2) false)


;; QuadradoOuFalse é:
;; - ou Quadrado
;; - ou Boolean


;; quem-colide: Bolha ListaDeQuadrados -> QuadradoOuFalse
;; objetivo: Retorna o quadrado que colide com a bolha,  caso nenhum colida, retorna false.
;; exemplos:
;; (quem-colide B1 LQ1) = false
;; (quem-colide B1 LQ2) = false
(define (quem-colide b ldq)
  (cond
    [(empty? ldq) false]
    [(and (> (bolha-x b) (- (quadrado-x (first ldq)) (/ (quadrado-l (first ldq)) 2)))
          (< (bolha-x b) (+ (quadrado-x (first ldq)) (/ (quadrado-l (first ldq)) 2)))
          (> (bolha-y b) (- (quadrado-y (first ldq)) (/ (quadrado-l (first ldq)) 2)))
          (< (bolha-y b) (+ (quadrado-y (first ldq)) (/ (quadrado-l (first ldq)) 2)))) (first ldq)]
    [else (quem-colide b (rest ldq))]))

(check-expect (quem-colide B1 LQ1) false)
(check-expect (quem-colide B1 LQ2) false)


;; fora-dos-limites: Bolha -> Boolean
;; objetivo: Informa se determinada bolha está fora dos limites.
;; exemplos:
;; (fora-dos-limites B1) = false
;; (fora-dos-limites B4) = true
(define (fora-dos-limites b)
  (cond
    [(or (> (bolha-x b) LARGURA) (< (bolha-x  b) 0) (> (bolha-x b) ALTURA) (< (bolha-x b) 0)) true]
    [else false]))

(check-expect (fora-dos-limites B1) false)
(check-expect (fora-dos-limites B4) true)


;; atualiza-bolhas: ListaDeBolhas -> ListaDeBolhas
;; objetivo: Estoura ou move bolhas de acordo com sua posição.
;; exemplos:
;; (atualiza-bolhas LB1 LQ1) = (list (make-bolha 1 1 1 1 "blue" 10 true true)
;;                                   (make-bolha 2 2 2 2 "medium cyan" 12 false true))
;; (atualiza-bolhas LB2 LQ1) = empty
;; (atualiza-bolhas LB4 LQ1) = (cons (make-bolha 401 401 1 1 "blue" 10 false false) empty)
(define (atualiza-bolhas ldb ldq)
  (cond
   [(empty? ldb) empty]
   [(and (colidindo (first ldb) ldq) (bolha-inteira (first ldb)))
    (cond
      [(not (bolha-dura (first ldb))) (cons (estoura-bolha (first ldb))
                                            (atualiza-bolhas (rest ldb) ldq))]
      [(quadrado-duro (quem-colide (first ldb) ldq)) (cons (estoura-bolha (first ldb))
                                                           (atualiza-bolhas (rest ldb) ldq))]
      [else (cons (move-bolha (inverte-sentido (first ldb))) (atualiza-bolhas (rest ldb) ldq))])]
   [(and (fora-dos-limites (first ldb)) (bolha-inteira (first ldb)))
    (cons (estoura-bolha (first ldb))(atualiza-bolhas (rest ldb) ldq))]
   [else (cons (move-bolha (first ldb)) (atualiza-bolhas (rest ldb) ldq))]))

(check-expect (atualiza-bolhas LB1 LQ1) (list (make-bolha 1 1 1 1 "blue" 10 true true)
                                              (make-bolha 2 2 2 2 "medium cyan" 12 false true)))
(check-expect (atualiza-bolhas LB2 LQ1) empty)
(check-expect (atualiza-bolhas LB4 LQ1) (cons (make-bolha 401 401 1 1 "blue" 10 false false) empty))


;; desenha-bolha: Bolha -> Imagem
;; objetivo: Fornece a representação gráfica da bolha informada.
;; exemplos:
;; (desenha-bolha B1) = (circle (bolha-raio B1) "solid" (bolha-cor B1))
(define (desenha-bolha b)
  (circle (bolha-raio b) "solid" (bolha-cor b)))

(check-expect (desenha-bolha B1) (circle (bolha-raio B1) "solid" (bolha-cor B1)))


;; desenha-quadrado: Quadrado -> Imagem
;; objetivo: Fornece a representação gráfica do quadrado informada.
;; exemplos:
;; (desenha-quadrado Q1) = (square (quadrado-l Q1) "solid" (quadrado-c Q1))
(define (desenha-quadrado q)
  (square (quadrado-l q) "solid" (quadrado-c q)))

(check-expect (desenha-quadrado Q1) (square (quadrado-l Q1) "solid" (quadrado-c Q1)))


;; gera-canhao: Canhão -> Cena
;; objetivo: Fornece a representação gráfica do canhão informado.
;; exemplos:
;; (gera-canhao C1) = (place-image (square (* MARGEM 2) "solid" "black") 0 0
;;                                 (empty-scene LARGURA ALTURA)))
;; (gera-canhao C2) = (place-image (square (* MARGEM 2) "solid" "black") 0 ALTURA
;;                                 (empty-scene LARGURA ALTURA)))
;; (gera-canhao C3) = (place-image (square (* MARGEM 2) "solid" "black") LARGURA ALTURA
;;                                 (empty-scene LARGURA ALTURA)))
;; (gera-canhao C4) = (place-image (square (* MARGEM 2) "solid" "black") LARGURA 0
;;                                 (empty-scene LARGURA ALTURA)))
(define (gera-canhao c)
  (cond
    [(symbol=? (canhao-posicao c) 'SE)
     (place-image (square (* MARGEM 2) "solid" "black") 0 0 (empty-scene LARGURA ALTURA))]
    [(symbol=? (canhao-posicao c) 'IE)
     (place-image (square (* MARGEM 2) "solid" "black") 0 ALTURA (empty-scene LARGURA ALTURA))]
    [(symbol=? (canhao-posicao c) 'SD)
     (place-image (square (* MARGEM 2) "solid" "black") LARGURA 0 (empty-scene LARGURA ALTURA))]
    [(symbol=? (canhao-posicao c) 'ID)
     (place-image (square (* MARGEM 2) "solid" "black") LARGURA ALTURA (empty-scene LARGURA ALTURA))]
    [else (empty-scene LARGURA ALTURA)]))

(check-expect (gera-canhao C1)
              (place-image (square (* MARGEM 2) "solid" "black") 0 0 (empty-scene LARGURA ALTURA)))
(check-expect (gera-canhao C2)
              (place-image (square (* MARGEM 2) "solid" "black") 0 ALTURA
                           (empty-scene LARGURA ALTURA)))
(check-expect (gera-canhao C3)
              (place-image (square (* MARGEM 2) "solid" "black") LARGURA ALTURA
                           (empty-scene LARGURA ALTURA)))
(check-expect (gera-canhao C4)
              (place-image (square (* MARGEM 2) "solid" "black") LARGURA 0
                           (empty-scene LARGURA ALTURA)))


;; gera-quadrados: ListaDeQuadrados Canhao -> Cena
;; objetivo: Desenha Quadrados na tela.
;; exemplos:
;; (gera-quadrados LQ1 C1) = (place-image (desenha-quadrado (first LQ1))
;;                                        (quadrado-x (first LQ1))
;;                                        (quadrado-y (first LQ1))
;;                                        (gera-quadrados (rest LQ1) C1))
;; (gera-quadrados LQ2 C1) = (gera-canhao C1)
(define (gera-quadrados ldq c)
  (cond
    [(empty? ldq) (gera-canhao c)]
    [else (place-image (desenha-quadrado (first ldq)) (quadrado-x (first ldq))
                       (quadrado-y (first ldq)) (gera-quadrados (rest ldq) c))]))

(check-expect (gera-quadrados LQ1 C1)
              (place-image (desenha-quadrado (first LQ1))
                           (quadrado-x (first LQ1))
                           (quadrado-y (first LQ1))
                           (gera-quadrados (rest LQ1) C1)))
(check-expect (gera-quadrados LQ2 C1) (gera-canhao C1))


;; gera-cena: ListaDeBolhas ListaDeQuadrados Canhao -> Cena
;; objetivo: Desenha Bolhas na tela.
;; exemplos:
;; (gera-cena LB2 LQ1 C1) = (gera-quadrados LQ1 C1)
;; (gera-cena LB1 LQ1 C1) = (place-image
;;                           (desenha-bolha (first LB1))(bolha-x (first LB1))
;;                           (bolha-y (first LB1)) (gera-cena (rest LB1) LQ1 C1))
;; (gera-cena LB3 LQ1 C1) = (gera-cena (rest LB3) LQ1 C1)
(define (gera-cena ldb ldq c)
  (cond
    [(empty? ldb) (gera-quadrados ldq c)]
    [(bolha-inteira (first ldb)) (place-image
                                  (desenha-bolha (first ldb))(bolha-x (first ldb))
                                  (bolha-y (first ldb)) (gera-cena (rest ldb) ldq c))]
    [else (gera-cena (rest ldb) ldq c)]))

(check-expect (gera-cena LB2 LQ1 C1) (gera-quadrados LQ1 C1))
(check-expect (gera-cena LB1 LQ1 C1) (place-image
                                      (desenha-bolha (first LB1))(bolha-x (first LB1))
                                      (bolha-y (first LB1)) (gera-cena (rest LB1) LQ1 C1)))
(check-expect (gera-cena LB3 LQ1 C1) (gera-cena (rest LB3) LQ1 C1))


;; bolhas-estouradas?: ListaDeBolhas -> Boolean
;; objetivo: verifica se todas as bolhas foram estouradas, retorna true se não há bolhas inteiras,
;;     false se ainda há alguma inteira.
;; exemplos:
;; (bolhas-estouradas? LB1) = false
;; (bolhas-estouradas? LB3) = true
(define (bolhas-estouradas? ldb)
  (cond
    [(empty? ldb) true]
    [(bolha-inteira (first ldb)) false]
    [else (bolhas-estouradas? (rest ldb))]))

(check-expect (bolhas-estouradas? LB1) false)
(check-expect (bolhas-estouradas? LB3) true)


;; produz-filme: ListaDeBolhas ListaDeQuadrados Canhao -> ListaDeCenas
;; objetivo: Move as bolhas de sua posicao inicial até que ou saiam, ou colidam com um obstáculo, e
;;     explodindo.
;; exemplos:
;; (produz-filme LB1 LQ1 C1) = (cons (gera-cena LB1 LQ1 C1)
;;                                   (produz-filme (atualiza-bolhas LB1 LQ1) LQ1 C1))
;; (produz-filme LB2 LQ1 C2) = empty
;; (produz-filme LB3 LQ1 C3) = empty
(define (produz-filme ldb ldq c)
  (cond
    [(empty? ldb) empty]
    [(bolhas-estouradas? ldb) empty]
    [else (cons (gera-cena ldb ldq c) (produz-filme (atualiza-bolhas ldb ldq) ldq c))]))

(check-expect (produz-filme LB1 LQ1 C1) (cons (gera-cena LB1 LQ1 C1)
                                              (produz-filme (atualiza-bolhas LB1 LQ1) LQ1 C1)))
(check-expect (produz-filme LB2 LQ1 C2) empty)
(check-expect (produz-filme LB3 LQ1 C3) empty)


;; inicia-bolhas: ListaDeBolhas ListaDeQuadrados Canhao -> ListaDeBolhas
;; objetivo: Inicializa as bolhas conforme a posicao do canhao.
;; exemplos:
;; (inicia-bolhas LB5 C1) = (cons (make-bolha 0 0 1 1 "blue" 10 true true) empty)
;; (inicia-bolhas LB5 C2) = (cons (make-bolha ALTURA 0 1 -1 "blue" 10 true true) empty)
;; (inicia-bolhas LB5 C3) = (cons (make-bolha ALTURA LARGURA -1 -1 "blue" 10 true true) empty)
;; (inicia-bolhas LB5 C4) = (cons (make-bolha 0 LARGURA -1 1 "blue" 10 true true) empty)
(define (inicia-bolhas ldb c)
  (cond
    [(empty? ldb) empty]
    [(symbol=? (canhao-posicao c) 'SE)
     (cons (make-bolha 0 0
                       (bolha-ix (first ldb))
                       (bolha-iy (first ldb))
                       (bolha-cor (first ldb))
                       (bolha-raio (first ldb))
                       (bolha-dura (first ldb))
                       (bolha-inteira (first ldb)))
           (inicia-bolhas (rest ldb) c))]
    [(symbol=? (canhao-posicao c) 'SD)
     (cons (make-bolha 0 LARGURA
                    (- (bolha-ix (first ldb)))
                       (bolha-iy (first ldb))
                       (bolha-cor (first ldb))
                       (bolha-raio (first ldb))
                       (bolha-dura (first ldb))
                       (bolha-inteira (first ldb)))
           (inicia-bolhas (rest ldb) c))]
    [(symbol=? (canhao-posicao c) 'IE)
     (cons (make-bolha ALTURA 0
                       (bolha-ix (first ldb))
                    (- (bolha-iy (first ldb)))
                       (bolha-cor (first ldb))
                       (bolha-raio (first ldb))
                       (bolha-dura (first ldb))
                       (bolha-inteira (first ldb)))
           (inicia-bolhas (rest ldb) c))]
    [(symbol=? (canhao-posicao c) 'ID)
     (cons (make-bolha ALTURA LARGURA
                    (- (bolha-ix (first ldb)))
                    (- (bolha-iy (first ldb)))
                       (bolha-cor (first ldb))
                       (bolha-raio (first ldb))
                       (bolha-dura (first ldb))
                       (bolha-inteira (first ldb)))
           (inicia-bolhas (rest ldb) c))]
    [else empty]))

(check-expect (inicia-bolhas LB5 C1) (cons (make-bolha 0 0 1 1 "blue" 10 true true) empty))
(check-expect (inicia-bolhas LB5 C2) (cons (make-bolha ALTURA 0 1 -1 "blue" 10 true true) empty))
(check-expect (inicia-bolhas LB5 C3) (cons
                                      (make-bolha ALTURA LARGURA -1 -1 "blue" 10 true true) empty))
(check-expect (inicia-bolhas LB5 C4) (cons (make-bolha 0 LARGURA -1 1 "blue" 10 true true) empty))


;; roda-filme: ListaDeBolhas ListaDeQuadrados Canhao -> Movie
;; exibe a animação
;; exemplos:
;; (roda-filme LB1 LQ1 C1) = (run-movie 0.001 (produz-filme (inicia-bolhas LB1 C1) LQ1 C1))
(define (roda-filme ldb ldq c)
  (run-movie 0.001 (produz-filme (inicia-bolhas ldb c) ldq c)))

(check-expect (roda-filme LB1 LQ1 C1) (run-movie 0.001 (produz-filme (inicia-bolhas LB1 C1) LQ1 C1)))

(roda-filme LB1 LQ1 C1)
