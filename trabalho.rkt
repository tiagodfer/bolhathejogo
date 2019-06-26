;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname trabalho) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

;;Altura da tela
(define ALTURA 400)
;;Largura da tela
(define LARGURA 400)
;;Margem do canhao em relação a borda
(define MARGEM 50)

(define-struct bolha (x y ix iy cor raio dura inteira))
;;Um elemento Bolha do conjunto Bolha é
;; (make-bolha (x y ix iy cor raio duro)), onde :
;; - x : Number, coordenada X
;; - y : Number, coordenada Y
;; - ix : Number, valor absoluto dos incrementos na coordenada X
;; - iy : Number, valor absoluto dos incrementos na coordenada Y
;; - cor : String, cor de bolha
;; - raio : Number, raio de bolha
;; - dura : Boolean, true se duro, false se mole
;; - inteira : Boolean, verifica se a bolha não estourou
(define B1 (make-bolha 0 0 1 1 "blue" 10 true true))
(define B2 (make-bolha 0 0 2 2 "medium cyan" 12 false true))

(define-struct quadrado (x y c l duro))
;;Um elemento Quadrado do conjunto Quadrado é:
;; (make-quadrado (x y c l)), onde :
;; - x : Number, coordenada X
;; - y : Number, coordenada Y
;; - c : String, cor de quadrado
;; - l : Number, lado de quadrado
;; - duro: Boolean, false se mole, true se duro.
(define Q1 (make-quadrado 100 100 "pink" 10 false))
(define Q2 (make-quadrado 120 290 "black" 10 false))
(define Q3 (make-quadrado 155 65 "red" 10 false))

(define-struct canhao (posicao))
;;Um elemento canhao do conjunto canhao é
;;(make-canhao posicao) onde:
;; - posicao: Símbolo, 'SD para superior direito, 'ID para inferior direito, 'SE para superior esquerdo, 'IE para inferior esquerdo.
(define C1 (make-canhao 'SE))
(define C2 (make-canhao 'IE))

;;Uma ListaDeBolhas é:
;; - ou empty
;; - ou (cons b l) onde:
;; b: Bolha
;; l: ListaDeBolhas
(define LB1 (cons B1 (cons B2 empty)))

;;Uma ListaDeQuadrados é:
;; - ou empty
;; - ou (cons q l) onde:
;; q: Quadrados
;; l: ListaDeQuadrados
(define LQ1 (cons Q1 (cons Q2 (cons Q3 empty))))

;;Uma ListaDeCenas é:
;; - ou empty
;; - ou (cons c l) onde:
;; c é uma Cena
;; l é uma ListaDeCenas

;;bolhas-estouradas: ListaDeBolhas -> boolean
;;Objetivo: Se todas as bolhas estouraram, retorna true, se ainda existe alguma bolha, retorna false.
(define (bolhas-estouradas ldb)
  (cond
    [(empty? ldb) true]
    [(bolha-inteira (first ldb)) false]
    [else (bolhas-estouradas (rest ldb))]
  )
)

;;estoura-bolha: Bolha -> Bolha
;;Objetivo: Estourar a bolha informada.
(define (estoura-bolha b)
  (make-bolha (bolha-x b) (bolha-y b) (bolha-ix b) (bolha-iy b) (bolha-cor b) (bolha-raio b) (bolha-dura b) false)
)

;;move-bolha: Bolha -> Bolha
;;Objetivo: Mover a bolha informada.
(define (move-bolha b)
  (make-bolha (+ (bolha-x b) (bolha-ix b)) (+ (bolha-y b) (bolha-iy b)) (bolha-ix b) (bolha-iy b) (bolha-cor b) (bolha-raio b) (bolha-dura b) (bolha-inteira b))
)

;;inverte-sentido: Bolha -> Bolha
;;Objetivo: Inverter o sentido da bolha informada.
(define (inverte-sentido b)
  (make-bolha (bolha-x b) (bolha-y b) (- (bolha-ix b)) (- (bolha-iy b)) (bolha-cor b) (bolha-raio b) (bolha-dura b) (bolha-inteira b))
)


;;colidindo: Bolha ListaDeQuadrados -> boolean
;;Objetivo: Retorna true se a bolha em questão colide com algum quadrado da lista, caso contrário, retorna false.
(define (colidindo b ldq)
  (cond
    [(empty? ldq) false]
    [(and (> (bolha-x b) (- (quadrado-x (first ldq)) (/ (quadrado-l (first ldq)) 2))) (< (bolha-x b) (+ (quadrado-x (first ldq)) (/ (quadrado-l (first ldq)) 2))) (> (bolha-y b) (- (quadrado-y (first ldq)) (/ (quadrado-l (first ldq)) 2))) (< (bolha-y b) (+ (quadrado-y (first ldq)) (/ (quadrado-l (first ldq)) 2))))
    true]
    [else (colidindo b (rest ldq))]
  )
)

;;QuadradoOuFalse é:
;; - ou Quadrado.
;; - ou Boolean.

;;quem-colide: Bolha ListaDeQuadrados -> QuadradoOuFalse
;;Objetivo: Retorna o quadrado que colide com a bolha. Caso nenhum colida, retorna false
(define (quem-colide b ldq)
  (cond
    [(empty? ldq) false]
    [(and (> (bolha-x b) (- (quadrado-x (first ldq)) (/ (quadrado-l (first ldq)) 2))) (< (bolha-x b) (+ (quadrado-x (first ldq)) (/ (quadrado-l (first ldq)) 2))) (> (bolha-y b) (- (quadrado-y (first ldq)) (/ (quadrado-l (first ldq)) 2))) (< (bolha-y b) (+ (quadrado-y (first ldq)) (/ (quadrado-l (first ldq)) 2))))
    (first ldq)]
    [else (quem-colide b (rest ldq))]
  )
)

;;fora-dos-limites: Bolha -> Boolean
;;Objetivo: Informar se determinada bolha está fora dos limites.
(define (fora-dos-limites b)
  (cond
    [(or (> (bolha-x b) LARGURA) (< (bolha-x  b) 0) (> (bolha-x b) ALTURA) (< (bolha-x b) 0)) true]
    [else false]
  )
)

;;atualiza-bolhas: ListaDeBolhas -> ListaDeBolhas
;;Objetivo: estoura ou move bolhas de acordo com sua posição.
(define (atualiza-bolhas ldb ldq)
  (cond
   [(empty? ldb) empty]
   [(and (colidindo (first ldb) ldq) (bolha-inteira (first ldb)))
    (cond
      [(not (bolha-dura (first ldb))) (cons (estoura-bolha (first ldb)) (atualiza-bolhas (rest ldb) ldq))]
      [(quadrado-duro (quem-colide (first ldb) ldq)) (cons (estoura-bolha (first ldb)) (atualiza-bolhas (rest ldb) ldq))]
      [else (cons (move-bolha (inverte-sentido (first ldb))) (atualiza-bolhas (rest ldb) ldq))]
    )
   ]
   [(and (fora-dos-limites (first ldb)) (bolha-inteira (first ldb))) (cons (estoura-bolha (first ldb)) (atualiza-bolhas (rest ldb) ldq))]
   [else (cons (move-bolha (first ldb)) (atualiza-bolhas (rest ldb) ldq))]
  )
)

;;desenha-bolha: Bolha -> Imagem
;;Objetivo: Fornecer a representação gráfica da bolha informada.
(define (desenha-bolha b)
  (circle (bolha-raio b) "solid" (bolha-cor b))
)

;;desenha-quadrado: Quadrado -> Imagem
;;Objetivo: Fornecer a representação gráfica do quadrado informada.
(define (desenha-quadrado q)
  (square (quadrado-l q) "solid" (quadrado-c q))
)

;;gera-canhao: Canhao -> Cena
;;Objetivo: Desenha o canhão na tela.
(define (gera-canhao c)
  (cond
    [(symbol=? (canhao-posicao c) 'SE) (place-image (square (* MARGEM 2) "solid" "black") 0 0 (empty-scene LARGURA ALTURA))]
    [(symbol=? (canhao-posicao c) 'IE) (place-image (square (* MARGEM 2) "solid" "black") 0 ALTURA (empty-scene LARGURA ALTURA))]
    [(symbol=? (canhao-posicao c) 'SD) (place-image (square (* MARGEM 2) "solid" "black") LARGURA 0 (empty-scene LARGURA ALTURA))]
    [(symbol=? (canhao-posicao c) 'ID) (place-image (square (* MARGEM 2) "solid" "black") LARGURA ALTURA (empty-scene LARGURA ALTURA))]
    [else (empty-scene LARGURA ALTURA)]
  )
)

;;gera-quadrados: ListaDeQuadrados Canhao -> Cena
;;Objetivo: Desenha Quadrados na tela.
(define (gera-quadrados ldq c)
  (cond
    [(empty? ldq) (gera-canhao c)]
    [else (place-image (desenha-quadrado (first ldq)) (quadrado-x (first ldq)) (quadrado-y (first ldq)) (gera-quadrados (rest ldq) c))]
  )
)

;;gera-cena: ListaDeBolhas ListaDeQuadrados Canhao -> Cena
;;Objetivo: Desenha Bolhas na tela.
(define (gera-cena ldb ldq c)
  (cond
    [(empty? ldb) (gera-quadrados ldq c)]
    [(bolha-inteira (first ldb)) (place-image (desenha-bolha (first ldb)) (bolha-x (first ldb)) (bolha-y (first ldb)) (gera-cena (rest ldb) ldq c))]
    [else (gera-cena (rest ldb) ldq c)]
  )
)

;;bolhas-estouradas?: ListaDeBolhas -> boolean
;;Objetivo: verifica se todas as bolhas foram estouradas. True se não existem bolhas inteiras, false se ainda há alguma inteira.
(define (bolhas-estouradas? ldb)
  (cond
    [(empty? ldb) true]
    [(bolha-inteira (first ldb)) false]
    [else (bolhas-estouradas? (rest ldb))]
  )
)

;;produz-filme: ListaDeBolhas ListaDeQuadrados Canhao -> ListaDeCenas
;;Objetivo: Mover as bolhas de sua posicao inicial até que ou saiam, ou colidam com um obstáculo e explodam.
(define (produz-filme ldb ldq c)
  (cond
    [(empty? ldb) empty]
    [(bolhas-estouradas? ldb) empty]
    [else (cons (gera-cena ldb ldq c) (produz-filme (atualiza-bolhas ldb ldq) ldq c))]
  )
)

;;inicia-bolhas: ListaDeBolhas ListaDeQuadrados Canhao -> ListaDeBolhas
;;Objetivo: Inicializar as bolhas conforme a posicao do canhao.
(define (inicia-bolhas ldb c)
  (cond
    [(empty? ldb) empty]
    [(symbol=? (canhao-posicao c) 'SE) (cons (make-bolha 0 0 (bolha-ix (first ldb)) (bolha-iy (first ldb)) (bolha-cor (first ldb)) (bolha-raio (first ldb)) (bolha-dura (first ldb)) (bolha-inteira (first ldb))) (inicia-bolhas (rest ldb) c))]
    [(symbol=? (canhao-posicao c) 'SD) (cons (make-bolha 0 LARGURA (- (bolha-ix (first ldb))) (bolha-iy (first ldb)) (bolha-cor (first ldb)) (bolha-raio (first ldb)) (bolha-dura (first ldb)) (bolha-inteira (first ldb))) (inicia-bolhas (rest ldb) c))]
    [(symbol=? (canhao-posicao c) 'IE) (cons (make-bolha ALTURA 0 (bolha-ix (first ldb)) (- (bolha-iy (first ldb))) (bolha-cor (first ldb)) (bolha-raio (first ldb)) (bolha-dura (first ldb)) (bolha-inteira (first ldb))) (inicia-bolhas (rest ldb) c))]
    [(symbol=? (canhao-posicao c) 'ID) (cons (make-bolha ALTURA LARGURA (- (bolha-ix (first ldb))) (- (bolha-iy (first ldb))) (bolha-cor (first ldb)) (bolha-raio (first ldb)) (bolha-dura (first ldb)) (bolha-inteira (first ldb))) (inicia-bolhas (rest ldb) c))]
    [else empty]
  )
)

;;roda-filme: ListaDeBolhas ListaDeQuadrados Canhao -> Movie
(define (roda-filme ldb ldq c)
  (run-movie 0.001 (produz-filme (inicia-bolhas ldb c) ldq c))
)

(roda-filme LB1 LQ1 C1)