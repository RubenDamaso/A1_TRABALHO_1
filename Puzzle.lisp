
;;;; Autores: R�ben D�maso & Andr� Castanho


;;; Tabuleiros

(defun tabuleiro-vazio (&optional (linhas 2) (colunas 6))
  "Retorna um tabuleiro 2x6 (default) com as casas vazias"
  (make-list linhas :initial-element (make-list colunas :initial-element '0))
)


(defun tabuleiro-teste ()
  "Retorna um tabuleiro de teste 2x6 que corresponde ao tabuleiro de teste do enunciado do projeto"
  '((1 2 3 4 5 6)
    (6 5 4 3 2 1))
)
(defun tabuleiroTesteQuaseCompleto ()
  "Retorna um tabuleiro a uma jogada de estar completo"
  '((0 0 0 0 0 0)
    (0 0 0 1 2 0))
)


(defun tabuleiro-a ()
  "Retorna um tabuleiro de teste 2x6 que corresponde ao tabuleiro a) do enunciado do projeto"
  '((0 0 0 0 0 2)
    (0 0 0 0 4 0))
)
 
(defun tabuleiro-b ()
  "Retorna um tabuleiro de teste 2x6 que corresponde ao tabuleiro b) do enunciado do projeto"
  '((2 2 2 2 2 2)
    (2 2 2 2 2 2))
)
(defun tabuleiro-c ()
  "Retorna um tabuleiro de teste 2x6 que corresponde ao tabuleiro c) do enunciado do projeto"
  '((0 3 0 3 0 3)
    (3 0 3 0 3 0))
)
(defun tabuleiro-d ()
  "Retorna um tabuleiro de teste 2x6 que corresponde ao tabuleiro d) do enunciado do projeto"
  '((1 2 3 4 5 6)
    (6 5 4 3 2 1))
)
(defun tabuleiro-e ()
  "Retorna um tabuleiro de teste 2x6 que corresponde ao tabuleiro e) do enunciado do projeto"
  '((2 4 6 8 10 12)
    (12 10 8 6 4 2))
)
(defun tabuleiro-f ()
  "Retorna um tabuleiro de teste 2x6 que corresponde ao tabuleiro f) do enunciado do projeto"
  '((48 0 0 0 0 0)
    (0 0 0 0 0 48))
)
(defun tabuleiro-g ()
  "Retorna um tabuleiro de teste 2x6 que corresponde ao tabuleiro g) do enunciado do projeto"
  '((8 8 8 8 8 8)
    (8 8 8 8 8 8))
)




(defun linha (linha tabuleiro)
  "Retorna a linha especificada (0 ou 1) de um tabuleiro"
  (cond ((or (< linha 0) (> linha 1)) (error "So pode retornar a primeira ou segunda linha"))
        (t (nth linha tabuleiro)))
)


(defun celula(linha coluna tabuleiro)
  "Retorna o valor da calula localizada na linha e coluna especificadas do tabuleiro."
  (if (or (< linha 0) (>= linha (length tabuleiro))
          (< coluna 0) (>= coluna (length (nth linha tabuleiro))))
      (error "indices fora dos limites do tabuleiro.")
      (nth coluna (nth linha tabuleiro)))
)


(defun tabuleiro-vaziop (tabuleiro)
  "Retorna T se o tabuleiro estiver vazio (apenas zeros em todas as casas), caso contrario, NIL."
  (let* ((primeiraLinha (every #'(lambda (x) (= x 0)) (first tabuleiro)))
         (segundaLinha (every #'(lambda (x) (= x 0)) (second tabuleiro))))
    (and primeiraLinha segundaLinha))
)


(defun substituir-posicao (indice lista &optional (valor 0))
  "Substitui o elemento na posicao `indice` da lista pelo `valor` fornecido."
  (cond
    ((null lista) nil)
    ((= indice 0) (cons valor (cdr lista)))
    (t (cons (car lista) (substituir-posicao (- indice 1) (cdr lista) valor))))
)


(defun substituir (posLinha posColuna tabuleiro &optional (valor 0))
  "Substitui o valor na coordenada (posLinha, posColuna) do tabuleiro pelo valor fornecido."
  (cond
    ((zerop posLinha) (cons (substituir-posicao posColuna (car tabuleiro) valor) (cdr tabuleiro)))
    (t (cons (car tabuleiro) (substituir (1- posLinha) posColuna (cdr tabuleiro) valor))))
)                        


(defun incrementar-posicao (posLinha posColuna tabuleiro)
"Incrementa o n�mero de pe�as de uma determinada coordenada (posLinha, posColuna) do tabuleiro "
  (substituir posLinha posColuna tabuleiro (+ (celula posLinha posColuna tabuleiro) 1))
)


;;; Operadores
(defun distribuir-pecas (npecas linha coluna &optional (tabuleiro (tabuleiro-vazio)))
  (if (zerop npecas)
      nil
      (let* ((total-colunas (length (first tabuleiro)))  ; N�mero de colunas
             (total-linhas (length tabuleiro))           ; N�mero de linhas

             ;; Quando a linha � 1
             (nova-coluna (if (= linha 1)
                              (if (< coluna 5)
                                  (1+ coluna)  ; Incrementa a coluna
                                  5)           ; Caso a coluna atinja 5, ela fica 5
                              (if (> coluna 0)
                                  (1- coluna)  ; Caso contr�rio, a coluna � decrementada
                                  0)))        

             ;; Mudan�a de linha: se a linha for 1 e a coluna for 5, a linha � decrementada
             (nova-linha (if (= linha 1)  ; Se a linha for 1
                             (if (= coluna 5)
                                 0  ; A linha � decrementada para 0
                                 linha)  ; Se a coluna n�o for 5, a linha permanece igual
                             (if (= linha 0)  ; Se a linha for 0
                                 (if (= coluna 0)
                                     1  ; Quando a coluna atingir 0, a linha vai para 1
                                     linha)  ; Caso contr�rio, a linha permanece igual
                                 linha))))  ; Caso a linha n�o seja nem 1 nem 0, permanece igual

     
        (if (and (>= nova-linha 0) (< nova-linha total-linhas)
                 (>= nova-coluna 0) (< nova-coluna total-colunas))
            (cons (list nova-linha nova-coluna)
                  (distribuir-pecas
                   (1- npecas)
                   nova-linha
                   nova-coluna
                   (incrementar-posicao nova-linha nova-coluna tabuleiro)))
            (error "�ndices fora dos limites do tabuleiro")))))



(defun operador (linha coluna tabuleiro)
  "Realiza a opera��o de remo��o e redistribui��o de pe�as em um tabuleiro."
  (let* ((pecas (celula linha coluna tabuleiro)))  ; Obt�m as pe�as da posi��o inicial
    (if (zerop pecas)  ; Verifica se n�o h� pe�as para distribuir
        tabuleiro  ; Se n�o houver pe�as, retorna o tabuleiro inalterado
        (let* ((tabuleiro-atualizado (substituir linha coluna tabuleiro 0))  ; Remove as pe�as iniciais
               (posicoes (distribuir-pecas pecas linha coluna tabuleiro-atualizado))  ; Calcula as posi��es para distribuir
               ;; Incrementa as pe�as nas posi��es calculadas
               (tabuleiro-incrementado
                (reduce (lambda (tab pos)
                          (let* ((l (first pos))
                                 (c (second pos)))
                            (incrementar-posicao l c tab)))
                        posicoes
                        :initial-value tabuleiro-atualizado))
               ;; Ap�s incrementar, obt�m a �ltima posi��o
               (ultima-posicao (car (last posicoes)))
               (ultima-linha (first ultima-posicao))
               (ultima-coluna (second ultima-posicao))
               (pecas-na-ultima (celula ultima-linha ultima-coluna tabuleiro-incrementado)))
          ;; Verifica se a �ltima posi��o tem 1, 3 ou 5 e, se sim, remove as pe�as dessa essa c�lula
          (if (member pecas-na-ultima '(1 3 5))   
              (substituir ultima-linha ultima-coluna tabuleiro-incrementado 0)   
              tabuleiro-incrementado)))))  ; Caso contr�rio, retorna o tabuleiro incrementado

 
(defun no-teste ()
  "Cria um n� de teste com o tabuleiro inicial."
  (criar-no (tabuleiro-teste)))

;;Criar um n�
(defun criar-no (tabuleiro &optional (g 0) (pai nil) (h 0))
  (list tabuleiro g pai h)
)

;;Estado do n�
(defun no-estado (no)
  (car no))

;;Profundidade do n�
(defun no-profundidade (no)
  (cadr no))

;;Pai do n�
(defun no-pai (no)
  (caddr no))

;;Heuristica do n�
(defun no-heuristica (no)
  (last no))

(defun caminho-solucao (no)
  (if (null (no-pai no))
      (list (no-estado no))
      (cons (no-estado no) (caminho-solucao (no-pai no)))))


(defun calcular-heuristica (no numeroDePecasInicial tipo-heuristica)
  "Calcula a heur�stica com base no tipo especificado."
  (cond
    ;; Heur�stica Base
    ((eq tipo-heuristica 'pecas)   
     (let* ((estado (no-estado no))  
            (pecas-atuais (reduce #'+   
                                  (mapcar #'(lambda (linha)
                                              (reduce #'+ linha))   
                                          estado)))
            (c (- numeroDePecasInicial pecas-atuais))   
            (o pecas-atuais))                       
       (- o c)))  

     ;; Heur�stica para priorizar posi��es com n�mero baixo de  pe�as
     ((eq tipo-heuristica 'min-pecas)
      (reduce #'+
              (apply #'append
                     (mapcar (lambda (linha)
                               (mapcar (lambda (pecas)
                                         (if (and (< pecas 3) (> pecas 0)) pecas 0))   
                                       linha))
                             (no-estado no)))))
  
    ;; Caso o tipo de heur�stica n�o seja reconhecido
    (t
     (error "Tipo de heur�stica desconhecido"))))  ; Caso um tipo inv�lido seja fornecido


(defun novo-sucessor (no linha coluna operador &optional (numero-pecas-inicial nil)(heuristica nil))
  "Cria um novo sucessor aplicando um operador ao estado do n�."
  (let ((novo-estado (funcall operador linha coluna (no-estado no))))
    (if (and novo-estado (listp novo-estado))
        (let ((novo-no (criar-no novo-estado
                                 (+ 1 (no-profundidade no))  ; Incrementa a profundidade
                                 no)))                       ; Define o n� pai
          (if numero-pecas-inicial  ; Se o n�mero de pe�as inicial foi fornecido, calcula a heur�stica
              (let* ((heuristica (calcular-heuristica novo-no numero-pecas-inicial heuristica)))
                (criar-no novo-estado
                          (+ 1 (no-profundidade no))  ; Incrementa a profundidade
                          no                          ; Define o n� pai
                          heuristica))                ; Adiciona a heur�stica
              novo-no))                               ; Retorna o n� sem heur�stica
        nil)))  ; Caso o novo estado n�o seja v�lido


(defun sucessores (no operadores algoritmo &optional profundidade-max numero-pecas-inicial (heuristica nil))
  "Gera os n�s sucessores a partir de um n� atual aplicando operadores dispon�veis."

  (if (and (eql algoritmo 'dfs) profundidade-max
           (>= (no-profundidade no) profundidade-max))
      nil  ;; N�o expande mais, atingiu a profundidade m�xima para DFS
      (remove nil 
              (mapcar (lambda (op)
                        (let ((sucessor (novo-sucessor no (first op) (second op) #'operador numero-pecas-inicial heuristica)))
                          (if (and sucessor
                                   (not (equal (no-estado no) (no-estado sucessor))))  ;; Verifica se o sucessor � diferente do estado atual
                              sucessor)))  ;; Inclui o sucessor se for diferente
                      operadores))))

(defun ordenar-nos (nos)
  "Ordena uma lista de n�s de acordo com o custo (g + h), do menor para o maior."
  (sort nos #'<
        :key (lambda (no) (+ (no-profundidade no) (no-heuristica no)))))



(defun colocar-sucessores-em-abertos (abertos sucessores)
  "Coloca os sucessores na lista de ABERTOS, ordenando por f = g + h."
  (sort (append abertos sucessores)
        #'<
        :key (lambda (no)
               (+ (no-profundidade no)
                  (if (listp (no-heuristica no))
                      (car (no-heuristica no))
                      (no-heuristica no))))))


(defun calcular-f (no)
  "Calcula o valor de f = g + h para um n�."
  (+ (no-profundidade no)
     (if (listp (no-heuristica no))
         (car (no-heuristica no))
         (no-heuristica no))))

(defun atualizar-no (no-antigo no-novo)
  "Atualiza o n� antigo com os valores do n� novo, criando um novo n� atualizado."
  (criar-no (no-estado no-novo)
            (no-profundidade no-novo)
            (no-pai no-antigo)
            (no-heuristica no-novo)))

(defun move-para-abertos (no-antigo no-novo)
  "Move um n� de FECHADOS para ABERTOS, criando um novo n� atualizado."
  (criar-no (no-estado no-novo)
            (no-profundidade no-novo)
            (no-pai no-antigo)
            (no-heuristica no-novo)
            ))


 

(defun operadores ()
 "Cria uma lista com todos os operadores do taabuleiro"
 (list '(0 0)'(0 1)'(0 2)'(0 3)'(0 4)'(0 5)'(1 0)'(1 1)'(1 2)'(1 3)'(1 4)'(1 5))
)
(defun no-existep (no lista-nos)
  "Verifica se um n� j� existe na lista de n�s, considerando o algoritmo especificado."
  (some (lambda (n) (equal (no-estado no) (no-estado n))) lista-nos))

 

(defun metodo-bisseccao (fun a b tolerancia)
  (if (>= (* (funcall fun a)  (funcall fun b)) 0)
      (error "Sinais n�o opostos")
      (metodo-bisseccao-recursivo fun a b (funcall fun a)  (funcall fun b) tolerancia)
      )
)

(defun metodo-bisseccao-recursivo (fun a b fa fb tolerancia)
  (let* ((c(/ (+ a b) 2))
         (fc (funcall fun c)))
    (cond
     ((or(< (- b a) tolerancia) (= fc 0))c)
     ((< (* fa fc)0)
      (metodo-bisseccao-recursivo fun a c fa fc tolerancia))
     (t
      (metodo-bisseccao-recursivo fun c b fc fb tolerancia)))))

(defun calcular-fator-ramificacao (profundidade nos-gerados a b tolerancia)
  "Calcula o fator de ramifica��o m�dio utilizando a bissec��o."
  (metodo-bisseccao (lambda (b) (f b profundidade nos-gerados)) a b tolerancia))

(defun f (ramificacao profundidade total-nos-gerados)
  "Fun��o para calcular o valor da equa��o do fator de ramifica��o."
  (- (somatorio ramificacao profundidade) total-nos-gerados))

(defun somatorio (ramificacao profundidade)
  "Calcula a soma b^n + b^(n-1) + ... + 1 para o valor da ramifica��o e profundidade."
  (if (= profundidade 1)
      ramificacao
      (+ (expt ramificacao profundidade) (somatorio ramificacao (- profundidade 1)))))


(defun total-pecas (tabuleiro)
  "Calcula o total de pe�as no tabuleiro somando os valores de todas as c�lulas."
  (reduce #'+  ; Soma os valores de todos os elementos
          (mapcar #'(lambda (linha)  ; Para cada linha do tabuleiro
                      (reduce #'+ linha))  ; Soma os elementos de cada linha
                  tabuleiro)))


(defun calcular-penetrancia (no)
  "Calcula a penetr�ncia a partir de um n�"
  (let ((comprimento-caminho (no-profundidade(first no)))  ; Profundidade do n�
        (total-nos-gerados (second no)))  ; Total de n�s gerados no n�
    (if (zerop total-nos-gerados)  ; Verifica se o n�mero total de n�s gerados � zero
        (error "O n�mero total de n�s gerados n�o pode ser zero.")
        (/ comprimento-caminho total-nos-gerados))))  ; Calcula a penetr�ncia
  