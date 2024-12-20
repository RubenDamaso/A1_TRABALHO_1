
;;;; Autores: R�ben D�maso & Andr� Castanho
(defun bfs (tabuleiro)
  "Procura em largura (BFS)"
  (let* ((no-inicial (criar-no tabuleiro)); Cria o n� inicial
         (abertos (list no-inicial))      ; Lista de n�s n�o expandidos
         (fechados '())                   ; Lista de n�s j� expandidos
         (nos-gerados 0)                  ; Total de n�s Gerados
         (nos-expandidos 0)               ; Total de n�s expandidos
         (tempo-inicial (get-internal-real-time)))  ; Tempo de Execu��o
    (bfs-recursivo abertos fechados nos-gerados nos-expandidos tempo-inicial)))

(defun bfs-recursivo (abertos fechados nos-gerados nos-expandidos tempo-inicial)
  "Fun��o recursiva para realizar o BFS"
  (if (null abertos)
      nil  ; Se ABERTOS est� vazio, retorna nil (n�o encontrou solu��o)
      (let* ((no-atual (car abertos))  ; Retira o primeiro n� de ABERTOS
             (estado-atual (no-estado no-atual))
             (fechados-atualizados (cons estado-atual fechados))) ; Adiciona o n� atual a FECHADOS
        
        ;; Expande o n� atual gerando os sucessores
        (let* ((sucessores (sucessores no-atual (operadores) 'bfs))
               (novos-sucessores (remove-if (lambda (sucessor)
                                              (member (no-estado sucessor) fechados-atualizados))
                                            sucessores))  ; Remove sucessores que est�o em FECHADOS
               (novos-nos-gerados (+ nos-gerados (length sucessores)))
               (novos-nos-expandidos (+ nos-expandidos 1)))

          ;;Verifica se algum dos sucessores � o objetivo
          (let ((sucessor-objetivo (find-if (lambda (sucessor)
                                             (tabuleiro-vaziop (no-estado sucessor)))
                                           novos-sucessores)))
            (if sucessor-objetivo
                (let ((tempo-final (get-internal-real-time)))  
                  (let ((tempo-execucao (/ (- tempo-final tempo-inicial) 1000.0)))
                    (list sucessor-objetivo novos-nos-gerados novos-nos-expandidos tempo-execucao)))  ; Retorna a solu��O
                ;; Se n�o encontrou solu��o, coloca os sucessores em ABERTOS e continua a recurs�o
                (bfs-recursivo
                 (append (cdr abertos) novos-sucessores)  
                 fechados-atualizados  
                 novos-nos-gerados
                 novos-nos-expandidos
                 tempo-inicial)))))))




(defun dfs (tabuleiro profundidade-max)
  "Busca em profundidade (DFS)"
  (let* ((no-inicial (criar-no tabuleiro))   ; Cria o n� inicial
         (abertos (list no-inicial))         ; Lista de n�s n�o expandidos
         (fechados (list))                   ; Lista de n�s expandidoss
         (nos-gerados 0)                     ; Total de n�s gerados
         (nos-expandidos 0)                  ; Total de n�s expandidos
         (tempo-inicial (get-internal-real-time)))  
    (dfs-recursivo abertos fechados profundidade-max nos-gerados nos-expandidos tempo-inicial)))   


(defun dfs-recursivo (abertos fechados profundidade-max nos-gerados nos-expandidos tempo-inicial)
  "Fun��o recursiva que realiza a procura em profundidade (DFS)"
  (if (null abertos) 
      nil  ;Se ABERTOS est� vazio, retorna nil (n�o encontrou solu��o)
      (let* ((no-atual (car abertos))             ; Retira o primeiro n� de ABERTOS
             (estado-atual (no-estado no-atual))) ; Obt�m o estado atual do n� 
            ;; Verifica se atingimos a profundidade m�xima definida
            (if (>= (no-profundidade no-atual) profundidade-max)
                (dfs-recursivo (cdr abertos) fechados profundidade-max nos-gerados nos-expandidos tempo-inicial) ; N�o expande o n�
                ;; Caso contr�rio, expande o n� atual
                (let* ((sucessores (sucessores no-atual (operadores) 'dfs profundidade-max))
                       (novos-sucessores 
                         (remove nil 
                                 (mapcar (lambda (sucessor)
                                           (if (and sucessor
                                                    (not (no-existep sucessor fechados))  ; Se o sucessor n�o est� em fechados
                                                    (not (some (lambda (n) 
                                                                 (equal (no-estado n) (no-estado sucessor))) 
                                                               abertos)))  ; Se o sucessor n�o est� em abertos
                                               sucessor))
                                         sucessores))))
                  (let* ((abertos-atualizados (append novos-sucessores (cdr abertos)))  ; Atualiza a lista de ABERTOS
                         (novos-nos-gerados (+ nos-gerados (length sucessores)))  
                         (novos-nos-expandidos (+ nos-expandidos 1)))   
                    ;; Verifica se algum sucessor � o objetivo
                    (if (some (lambda (sucessor) (tabuleiro-vaziop (no-estado sucessor))) novos-sucessores)
                        (let* ((solucao (car (remove-if-not (lambda (sucessor) 
                                                             (tabuleiro-vaziop (no-estado sucessor))) 
                                                           novos-sucessores)))) 
                          (let* ((tempo-final (get-internal-real-time)))  
              (let ((tempo-execucao (/ (- tempo-final tempo-inicial) 1000.0)))
                (list solucao nos-gerados nos-expandidos tempo-execucao))))  ; Retorna o n� solu��o
                        (dfs-recursivo abertos-atualizados 
                                       (cons estado-atual fechados) 
                                       profundidade-max 
                                       novos-nos-gerados 
                                       novos-nos-expandidos tempo-inicial))))))))


(defun a* (tabuleiro heuristica)
  "Procura A* com contagem de n�s gerados e expandidos."
  (let* ((no-inicial (criar-no tabuleiro))   ; Cria o n� inicial
         (abertos (list no-inicial))         ; Lista de n�s n�o expandidos
         (fechados (list))                   ; Lista de n�s expandidos
         (nPecasInicial (total-pecas tabuleiro))
         (nos-gerados 0)                     ; Total de n�s gerados
         (nos-expandidos 0)                  ; Total de n�s expandidos
         (tempo-inicial (get-internal-real-time)); Tempo de Inicio do Algoritmo
         )                 
    (a*-recursivo abertos fechados nPecasInicial nos-gerados nos-expandidos tempo-inicial heuristica)))  


(defun a*-recursivo (abertos fechados nPecasInicial nos-gerados nos-expandidos tempo-inicial heuristica)
  "Fun��o recursiva que realiza a procura A*."
  (if (null abertos)
      nil   ; Se ABERTOS est� vazio, retorna nil (n�o encontrou solu��o)
      (let* ((no-atual (car abertos))         ; Retira o primeiro n� de ABERTOS
             (estado-atual (no-estado no-atual)))
        ;; Verifica se o n� atual � a solu��o
        (if (tabuleiro-vaziop estado-atual)
            (let ((tempo-final (get-internal-real-time)))
              (let ((tempo-execucao (/ (- tempo-final tempo-inicial) 1000.0)))
                (list no-atual nos-gerados nos-expandidos tempo-execucao)))  ; Retorna a solu��o e os contadores
            ;; Caso contr�rio, expande o n� atual
            (let* ((sucessores (sucessores no-atual (operadores) 'a* nil nPecasInicial heuristica))
                   (nos-gerados-atual (+ nos-gerados (length sucessores)))  
                   (nos-expandidos-atual (+ nos-expandidos 1))  
                   ;; Filtra sucessores para evitar n�s repetidos
                   (abertos-atualizados (mapcar (lambda (sucessor)
                                                  (let ((existe-em-abertos (find sucessor abertos :test 'equal))
                                                        (existe-em-fechados (find sucessor fechados :test 'equal)))
                                                    (cond
                                                     ;; Sucessor est� em ABERTOS
                                                     ((and existe-em-abertos
                                                           (< (calcular-f sucessor) (calcular-f existe-em-abertos)))
                                                      (atualizar-no existe-em-abertos sucessor))
                                                     ;; Sucessor est� em FECHADOS e seu f foi melhorado
                                                     ((and existe-em-fechados
                                                           (< (calcular-f sucessor) (calcular-f existe-em-fechados)))
                                                      (move-para-abertos existe-em-fechados sucessor))
                                                     ;; Sucessor n�o est� em ABERTOS nem em FECHADOS
                                                     (t sucessor))))
                                                sucessores)))
              ;; Continua com os sucessores colocados em ABERTOS
              (a*-recursivo (colocar-sucessores-em-abertos (cdr abertos) abertos-atualizados)
                            (cons estado-atual fechados)  ; Atualiza FECHADOS
                            nPecasInicial
                            nos-gerados-atual
                            nos-expandidos-atual
                            tempo-inicial heuristica))))))
