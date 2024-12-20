
;;;; Autores: Rúben Dâmaso & André Castanho
(defun bfs (tabuleiro)
  "Procura em largura (BFS)"
  (let* ((no-inicial (criar-no tabuleiro)); Cria o nó inicial
         (abertos (list no-inicial))      ; Lista de nós não expandidos
         (fechados '())                   ; Lista de nós já expandidos
         (nos-gerados 0)                  ; Total de nós Gerados
         (nos-expandidos 0)               ; Total de nós expandidos
         (tempo-inicial (get-internal-real-time)))  ; Tempo de Execução
    (bfs-recursivo abertos fechados nos-gerados nos-expandidos tempo-inicial)))

(defun bfs-recursivo (abertos fechados nos-gerados nos-expandidos tempo-inicial)
  "Função recursiva para realizar o BFS"
  (if (null abertos)
      nil  ; Se ABERTOS está vazio, retorna nil (não encontrou solução)
      (let* ((no-atual (car abertos))  ; Retira o primeiro nó de ABERTOS
             (estado-atual (no-estado no-atual))
             (fechados-atualizados (cons estado-atual fechados))) ; Adiciona o nó atual a FECHADOS
        
        ;; Expande o nó atual gerando os sucessores
        (let* ((sucessores (sucessores no-atual (operadores) 'bfs))
               (novos-sucessores (remove-if (lambda (sucessor)
                                              (member (no-estado sucessor) fechados-atualizados))
                                            sucessores))  ; Remove sucessores que estão em FECHADOS
               (novos-nos-gerados (+ nos-gerados (length sucessores)))
               (novos-nos-expandidos (+ nos-expandidos 1)))

          ;;Verifica se algum dos sucessores é o objetivo
          (let ((sucessor-objetivo (find-if (lambda (sucessor)
                                             (tabuleiro-vaziop (no-estado sucessor)))
                                           novos-sucessores)))
            (if sucessor-objetivo
                (let ((tempo-final (get-internal-real-time)))  
                  (let ((tempo-execucao (/ (- tempo-final tempo-inicial) 1000.0)))
                    (list sucessor-objetivo novos-nos-gerados novos-nos-expandidos tempo-execucao)))  ; Retorna a soluçãO
                ;; Se não encontrou solução, coloca os sucessores em ABERTOS e continua a recursão
                (bfs-recursivo
                 (append (cdr abertos) novos-sucessores)  
                 fechados-atualizados  
                 novos-nos-gerados
                 novos-nos-expandidos
                 tempo-inicial)))))))




(defun dfs (tabuleiro profundidade-max)
  "Busca em profundidade (DFS)"
  (let* ((no-inicial (criar-no tabuleiro))   ; Cria o nó inicial
         (abertos (list no-inicial))         ; Lista de nós não expandidos
         (fechados (list))                   ; Lista de nós expandidoss
         (nos-gerados 0)                     ; Total de nós gerados
         (nos-expandidos 0)                  ; Total de nós expandidos
         (tempo-inicial (get-internal-real-time)))  
    (dfs-recursivo abertos fechados profundidade-max nos-gerados nos-expandidos tempo-inicial)))   


(defun dfs-recursivo (abertos fechados profundidade-max nos-gerados nos-expandidos tempo-inicial)
  "Função recursiva que realiza a procura em profundidade (DFS)"
  (if (null abertos) 
      nil  ;Se ABERTOS está vazio, retorna nil (não encontrou solução)
      (let* ((no-atual (car abertos))             ; Retira o primeiro nó de ABERTOS
             (estado-atual (no-estado no-atual))) ; Obtém o estado atual do nó 
            ;; Verifica se atingimos a profundidade máxima definida
            (if (>= (no-profundidade no-atual) profundidade-max)
                (dfs-recursivo (cdr abertos) fechados profundidade-max nos-gerados nos-expandidos tempo-inicial) ; Não expande o nó
                ;; Caso contrário, expande o nó atual
                (let* ((sucessores (sucessores no-atual (operadores) 'dfs profundidade-max))
                       (novos-sucessores 
                         (remove nil 
                                 (mapcar (lambda (sucessor)
                                           (if (and sucessor
                                                    (not (no-existep sucessor fechados))  ; Se o sucessor não está em fechados
                                                    (not (some (lambda (n) 
                                                                 (equal (no-estado n) (no-estado sucessor))) 
                                                               abertos)))  ; Se o sucessor não está em abertos
                                               sucessor))
                                         sucessores))))
                  (let* ((abertos-atualizados (append novos-sucessores (cdr abertos)))  ; Atualiza a lista de ABERTOS
                         (novos-nos-gerados (+ nos-gerados (length sucessores)))  
                         (novos-nos-expandidos (+ nos-expandidos 1)))   
                    ;; Verifica se algum sucessor é o objetivo
                    (if (some (lambda (sucessor) (tabuleiro-vaziop (no-estado sucessor))) novos-sucessores)
                        (let* ((solucao (car (remove-if-not (lambda (sucessor) 
                                                             (tabuleiro-vaziop (no-estado sucessor))) 
                                                           novos-sucessores)))) 
                          (let* ((tempo-final (get-internal-real-time)))  
              (let ((tempo-execucao (/ (- tempo-final tempo-inicial) 1000.0)))
                (list solucao nos-gerados nos-expandidos tempo-execucao))))  ; Retorna o nó solução
                        (dfs-recursivo abertos-atualizados 
                                       (cons estado-atual fechados) 
                                       profundidade-max 
                                       novos-nos-gerados 
                                       novos-nos-expandidos tempo-inicial))))))))


(defun a* (tabuleiro heuristica)
  "Procura A* com contagem de nós gerados e expandidos."
  (let* ((no-inicial (criar-no tabuleiro))   ; Cria o nó inicial
         (abertos (list no-inicial))         ; Lista de nós não expandidos
         (fechados (list))                   ; Lista de nós expandidos
         (nPecasInicial (total-pecas tabuleiro))
         (nos-gerados 0)                     ; Total de nós gerados
         (nos-expandidos 0)                  ; Total de nós expandidos
         (tempo-inicial (get-internal-real-time)); Tempo de Inicio do Algoritmo
         )                 
    (a*-recursivo abertos fechados nPecasInicial nos-gerados nos-expandidos tempo-inicial heuristica)))  


(defun a*-recursivo (abertos fechados nPecasInicial nos-gerados nos-expandidos tempo-inicial heuristica)
  "Função recursiva que realiza a procura A*."
  (if (null abertos)
      nil   ; Se ABERTOS está vazio, retorna nil (não encontrou solução)
      (let* ((no-atual (car abertos))         ; Retira o primeiro nó de ABERTOS
             (estado-atual (no-estado no-atual)))
        ;; Verifica se o nó atual é a solução
        (if (tabuleiro-vaziop estado-atual)
            (let ((tempo-final (get-internal-real-time)))
              (let ((tempo-execucao (/ (- tempo-final tempo-inicial) 1000.0)))
                (list no-atual nos-gerados nos-expandidos tempo-execucao)))  ; Retorna a solução e os contadores
            ;; Caso contrário, expande o nó atual
            (let* ((sucessores (sucessores no-atual (operadores) 'a* nil nPecasInicial heuristica))
                   (nos-gerados-atual (+ nos-gerados (length sucessores)))  
                   (nos-expandidos-atual (+ nos-expandidos 1))  
                   ;; Filtra sucessores para evitar nós repetidos
                   (abertos-atualizados (mapcar (lambda (sucessor)
                                                  (let ((existe-em-abertos (find sucessor abertos :test 'equal))
                                                        (existe-em-fechados (find sucessor fechados :test 'equal)))
                                                    (cond
                                                     ;; Sucessor está em ABERTOS
                                                     ((and existe-em-abertos
                                                           (< (calcular-f sucessor) (calcular-f existe-em-abertos)))
                                                      (atualizar-no existe-em-abertos sucessor))
                                                     ;; Sucessor está em FECHADOS e seu f foi melhorado
                                                     ((and existe-em-fechados
                                                           (< (calcular-f sucessor) (calcular-f existe-em-fechados)))
                                                      (move-para-abertos existe-em-fechados sucessor))
                                                     ;; Sucessor não está em ABERTOS nem em FECHADOS
                                                     (t sucessor))))
                                                sucessores)))
              ;; Continua com os sucessores colocados em ABERTOS
              (a*-recursivo (colocar-sucessores-em-abertos (cdr abertos) abertos-atualizados)
                            (cons estado-atual fechados)  ; Atualiza FECHADOS
                            nPecasInicial
                            nos-gerados-atual
                            nos-expandidos-atual
                            tempo-inicial heuristica))))))
