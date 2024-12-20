;;;; Autores: Rúben Dâmaso & André Castanho
(defparameter *path* "C:\\Users\\HP\\Documents\\ProjetoIA")

(load "C:\\Users\\HP\\Documents\\ProjetoIA\\Puzzle.lisp")
(load "C:\\Users\\HP\\Documents\\ProjetoIA\\Procura.lisp")

(defun ler-tabuleiros-do-ficheiro (ficheiro)
  "Lê os tabuleiros no ficheiro e devolve uma lista de tabuleiros."
  (with-open-file (stream ficheiro)
    (let ((tabuleiros '()))
      (loop for linha = (read-line stream nil)
            while linha
            do (push (read-from-string linha) tabuleiros))
      (nreverse tabuleiros))))

(defun selecionar-tabuleiro (numero ficheiro)
  "Seleciona o tabuleiro correspondente ao número inserido pelo utilizador, a partir do ficheiro."
  (let* ((full-path (format nil "~A\\~A" *path* ficheiro))
         (tabuleiros (ler-tabuleiros-do-ficheiro full-path)))
    (if (and (>= numero 1) (<= numero (length tabuleiros)))
        (nth (1- numero) tabuleiros)
        (error "Número de tabuleiro inválido!"))))
(defun iniciar (ficheiro)
  "Interface com o utilizador para escolher um tabuleiro, algoritmo e heurística/profundidade (se aplicável)."
  (loop
    (format t "Escolha um número de tabuleiro (ou 0 para sair): ")
    (let ((numero (read)))
      (if (= numero 0)
          (progn
            (return))  ; Termina quando o número 0 for inserido.
          (handler-case
              (let ((tabuleiro (selecionar-tabuleiro numero ficheiro)))
                (format t "Tabuleiro escolhido: ~a~%" tabuleiro)
                ;; Escolhe o algoritmo
                (format t "Escolha um algoritmo (1 para BFS, 2 para DFS, 3 para A*): ")
                (let ((opcao (read)))
                  (cond
                    ((= opcao 1)  ; BFS
                     (let ((resultado (bfs tabuleiro)))
                       (imprimir-solucao-em-ficheiro resultado)))
                    ((= opcao 2)  ; DFS
                     (format t "Escolha a profundidade máxima para DFS: ")
                     (let ((profundidade-max (read)))
                       (let ((resultado (dfs tabuleiro profundidade-max)))
                         (imprimir-solucao-em-ficheiro resultado))))
                    ((= opcao 3)  ; A*
                     (format t "Escolha uma heurística (1 para a Heuristica Base , 2 para a Heuristica dos Alunos): ")
                     (let ((heuristica (read)))
                       (cond
                         ((= heuristica 1)  ; Heurística Base
                          (let ((resultado (a* tabuleiro 'pecas)))
                            (imprimir-solucao-em-ficheiro resultado)))
                         ((= heuristica 2)  ; Heurística Dos Alunos
                          (let ((resultado (a* tabuleiro 'min-pecas)))
                            (imprimir-solucao-em-ficheiro resultado)))
                         (t (format t "Opção de heurística inválida!")))))
                    (t (format t "Opção inválida!")))))
            (error (e)
                   (format t "Erro: ~a~%" e)))))))

(defun imprimir-solucao-em-ficheiro (resultado)
  "Imprime o caminho da solução e métricas num ficheiro de texto."
  (let* ((solucao (first resultado))               ; Nó solução
         (nos-gerados (second resultado))         ; Número de nós gerados
         (nos-expandidos (third resultado))       ; Número de nós expandidos
         (caminho (reverse (caminho-solucao solucao))) ; Caminho da solução
         (profundidade (- (length caminho) 1))         ; Profundidade a que foi encontrado
         (tempo-execucao (last resultado))        ; Tempo de execução
         (fator-ramificacao
          (if (> profundidade 1)
              (calcular-fator-ramificacao profundidade nos-gerados 0 100 0.1) ; Bisseção
              0)))                               ; Fator de ramificação médio

    (let* ((nome-ficheiro "resultado.txt")
           (caminho-ficheiro (merge-pathnames nome-ficheiro *path*)))

      ;; Abre o ficheiro em modo de escrita
      (with-open-file (stream caminho-ficheiro
                              :direction :output
                              :if-exists :append
                              :if-does-not-exist :create)
        ;; Escreve o caminho da solução
        (format stream "Caminho até a solução:~%")
        (loop for estado in caminho
              for passo from 1
              do (format stream "~%~A)~%" passo)
              (loop for linha in estado
                    do (format stream "  ~A~%" linha)))

        ;; Escreve as métricas
        (format stream "~%Métricas:~%")
        (format stream "Nós gerados: ~A~%" nos-gerados)
        (format stream "Nós expandidos: ~A~%" nos-expandidos)
        (format stream "Profundidade da solução: ~A~%" profundidade)
        (format stream "Penetrância: ~,3f~%" (calcular-penetrancia resultado))
        (format stream "Fator de ramificação: ~,3f~%" fator-ramificacao)
        (format stream "Tempo de Execução: ~,3f ms~%" tempo-execucao))

      ;; Exibe os resultados no terminal
      (format t "Caminho até a solução:~%")
      (loop for estado in caminho
            for passo from 1
            do (format t "~%~A)~%" passo)
               (loop for linha in estado
                     do (format t "  ~A~%" linha)))

      ;; Exibe as métricas no terminal
      (format t "~%Métricas:~%")
      (format t "Nós gerados: ~A~%" nos-gerados)
      (format t "Nós expandidos: ~A~%" nos-expandidos)
      (format t "Profundidade da solução: ~A~%" profundidade)
      (format t "Penetrância: ~,3f~%" (calcular-penetrancia resultado))
      (format t "Fator de ramificação: ~,3f~%" fator-ramificacao)
      (format t "Tempo de Execução: ~,3f ms~%" tempo-execucao)

      ;; Confirmação no terminal
      (format t "Resultados escritos em ~A.~%" caminho-ficheiro))))

