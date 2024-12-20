;;;; Autores: R�ben D�maso & Andr� Castanho
(defparameter *path* "C:\\Users\\HP\\Documents\\ProjetoIA")

(load "C:\\Users\\HP\\Documents\\ProjetoIA\\Puzzle.lisp")
(load "C:\\Users\\HP\\Documents\\ProjetoIA\\Procura.lisp")

(defun ler-tabuleiros-do-ficheiro (ficheiro)
  "L� os tabuleiros no ficheiro e devolve uma lista de tabuleiros."
  (with-open-file (stream ficheiro)
    (let ((tabuleiros '()))
      (loop for linha = (read-line stream nil)
            while linha
            do (push (read-from-string linha) tabuleiros))
      (nreverse tabuleiros))))

(defun selecionar-tabuleiro (numero ficheiro)
  "Seleciona o tabuleiro correspondente ao n�mero inserido pelo utilizador, a partir do ficheiro."
  (let* ((full-path (format nil "~A\\~A" *path* ficheiro))
         (tabuleiros (ler-tabuleiros-do-ficheiro full-path)))
    (if (and (>= numero 1) (<= numero (length tabuleiros)))
        (nth (1- numero) tabuleiros)
        (error "N�mero de tabuleiro inv�lido!"))))
(defun iniciar (ficheiro)
  "Interface com o utilizador para escolher um tabuleiro, algoritmo e heur�stica/profundidade (se aplic�vel)."
  (loop
    (format t "Escolha um n�mero de tabuleiro (ou 0 para sair): ")
    (let ((numero (read)))
      (if (= numero 0)
          (progn
            (return))  ; Termina quando o n�mero 0 for inserido.
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
                     (format t "Escolha a profundidade m�xima para DFS: ")
                     (let ((profundidade-max (read)))
                       (let ((resultado (dfs tabuleiro profundidade-max)))
                         (imprimir-solucao-em-ficheiro resultado))))
                    ((= opcao 3)  ; A*
                     (format t "Escolha uma heur�stica (1 para a Heuristica Base , 2 para a Heuristica dos Alunos): ")
                     (let ((heuristica (read)))
                       (cond
                         ((= heuristica 1)  ; Heur�stica Base
                          (let ((resultado (a* tabuleiro 'pecas)))
                            (imprimir-solucao-em-ficheiro resultado)))
                         ((= heuristica 2)  ; Heur�stica Dos Alunos
                          (let ((resultado (a* tabuleiro 'min-pecas)))
                            (imprimir-solucao-em-ficheiro resultado)))
                         (t (format t "Op��o de heur�stica inv�lida!")))))
                    (t (format t "Op��o inv�lida!")))))
            (error (e)
                   (format t "Erro: ~a~%" e)))))))

(defun imprimir-solucao-em-ficheiro (resultado)
  "Imprime o caminho da solu��o e m�tricas num ficheiro de texto."
  (let* ((solucao (first resultado))               ; N� solu��o
         (nos-gerados (second resultado))         ; N�mero de n�s gerados
         (nos-expandidos (third resultado))       ; N�mero de n�s expandidos
         (caminho (reverse (caminho-solucao solucao))) ; Caminho da solu��o
         (profundidade (- (length caminho) 1))         ; Profundidade a que foi encontrado
         (tempo-execucao (last resultado))        ; Tempo de execu��o
         (fator-ramificacao
          (if (> profundidade 1)
              (calcular-fator-ramificacao profundidade nos-gerados 0 100 0.1) ; Bisse��o
              0)))                               ; Fator de ramifica��o m�dio

    (let* ((nome-ficheiro "resultado.txt")
           (caminho-ficheiro (merge-pathnames nome-ficheiro *path*)))

      ;; Abre o ficheiro em modo de escrita
      (with-open-file (stream caminho-ficheiro
                              :direction :output
                              :if-exists :append
                              :if-does-not-exist :create)
        ;; Escreve o caminho da solu��o
        (format stream "Caminho at� a solu��o:~%")
        (loop for estado in caminho
              for passo from 1
              do (format stream "~%~A)~%" passo)
              (loop for linha in estado
                    do (format stream "  ~A~%" linha)))

        ;; Escreve as m�tricas
        (format stream "~%M�tricas:~%")
        (format stream "N�s gerados: ~A~%" nos-gerados)
        (format stream "N�s expandidos: ~A~%" nos-expandidos)
        (format stream "Profundidade da solu��o: ~A~%" profundidade)
        (format stream "Penetr�ncia: ~,3f~%" (calcular-penetrancia resultado))
        (format stream "Fator de ramifica��o: ~,3f~%" fator-ramificacao)
        (format stream "Tempo de Execu��o: ~,3f ms~%" tempo-execucao))

      ;; Exibe os resultados no terminal
      (format t "Caminho at� a solu��o:~%")
      (loop for estado in caminho
            for passo from 1
            do (format t "~%~A)~%" passo)
               (loop for linha in estado
                     do (format t "  ~A~%" linha)))

      ;; Exibe as m�tricas no terminal
      (format t "~%M�tricas:~%")
      (format t "N�s gerados: ~A~%" nos-gerados)
      (format t "N�s expandidos: ~A~%" nos-expandidos)
      (format t "Profundidade da solu��o: ~A~%" profundidade)
      (format t "Penetr�ncia: ~,3f~%" (calcular-penetrancia resultado))
      (format t "Fator de ramifica��o: ~,3f~%" fator-ramificacao)
      (format t "Tempo de Execu��o: ~,3f ms~%" tempo-execucao)

      ;; Confirma��o no terminal
      (format t "Resultados escritos em ~A.~%" caminho-ficheiro))))

