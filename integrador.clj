(ns integrador.core
  (:require [clojure.string :as str]))

(def alunos (atom []))


(defn ler-entrada [prompt]
  (print prompt)
  (flush)
  (str/trim (read-line)))

(defn exibir-menu []
  (println "\n=== MENU PRINCIPAL ===")
  (println "1 - Cadastrar Alunos")
  (println "2 - Relatório de Notas")
  (println "3 - Estatísticas Gerais")
  (println "4 - Buscar aluno pelo nome (Desafio Extra)")
  (println "0 - Sair")
  (ler-entrada "Escolha uma opção: "))


(defn calcular-media [notas]
  (if (empty? notas)
    0.0
    (/ (apply + notas) (count notas))))

(defn adicionar-status [aluno]
  (let [nota (:nota aluno)
        status (if (>= nota 7.0) "Aprovado" "Reprovado")]
    (assoc aluno :status status)))

(defn alunos-com-status [lista-alunos]
  (map adicionar-status lista-alunos))


(defn cadastrar-alunos []
  (loop [alunos-temp []]
    (let [nome (ler-entrada "Nome do aluno (deixe em branco para sair): ")
          nome-upper (str/upper-case nome)]
      (if (empty? nome-upper)
        (do
          (swap! alunos concat alunos-temp)
          (println (count alunos-temp) "aluno(s) cadastrado(s)."))
        (let [nota-str (ler-entrada "Nota do aluno: ")
              nota (try (Double/parseDouble nota-str)
                        (catch NumberFormatException _ nil))]
          (if (and nota (>= nota 0) (<= nota 10))
            (recur (conj alunos-temp {:nome nome :nota nota}))
            (do
              (println "Nota inválida. Tente novamente.")
              (recur alunos-temp))))))))

(defn relatorio-de-notas []
  (let [lista-alunos @alunos
        alunos-processados (alunos-com-status lista-alunos)
        aprovados (filter #(= "Aprovado" (:status %)) alunos-processados)
        notas (map :nota lista-alunos)
        media (calcular-media notas)]
    (if (empty? lista-alunos)
      (println "Nenhum aluno cadastrado.")
      (do
        (println "\n--- Lista de Alunos Cadastrados ---")
        (doseq [aluno alunos-processados]
          (println (format "Nome: %s, Nota: %.1f, Status: %s"
                           (:nome aluno) (:nota aluno) (:status aluno))))

        (println "\n--- Alunos Aprovados ---")
        (if (empty? aprovados)
          (println "Nenhum aluno aprovado.")
          (doseq [aluno aprovados]
            (println (format "Nome: %s, Nota: %.1f" (:nome aluno) (:nota aluno)))))

        (println (format "\nMédia Geral da Turma: %.2f" media))))))

(defn estatisticas-gerais []
  (let [lista-alunos @alunos
        total-alunos (count lista-alunos)]
    (if (zero? total-alunos)
      (println "Nenhum aluno cadastrado para estatísticas.")
      (let [alunos-processados (alunos-com-status lista-alunos)
            aprovados (filter #(= "Aprovado" (:status %)) alunos-processados)
            reprovados (filter #(= "Reprovado" (:status %)) alunos-processados)
            num-aprovados (count aprovados)
            num-reprovados (count reprovados)
            notas (map :nota lista-alunos)
            maior-nota (apply max notas)
            menor-nota (apply min notas)
            media (calcular-media notas)]

        (println "\n--- Estatísticas Gerais ---")
        (println "Total de Alunos Cadastrados:" total-alunos)
        (println "Número de Aprovados:" num-aprovados)
        (println "Número de Reprovados:" num-reprovados)
        (println (format "Maior Nota: %.1f" maior-nota))
        (println (format "Menor Nota: %.1f" menor-nota))
        (println (format "Média Geral da Turma: %.2f" media))))))

(defn buscar-aluno []
  (let [nome-busca (ler-entrada "Digite o nome do aluno para buscar: ")
        nome-busca-upper (str/upper-case nome-busca)
        lista-alunos @alunos
        alunos-processados (alunos-com-status lista-alunos)
        aluno-encontrado (filter #(= nome-busca-upper (str/upper-case (:nome %))) alunos-processados)]
    (if (empty? aluno-encontrado)
      (println "Aluno não encontrado.")
      (let [aluno (first aluno-encontrado)]
        (println "\n--- Aluno Encontrado ---")
        (println (format "Nome: %s" (:nome aluno)))
        (println (format "Nota: %.1f" (:nota aluno)))
        (println (format "Status: %s" (:status aluno)))))))


(defn -main []
  (loop []
    (let [opcao (exibir-menu)]
      (cond
        (= "1" opcao) (cadastrar-alunos)
        (= "2" opcao) (relatorio-de-notas)
        (= "3" opcao) (estatisticas-gerais)
        (= "4" opcao) (buscar-aluno)
        (= "0" opcao) (println "Saindo do sistema. Até mais!")
        :else (println "Opção inválida. Tente novamente."))
      (when (not= "0" opcao)
        (recur)))))

(comment
  (reset! alunos [{:nome "Ana" :nota 8.5}
                  {:nome "Bruno" :nota 6.0}
                  {:nome "Carlos" :nota 9.2}
                  {:nome "Daniela" :nota 5.5}])
  (relatorio-de-notas)
  (estatisticas-gerais)
  (buscar-aluno)
  )
