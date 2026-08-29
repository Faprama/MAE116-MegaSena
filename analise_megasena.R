# ============================================================
# MAE116 - Mega-Sena
# Probabilidades teóricas e frequências observadas
# ============================================================
#
# Objetivos:
# 1. Ler e validar os resultados oficiais da Mega-Sena.
# 2. Organizar a base de dados utilizada na atividade.
# 3. Calcular probabilidades teóricas de eventos selecionados.
# 4. Calcular frequências observadas nos concursos históricos.
# 5. Comparar modelo probabilístico e dados.
# 6. Analisar propriedades das seis dezenas sorteadas.
# 7. Produzir tabelas e gráficos utilizados no material didático.
#
# ============================================================


# ============================================================
# 1. PACOTES E PARÂMETROS GERAIS
# ============================================================

library(readxl)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)


# ------------------------------------------------------------
# 1.1 Parâmetros da Mega-Sena
# ------------------------------------------------------------

N_DEZENAS <- 60
N_SORTEADAS <- 6

# Probabilidade de uma dezena específica aparecer em um sorteio
P_DEZENA <- N_SORTEADAS / N_DEZENAS

# Número total de combinações possíveis
N_COMBINACOES <- choose(
  N_DEZENAS,
  N_SORTEADAS
)


# ------------------------------------------------------------
# 1.2 Parâmetros computacionais
# ------------------------------------------------------------

# Número de simulações para a faixa simultânea
B <- 2000

# Semente para tornar a simulação reprodutível
set.seed(116)


# ------------------------------------------------------------
# 1.3 Arquivos e diretórios
# ------------------------------------------------------------

ARQUIVO_DADOS <- "Mega-Sena.xlsx"
DIRETORIO_RESULTADOS <- "resultados"

dir.create(
  DIRETORIO_RESULTADOS,
  showWarnings = FALSE,
  recursive = TRUE
)


# ============================================================
# 2. LEITURA E VALIDAÇÃO DA BASE
# ============================================================


# ------------------------------------------------------------
# 2.1 Verificar existência do arquivo
# ------------------------------------------------------------

if (!file.exists(ARQUIVO_DADOS)) {
  stop(
    paste0(
      "O arquivo '",
      ARQUIVO_DADOS,
      "' não foi encontrado no diretório do projeto."
    )
  )
}


# ------------------------------------------------------------
# 2.2 Ler a planilha
# ------------------------------------------------------------

mega <- read_excel(
  ARQUIVO_DADOS
)


# ------------------------------------------------------------
# 2.3 Verificar colunas necessárias
# ------------------------------------------------------------

colunas_necessarias <- c(
  "Concurso",
  "Data do Sorteio",
  "Bola1",
  "Bola2",
  "Bola3",
  "Bola4",
  "Bola5",
  "Bola6"
)

colunas_ausentes <- setdiff(
  colunas_necessarias,
  names(mega)
)

if (length(colunas_ausentes) > 0) {
  stop(
    paste0(
      "A planilha não contém as seguintes colunas necessárias: ",
      paste(colunas_ausentes, collapse = ", "),
      "."
    )
  )
}


# ------------------------------------------------------------
# 2.4 Selecionar variáveis utilizadas
# ------------------------------------------------------------

mega_sorteios <- mega |>
  select(
    Concurso,
    `Data do Sorteio`,
    Bola1,
    Bola2,
    Bola3,
    Bola4,
    Bola5,
    Bola6
  )


# ------------------------------------------------------------
# 2.5 Converter a data
# ------------------------------------------------------------

if (inherits(mega_sorteios$`Data do Sorteio`, "Date")) {
  
  mega_sorteios$`Data do Sorteio` <-
    as.Date(
      mega_sorteios$`Data do Sorteio`
    )
  
} else {
  
  mega_sorteios$`Data do Sorteio` <-
    dmy(
      mega_sorteios$`Data do Sorteio`
    )
}


# ------------------------------------------------------------
# 2.6 Validações gerais
# ------------------------------------------------------------

if (nrow(mega_sorteios) == 0) {
  stop(
    "A planilha não contém concursos."
  )
}


if (anyNA(mega_sorteios$Concurso)) {
  stop(
    "Há valores ausentes na coluna 'Concurso'."
  )
}


if (anyDuplicated(mega_sorteios$Concurso) > 0) {
  stop(
    "Há números de concurso duplicados na base."
  )
}


if (anyNA(mega_sorteios$`Data do Sorteio`)) {
  stop(
    paste0(
      "Há datas ausentes ou que não puderam ser ",
      "convertidas na coluna 'Data do Sorteio'."
    )
  )
}


# ------------------------------------------------------------
# 2.7 Validar as seis dezenas
# ------------------------------------------------------------

matriz_validacao <- as.matrix(
  mega_sorteios |>
    select(
      Bola1,
      Bola2,
      Bola3,
      Bola4,
      Bola5,
      Bola6
    )
)


if (anyNA(matriz_validacao)) {
  stop(
    "Há valores ausentes entre as seis dezenas sorteadas."
  )
}


if (!all(is.finite(matriz_validacao))) {
  stop(
    "Há valores não numéricos ou não finitos entre as dezenas."
  )
}


if (
  any(
    abs(
      matriz_validacao -
      round(matriz_validacao)
    ) >
    sqrt(.Machine$double.eps)
  )
) {
  stop(
    "Todas as dezenas devem ser números inteiros."
  )
}


if (
  any(
    matriz_validacao < 1 |
    matriz_validacao > N_DEZENAS
  )
) {
  stop(
    "Todas as dezenas devem estar entre 1 e 60."
  )
}


linhas_com_repeticao <- apply(
  matriz_validacao,
  1,
  function(x) {
    length(unique(x)) != N_SORTEADAS
  }
)


if (any(linhas_com_repeticao)) {
  stop(
    paste0(
      "Há concursos com dezenas repetidas dentro do mesmo sorteio. ",
      "Verifique a base de dados."
    )
  )
}


# ============================================================
# 3. PREPARAÇÃO DA BASE
# ============================================================

mega_sorteios <- mega_sorteios |>
  mutate(
    
    # Número de dezenas pares em cada concurso
    n_pares =
      (Bola1 %% 2 == 0) +
      (Bola2 %% 2 == 0) +
      (Bola3 %% 2 == 0) +
      (Bola4 %% 2 == 0) +
      (Bola5 %% 2 == 0) +
      (Bola6 %% 2 == 0),
    
    # Menor dezena do concurso
    minimo =
      pmin(
        Bola1,
        Bola2,
        Bola3,
        Bola4,
        Bola5,
        Bola6
      ),
    
    # Maior dezena do concurso
    maximo =
      pmax(
        Bola1,
        Bola2,
        Bola3,
        Bola4,
        Bola5,
        Bola6
      ),
    
    # Média das seis dezenas
    media =
      (
        Bola1 +
          Bola2 +
          Bola3 +
          Bola4 +
          Bola5 +
          Bola6
      ) /
      N_SORTEADAS
  )


# ------------------------------------------------------------
# 3.1 Informações gerais
# ------------------------------------------------------------

n_concursos <-
  nrow(
    mega_sorteios
  )


periodo <-
  range(
    mega_sorteios$`Data do Sorteio`,
    na.rm = TRUE
  )


# ------------------------------------------------------------
# 3.2 Matrizes auxiliares
# ------------------------------------------------------------

matriz_dezenas <- as.matrix(
  mega_sorteios |>
    select(
      Bola1,
      Bola2,
      Bola3,
      Bola4,
      Bola5,
      Bola6
    )
)


matriz_ordenada <- t(
  apply(
    matriz_dezenas,
    1,
    sort
  )
)


# ============================================================
# 4. FUNÇÕES AUXILIARES
# ============================================================


# ------------------------------------------------------------
# 4.1 Teste de valor inteiro
# ------------------------------------------------------------

eh_inteiro <- function(x) {
  
  is.numeric(x) &&
    length(x) == 1 &&
    !is.na(x) &&
    is.finite(x) &&
    abs(
      x - round(x)
    ) <
    sqrt(.Machine$double.eps)
}


# ------------------------------------------------------------
# 4.2 Validação de uma dezena
# ------------------------------------------------------------

validar_dezena <- function(dezena) {
  
  if (
    !eh_inteiro(dezena) ||
    dezena < 1 ||
    dezena > N_DEZENAS
  ) {
    stop(
      "A dezena deve ser um número inteiro entre 1 e 60."
    )
  }
}


# ------------------------------------------------------------
# 4.3 Número e frequência de uma dezena específica
# ------------------------------------------------------------

n_dezena <- function(dezena) {
  
  validar_dezena(dezena)
  
  sum(
    matriz_dezenas == dezena
  )
}


freq_dezena <- function(dezena) {
  
  n_dezena(dezena) /
    n_concursos
}


# ------------------------------------------------------------
# 4.4 Probabilidade de uma dezena específica
# ------------------------------------------------------------

prob_dezena <- function(dezena) {
  
  validar_dezena(dezena)
  
  P_DEZENA
}


# ------------------------------------------------------------
# 4.5 Validação do número de dezenas pares
# ------------------------------------------------------------

validar_k_pares <- function(k) {
  
  if (
    !eh_inteiro(k) ||
    k < 0 ||
    k > N_SORTEADAS
  ) {
    stop(
      "k deve ser um número inteiro entre 0 e 6."
    )
  }
}


# ------------------------------------------------------------
# 4.6 Frequência e probabilidade de k dezenas pares
# ------------------------------------------------------------

freq_n_pares <- function(k) {
  
  validar_k_pares(k)
  
  mean(
    mega_sorteios$n_pares == k
  )
}


prob_n_pares <- function(k) {
  
  validar_k_pares(k)
  
  choose(
    30,
    k
  ) *
    choose(
      30,
      N_SORTEADAS - k
    ) /
    N_COMBINACOES
}


# ------------------------------------------------------------
# 4.7 Validação de uma combinação específica
# ------------------------------------------------------------

validar_combinacao <- function(dezenas) {
  
  if (
    !is.numeric(dezenas) ||
    length(dezenas) != N_SORTEADAS
  ) {
    stop(
      "A combinação deve conter exatamente seis dezenas numéricas."
    )
  }
  
  
  if (
    any(is.na(dezenas)) ||
    any(!is.finite(dezenas))
  ) {
    stop(
      "A combinação contém valores ausentes ou não finitos."
    )
  }
  
  
  if (
    any(
      abs(
        dezenas -
        round(dezenas)
      ) >
      sqrt(.Machine$double.eps)
    )
  ) {
    stop(
      "As dezenas da combinação devem ser números inteiros."
    )
  }
  
  
  if (
    any(dezenas < 1) ||
    any(dezenas > N_DEZENAS)
  ) {
    stop(
      "As dezenas devem estar entre 1 e 60."
    )
  }
  
  
  if (
    length(
      unique(dezenas)
    ) !=
    N_SORTEADAS
  ) {
    stop(
      "As seis dezenas devem ser distintas."
    )
  }
}


# ------------------------------------------------------------
# 4.8 Número, frequência e probabilidade de uma combinação
# ------------------------------------------------------------

n_combinacao <- function(dezenas) {
  
  validar_combinacao(dezenas)
  
  alvo <- sort(
    dezenas
  )
  
  alvo_matriz <- matrix(
    alvo,
    nrow = n_concursos,
    ncol = N_SORTEADAS,
    byrow = TRUE
  )
  
  sum(
    rowSums(
      matriz_ordenada ==
        alvo_matriz
    ) ==
      N_SORTEADAS
  )
}


freq_combinacao <- function(dezenas) {
  
  n_combinacao(dezenas) /
    n_concursos
}


prob_combinacao <- function(dezenas) {
  
  validar_combinacao(dezenas)
  
  1 /
    N_COMBINACOES
}


# ------------------------------------------------------------
# 4.9 Detectar dezenas consecutivas
# ------------------------------------------------------------

tem_consecutivas <- function(x) {
  
  x <- sort(
    x
  )
  
  any(
    diff(x) == 1
  )
}


# ============================================================
# 5. EVENTOS BÁSICOS DA ATIVIDADE
# ============================================================


# ------------------------------------------------------------
# 5.1 Dezena 10
# ------------------------------------------------------------

prob_dezena_10 <-
  prob_dezena(10)

freq_dezena_10 <-
  freq_dezena(10)

n_dezena_10 <-
  n_dezena(10)


# ------------------------------------------------------------
# 5.2 Exatamente três dezenas pares
# ------------------------------------------------------------

prob_3_pares <-
  prob_n_pares(3)

freq_3_pares <-
  freq_n_pares(3)


# ------------------------------------------------------------
# 5.3 Seis dezenas pares
# ------------------------------------------------------------

prob_6_pares <-
  prob_n_pares(6)

freq_6_pares <-
  freq_n_pares(6)


# ------------------------------------------------------------
# 5.4 Seis dezenas ímpares
# ------------------------------------------------------------

prob_6_impares <-
  prob_n_pares(0)

freq_6_impares <-
  freq_n_pares(0)


# ------------------------------------------------------------
# 5.5 Todas as dezenas menores ou iguais a 30
# ------------------------------------------------------------

prob_todas_ate_30 <-
  choose(
    30,
    N_SORTEADAS
  ) /
  N_COMBINACOES


evento_todas_ate_30 <-
  apply(
    matriz_dezenas,
    1,
    function(x) {
      all(
        x <= 30
      )
    }
  )


n_todas_ate_30 <-
  sum(
    evento_todas_ate_30
  )


freq_todas_ate_30 <-
  mean(
    evento_todas_ate_30
  )


# ------------------------------------------------------------
# 5.6 Duas combinações específicas
# ------------------------------------------------------------

combinacao_1 <-
  c(
    1,
    2,
    3,
    4,
    5,
    6
  )


combinacao_2 <-
  c(
    7,
    19,
    28,
    36,
    44,
    59
  )


prob_combinacao_1 <-
  prob_combinacao(
    combinacao_1
  )


prob_combinacao_2 <-
  prob_combinacao(
    combinacao_2
  )


n_combinacao_1 <-
  n_combinacao(
    combinacao_1
  )


n_combinacao_2 <-
  n_combinacao(
    combinacao_2
  )


freq_combinacao_1 <-
  freq_combinacao(
    combinacao_1
  )


freq_combinacao_2 <-
  freq_combinacao(
    combinacao_2
  )


# ------------------------------------------------------------
# 5.7 Número esperado de aparições de uma dezena
# ------------------------------------------------------------

n_esperado_dezena <-
  n_concursos *
  P_DEZENA


# ============================================================
# 6. TABELA GERAL: MODELO x DADOS
# ============================================================

comparacao <- tibble(
  
  evento = c(
    "Dezena 10 sorteada",
    "Exatamente 3 dezenas pares",
    "6 dezenas pares",
    "6 dezenas ímpares",
    "Todas as dezenas <= 30"
  ),
  
  prob_teorica = c(
    prob_dezena_10,
    prob_3_pares,
    prob_6_pares,
    prob_6_impares,
    prob_todas_ate_30
  ),
  
  freq_observada = c(
    freq_dezena_10,
    freq_3_pares,
    freq_6_pares,
    freq_6_impares,
    freq_todas_ate_30
  )
) |>
  mutate(
    diferenca =
      freq_observada -
      prob_teorica
  )


comparacao_pct <- comparacao |>
  mutate(
    prob_teorica =
      100 *
      prob_teorica,
    
    freq_observada =
      100 *
      freq_observada,
    
    diferenca =
      100 *
      diferenca
  )


# ============================================================
# 7. FREQUÊNCIAS DAS 60 DEZENAS
# ============================================================

contagens_dezenas <- tabulate(
  as.integer(
    matriz_dezenas
  ),
  nbins = N_DEZENAS
)


frequencias_dezenas <- tibble(
  
  dezena =
    1:N_DEZENAS,
  
  n =
    contagens_dezenas,
  
  frequencia_relativa =
    contagens_dezenas /
    n_concursos
) |>
  mutate(
    frequencia_pct =
      100 *
      frequencia_relativa,
    
    diferenca_pct =
      100 *
      (
        frequencia_relativa -
          P_DEZENA
      )
  )


mais_frequentes <-
  frequencias_dezenas |>
  slice_max(
    frequencia_relativa,
    n = 5,
    with_ties = TRUE
  )


menos_frequentes <-
  frequencias_dezenas |>
  slice_min(
    frequencia_relativa,
    n = 5,
    with_ties = TRUE
  )


dezena_maxima <-
  frequencias_dezenas |>
  slice_max(
    frequencia_relativa,
    n = 1,
    with_ties = TRUE
  )


dezena_minima <-
  frequencias_dezenas |>
  slice_min(
    frequencia_relativa,
    n = 1,
    with_ties = TRUE
  )


# ============================================================
# 8. DISTRIBUIÇÃO DO NÚMERO DE DEZENAS PARES
# ============================================================

distribuicao_pares <- tibble(
  n_pares =
    0:N_SORTEADAS
) |>
  mutate(
    
    prob_teorica =
      vapply(
        n_pares,
        prob_n_pares,
        numeric(1)
      ),
    
    freq_observada =
      vapply(
        n_pares,
        freq_n_pares,
        numeric(1)
      ),
    
    diferenca =
      freq_observada -
      prob_teorica
  )


# ============================================================
# 9. DISTRIBUIÇÃO DO MÍNIMO
# ============================================================


# ------------------------------------------------------------
# 9.1 Distribuição observada
# ------------------------------------------------------------

resumo_minimo <- mega_sorteios |>
  count(
    minimo,
    name = "n"
  ) |>
  mutate(
    frequencia_relativa =
      n /
      n_concursos,
    
    frequencia_pct =
      100 *
      frequencia_relativa
  )


# ------------------------------------------------------------
# 9.2 Distribuição teórica
# ------------------------------------------------------------
#
# Se L representa o mínimo, então
#
# P(L = l) =
# choose(60 - l, 5) / choose(60, 6),
#
# para l = 1, ..., 55.
#

distribuicao_minimo_teorica <- tibble(
  minimo =
    1:55
) |>
  mutate(
    prob_teorica =
      choose(
        N_DEZENAS -
          minimo,
        N_SORTEADAS - 1
      ) /
      N_COMBINACOES,
    
    prob_teorica_pct =
      100 *
      prob_teorica
  )


comparacao_minimo <-
  distribuicao_minimo_teorica |>
  left_join(
    resumo_minimo,
    by = "minimo"
  ) |>
  mutate(
    n =
      coalesce(
        n,
        0L
      ),
    
    frequencia_relativa =
      coalesce(
        frequencia_relativa,
        0
      ),
    
    frequencia_pct =
      100 *
      frequencia_relativa,
    
    diferenca_pct =
      frequencia_pct -
      prob_teorica_pct
  )


moda_minimo_teorica <-
  comparacao_minimo |>
  slice_max(
    prob_teorica,
    n = 1,
    with_ties = TRUE
  )


moda_minimo_observada <-
  comparacao_minimo |>
  slice_max(
    frequencia_relativa,
    n = 1,
    with_ties = TRUE
  )


maior_minimo <-
  max(
    mega_sorteios$minimo,
    na.rm = TRUE
  )


concursos_maior_minimo <-
  mega_sorteios |>
  filter(
    minimo ==
      maior_minimo
  )


# ============================================================
# 10. DISTRIBUIÇÃO DO MÁXIMO
# ============================================================


# ------------------------------------------------------------
# 10.1 Distribuição observada
# ------------------------------------------------------------

resumo_maximo <- mega_sorteios |>
  count(
    maximo,
    name = "n"
  ) |>
  mutate(
    frequencia_relativa =
      n /
      n_concursos,
    
    frequencia_pct =
      100 *
      frequencia_relativa
  )


# ------------------------------------------------------------
# 10.2 Distribuição teórica
# ------------------------------------------------------------
#
# Se U representa o máximo, então
#
# P(U = u) =
# choose(u - 1, 5) / choose(60, 6),
#
# para u = 6, ..., 60.
#

distribuicao_maximo_teorica <- tibble(
  maximo =
    N_SORTEADAS:N_DEZENAS
) |>
  mutate(
    prob_teorica =
      choose(
        maximo - 1,
        N_SORTEADAS - 1
      ) /
      N_COMBINACOES,
    
    prob_teorica_pct =
      100 *
      prob_teorica
  )


comparacao_maximo <-
  distribuicao_maximo_teorica |>
  left_join(
    resumo_maximo,
    by = "maximo"
  ) |>
  mutate(
    n =
      coalesce(
        n,
        0L
      ),
    
    frequencia_relativa =
      coalesce(
        frequencia_relativa,
        0
      ),
    
    frequencia_pct =
      100 *
      frequencia_relativa,
    
    diferenca_pct =
      frequencia_pct -
      prob_teorica_pct
  )


moda_maximo_teorica <-
  comparacao_maximo |>
  slice_max(
    prob_teorica,
    n = 1,
    with_ties = TRUE
  )


moda_maximo_observada <-
  comparacao_maximo |>
  slice_max(
    frequencia_relativa,
    n = 1,
    with_ties = TRUE
  )


menor_maximo <-
  min(
    mega_sorteios$maximo,
    na.rm = TRUE
  )


concursos_menor_maximo <-
  mega_sorteios |>
  filter(
    maximo ==
      menor_maximo
  )


# ============================================================
# 11. MÉDIA DAS SEIS DEZENAS
# ============================================================


# ------------------------------------------------------------
# 11.1 Resumo observado
# ------------------------------------------------------------

resumo_media <- mega_sorteios |>
  summarise(
    
    media_observada =
      mean(
        media
      ),
    
    desvio_padrao_observado =
      sd(
        media
      ),
    
    menor_media =
      min(
        media
      ),
    
    maior_media =
      max(
        media
      )
  )


# ------------------------------------------------------------
# 11.2 Média teórica
# ------------------------------------------------------------

media_teorica <-
  (
    N_DEZENAS + 1
  ) /
  2


# ------------------------------------------------------------
# 11.3 Variância populacional das dezenas 1,...,60
# ------------------------------------------------------------
#
# Para uma variável uniforme discreta em 1,...,N:
#
# Var(X) = (N^2 - 1)/12.
#

variancia_populacional <-
  (
    N_DEZENAS^2 -
      1
  ) /
  12


# ------------------------------------------------------------
# 11.4 Variância e desvio-padrão teóricos da média
# ------------------------------------------------------------
#
# Como o sorteio é feito sem reposição:
#
# Var(Xbar) =
# Var(X)/n * (N-n)/(N-1).
#

variancia_media_teorica <-
  variancia_populacional /
  N_SORTEADAS *
  (
    N_DEZENAS -
      N_SORTEADAS
  ) /
  (
    N_DEZENAS -
      1
  )


dp_media_teorico <-
  sqrt(
    variancia_media_teorica
  )


# ------------------------------------------------------------
# 11.5 Comparação teoria x dados
# ------------------------------------------------------------

comparacao_media <- tibble(
  
  medida = c(
    "Média",
    "Desvio-padrão"
  ),
  
  teorico = c(
    media_teorica,
    dp_media_teorico
  ),
  
  observado = c(
    resumo_media$media_observada,
    resumo_media$desvio_padrao_observado
  )
)


# ============================================================
# 12. DEZENAS CONSECUTIVAS
# ============================================================


# ------------------------------------------------------------
# 12.1 Probabilidade teórica
# ------------------------------------------------------------

prob_sem_consecutivas <-
  choose(
    N_DEZENAS -
      N_SORTEADAS +
      1,
    N_SORTEADAS
  ) /
  N_COMBINACOES


prob_com_consecutivas <-
  1 -
  prob_sem_consecutivas


# ------------------------------------------------------------
# 12.2 Identificação nos concursos históricos
# ------------------------------------------------------------

evento_com_consecutivas <-
  apply(
    matriz_dezenas,
    1,
    tem_consecutivas
  )


# ------------------------------------------------------------
# 12.3 Frequências observadas
# ------------------------------------------------------------

n_com_consecutivas <-
  sum(
    evento_com_consecutivas
  )


freq_com_consecutivas <-
  mean(
    evento_com_consecutivas
  )


n_sem_consecutivas <-
  n_concursos -
  n_com_consecutivas


freq_sem_consecutivas <-
  1 -
  freq_com_consecutivas


comparacao_consecutivas <- tibble(
  
  evento = c(
    "Pelo menos um par consecutivo",
    "Nenhum par consecutivo"
  ),
  
  prob_teorica = c(
    prob_com_consecutivas,
    prob_sem_consecutivas
  ),
  
  n_observado = c(
    n_com_consecutivas,
    n_sem_consecutivas
  ),
  
  freq_observada = c(
    freq_com_consecutivas,
    freq_sem_consecutivas
  )
) |>
  mutate(
    prob_teorica_pct =
      100 *
      prob_teorica,
    
    freq_observada_pct =
      100 *
      freq_observada,
    
    diferenca_pct =
      100 *
      (
        freq_observada -
          prob_teorica
      )
  )


# ============================================================
# 13. APOSTAS MÚLTIPLAS E COMBINAÇÕES PREMIADAS
# ============================================================


# ------------------------------------------------------------
# 13.1 Número de apostas simples contidas em uma aposta múltipla
# ------------------------------------------------------------

n_combinacoes_aposta <- function(m) {
  
  if (
    !eh_inteiro(m) ||
    m < N_SORTEADAS ||
    m > N_DEZENAS
  ) {
    stop(
      "m deve ser um número inteiro entre 6 e 60."
    )
  }
  
  choose(
    m,
    N_SORTEADAS
  )
}


# ------------------------------------------------------------
# 13.2 Probabilidade de uma aposta com m dezenas acertar a Sena
# ------------------------------------------------------------

prob_sena_aposta <- function(m) {
  
  n_combinacoes_aposta(m) /
    N_COMBINACOES
}


# ------------------------------------------------------------
# 13.3 Número de combinações internas com k acertos
# ------------------------------------------------------------
#
# Suponha que a aposta contenha m dezenas e que r delas
# pertençam às seis dezenas sorteadas.
#
# Entre as choose(m,6) combinações simples contidas na aposta,
# o número de combinações com exatamente k acertos é
#
# choose(r,k) * choose(m-r,6-k).
#

n_premios_aposta <- function(m, r, k) {
  
  if (
    !eh_inteiro(m) ||
    m < N_SORTEADAS ||
    m > N_DEZENAS
  ) {
    stop(
      "m deve ser um número inteiro entre 6 e 60."
    )
  }
  
  if (
    !eh_inteiro(r) ||
    r < 0 ||
    r > N_SORTEADAS ||
    r > m
  ) {
    stop(
      "r deve ser um número inteiro válido entre 0 e 6."
    )
  }
  
  if (
    !eh_inteiro(k) ||
    k < 0 ||
    k > N_SORTEADAS
  ) {
    stop(
      "k deve ser um número inteiro entre 0 e 6."
    )
  }
  
  if (k > r) {
    return(0)
  }
  
  if ((N_SORTEADAS - k) > (m - r)) {
    return(0)
  }
  
  choose(
    r,
    k
  ) *
    choose(
      m - r,
      N_SORTEADAS - k
    )
}


# ------------------------------------------------------------
# 13.4 Apostas de interesse
# ------------------------------------------------------------

m_apostas <- c(
  6,
  7,
  8,
  10
)


# ------------------------------------------------------------
# 13.5 Quantidade de combinações simples e chance de Sena
# ------------------------------------------------------------

tabela_sena_apostas <- tibble(
  
  dezenas_apostadas =
    m_apostas,
  
  combinacoes_simples =
    vapply(
      m_apostas,
      n_combinacoes_aposta,
      numeric(1)
    ),
  
  prob_sena =
    vapply(
      m_apostas,
      prob_sena_aposta,
      numeric(1)
    )
) |>
  mutate(
    
    aumento_relativo =
      combinacoes_simples,
    
    uma_em =
      round(
        1 /
          prob_sena
      )
  )


# ------------------------------------------------------------
# 13.6 Se a aposta contém as seis dezenas sorteadas
# ------------------------------------------------------------
#
# Neste caso r = 6.
#

premios_se_acerta_sena <- tibble(
  
  dezenas_apostadas =
    m_apostas,
  
  sena =
    vapply(
      m_apostas,
      function(m) {
        n_premios_aposta(
          m = m,
          r = 6,
          k = 6
        )
      },
      numeric(1)
    ),
  
  quinas =
    vapply(
      m_apostas,
      function(m) {
        n_premios_aposta(
          m = m,
          r = 6,
          k = 5
        )
      },
      numeric(1)
    ),
  
  quadras =
    vapply(
      m_apostas,
      function(m) {
        n_premios_aposta(
          m = m,
          r = 6,
          k = 4
        )
      },
      numeric(1)
    )
)




# ============================================================
# 14. SIMULAÇÃO SIMULTÂNEA DAS FREQUÊNCIAS DAS 60 DEZENAS
# ============================================================


# ------------------------------------------------------------
# 14.1 Função de simulação
# ------------------------------------------------------------

simula_extremos_frequencias <- function() {
  
  sorteios <- replicate(
    n_concursos,
    sample.int(
      N_DEZENAS,
      size = N_SORTEADAS,
      replace = FALSE
    )
  )
  
  contagens <- tabulate(
    sorteios,
    nbins = N_DEZENAS
  )
  
  proporcoes <-
    contagens /
    n_concursos
  
  c(
    minimo =
      min(
        proporcoes
      ),
    
    maximo =
      max(
        proporcoes
      ),
    
    desvio_max =
      max(
        abs(
          proporcoes -
            P_DEZENA
        )
      )
  )
}


# ------------------------------------------------------------
# 14.2 Executar as simulações
# ------------------------------------------------------------

extremos_freq_simulados <- replicate(
  B,
  simula_extremos_frequencias()
)


extremos_freq_simulados <-
  t(
    extremos_freq_simulados
  )


# ------------------------------------------------------------
# 14.3 Quantis simulados
# ------------------------------------------------------------

faixa_minimo_freq <- quantile(
  extremos_freq_simulados[, "minimo"],
  probs = c(
    0.025,
    0.50,
    0.975
  )
)


faixa_maximo_freq <- quantile(
  extremos_freq_simulados[, "maximo"],
  probs = c(
    0.025,
    0.50,
    0.975
  )
)


# ------------------------------------------------------------
# 14.4 Faixa simultânea de referência de 95%
# ------------------------------------------------------------

d95 <- unname(
  quantile(
    extremos_freq_simulados[, "desvio_max"],
    probs = 0.95
  )
)


limite_simultaneo_inferior <-
  P_DEZENA -
  d95


limite_simultaneo_superior <-
  P_DEZENA +
  d95


faixa_simultanea_95 <- c(
  limite_inferior =
    limite_simultaneo_inferior,
  
  limite_superior =
    limite_simultaneo_superior
)


dezenas_fora_faixa <-
  frequencias_dezenas |>
  filter(
    frequencia_relativa <
      limite_simultaneo_inferior |
      frequencia_relativa >
      limite_simultaneo_superior
  )


# ============================================================
# 15. GRÁFICOS
# ============================================================


# ------------------------------------------------------------
# 15.1 Frequências das 60 dezenas
# ------------------------------------------------------------

grafico_dezenas <- ggplot(
  frequencias_dezenas,
  aes(
    x = dezena,
    y = frequencia_pct
  )
) +
  annotate(
    "rect",
    xmin = -Inf,
    xmax = Inf,
    ymin =
      100 *
      limite_simultaneo_inferior,
    ymax =
      100 *
      limite_simultaneo_superior,
    fill = "grey80",
    alpha = 0.5
  ) +
  geom_col(
    fill = "steelblue"
  ) +
  geom_hline(
    yintercept =
      100 *
      P_DEZENA,
    linetype = "dashed",
    color = "red",
    linewidth = 0.8
  ) +
  scale_x_continuous(
    breaks =
      seq(
        5,
        N_DEZENAS,
        by = 5
      )
  ) +
  labs(
    title =
      "Frequência das dezenas nos concursos da Mega-Sena",
    
    subtitle =
      paste0(
        "Linha tracejada: 10%; ",
        "faixa cinza: região simultânea de referência de 95%"
      ),
    
    x =
      "Dezena",
    
    y =
      "Frequência observada (%)"
  ) +
  theme_minimal(
    base_size = 12
  )


# ------------------------------------------------------------
# 15.2 Número de dezenas pares
# ------------------------------------------------------------

distribuicao_pares_long <-
  distribuicao_pares |>
  select(
    n_pares,
    prob_teorica,
    freq_observada
  ) |>
  pivot_longer(
    cols = c(
      prob_teorica,
      freq_observada
    ),
    names_to = "tipo",
    values_to = "valor"
  ) |>
  mutate(
    valor =
      100 *
      valor,
    
    tipo = recode(
      tipo,
      prob_teorica =
        "Modelo teórico",
      freq_observada =
        "Dados observados"
    )
  )


grafico_pares <- ggplot(
  distribuicao_pares_long,
  aes(
    x = factor(
      n_pares
    ),
    y = valor,
    fill = tipo
  )
) +
  geom_col(
    position = "dodge",
    width = 0.75
  ) +
  labs(
    title =
      "Número de dezenas pares em um sorteio",
    
    subtitle =
      "Comparação entre o modelo probabilístico e os dados observados",
    
    x =
      "Número de dezenas pares",
    
    y =
      "Percentual (%)",
    
    fill = NULL
  ) +
  theme_minimal(
    base_size = 12
  ) +
  theme(
    legend.position =
      "bottom"
  )


# ------------------------------------------------------------
# 15.3 Distribuição simulada da maior frequência
# ------------------------------------------------------------

extremos_freq_df <-
  as.data.frame(
    extremos_freq_simulados
  )


grafico_extremos_freq <- ggplot(
  extremos_freq_df,
  aes(
    x = maximo
  )
) +
  geom_histogram(
    bins = 30,
    fill = "steelblue",
    color = "white",
    alpha = 0.8
  ) +
  geom_vline(
    xintercept =
      max(
        frequencias_dezenas$
          frequencia_relativa
      ),
    color = "red",
    linewidth = 1
  ) +
  labs(
    title =
      "Distribuição simulada da maior frequência",
    
    subtitle =
      "Linha vermelha: maior frequência observada nos dados",
    
    x =
      "Maior frequência relativa entre as 60 dezenas",
    
    y =
      "Número de simulações"
  ) +
  theme_minimal(
    base_size = 12
  )


# ------------------------------------------------------------
# 15.4 Mínimo: teoria x dados
# ------------------------------------------------------------

grafico_minimo_teoria_dados <- ggplot(
  comparacao_minimo,
  aes(
    x = minimo
  )
) +
  geom_col(
    aes(
      y = frequencia_pct,
      fill = "Dados observados"
    ),
    alpha = 0.75,
    width = 0.8
  ) +
  geom_line(
    aes(
      y = prob_teorica_pct,
      color = "Modelo teórico"
    ),
    linewidth = 1
  ) +
  geom_point(
    aes(
      y = prob_teorica_pct,
      color = "Modelo teórico"
    ),
    size = 1.5
  ) +
  scale_fill_manual(
    values = c(
      "Dados observados" =
        "steelblue"
    )
  ) +
  scale_color_manual(
    values = c(
      "Modelo teórico" =
        "red"
    )
  ) +
  scale_x_continuous(
    breaks =
      seq(
        1,
        55,
        by = 5
      )
  ) +
  labs(
    title =
      "Distribuição da menor dezena sorteada",
    
    subtitle =
      "Comparação entre o modelo teórico e os dados observados",
    
    x =
      "Menor dezena do sorteio",
    
    y =
      "Percentual (%)",
    
    fill = NULL,
    color = NULL
  ) +
  theme_minimal(
    base_size = 12
  ) +
  theme(
    legend.position =
      "bottom"
  )


# ------------------------------------------------------------
# 15.5 Máximo: teoria x dados
# ------------------------------------------------------------

grafico_maximo_teoria_dados <- ggplot(
  comparacao_maximo,
  aes(
    x = maximo
  )
) +
  geom_col(
    aes(
      y = frequencia_pct,
      fill = "Dados observados"
    ),
    alpha = 0.75,
    width = 0.8
  ) +
  geom_line(
    aes(
      y = prob_teorica_pct,
      color = "Modelo teórico"
    ),
    linewidth = 1
  ) +
  geom_point(
    aes(
      y = prob_teorica_pct,
      color = "Modelo teórico"
    ),
    size = 1.5
  ) +
  scale_fill_manual(
    values = c(
      "Dados observados" =
        "steelblue"
    )
  ) +
  scale_color_manual(
    values = c(
      "Modelo teórico" =
        "red"
    )
  ) +
  scale_x_continuous(
    breaks =
      seq(
        6,
        60,
        by = 5
      )
  ) +
  labs(
    title =
      "Distribuição da maior dezena sorteada",
    
    subtitle =
      "Comparação entre o modelo teórico e os dados observados",
    
    x =
      "Maior dezena do sorteio",
    
    y =
      "Percentual (%)",
    
    fill = NULL,
    color = NULL
  ) +
  theme_minimal(
    base_size = 12
  ) +
  theme(
    legend.position =
      "bottom"
  )


# ------------------------------------------------------------
# 15.6 Média das seis dezenas
# ------------------------------------------------------------

grafico_medias <- ggplot(
  mega_sorteios,
  aes(
    x = media
  )
) +
  geom_histogram(
    binwidth = 1,
    boundary = 0.5,
    fill = "steelblue",
    color = "white"
  ) +
  geom_vline(
    xintercept =
      media_teorica,
    linetype = "dashed",
    color = "red",
    linewidth = 0.8
  ) +
  labs(
    title =
      "Média das seis dezenas sorteadas",
    
    subtitle =
      "Linha tracejada: média teórica de 30,5",
    
    x =
      "Média das seis dezenas",
    
    y =
      "Número de concursos"
  ) +
  theme_minimal(
    base_size = 12
  )


# ============================================================
# 16. RESUMO DOS RESULTADOS NO CONSOLE
# ============================================================

cat("\n")
cat("============================================================\n")
cat("RESULTADOS DA ANÁLISE DA MEGA-SENA\n")
cat("============================================================\n\n")


cat(
  "Número de concursos:",
  n_concursos,
  "\n"
)


cat(
  "Período:",
  format(
    periodo[1],
    "%d/%m/%Y"
  ),
  "a",
  format(
    periodo[2],
    "%d/%m/%Y"
  ),
  "\n\n"
)


cat("DEZENA 10\n")

cat(
  "Probabilidade teórica:",
  round(
    prob_dezena_10,
    6
  ),
  "\n"
)

cat(
  "Número observado:",
  n_dezena_10,
  "\n"
)

cat(
  "Frequência observada:",
  round(
    100 *
      freq_dezena_10,
    3
  ),
  "%\n\n"
)


cat("EXATAMENTE 3 DEZENAS PARES\n")

cat(
  "Probabilidade teórica:",
  round(
    100 *
      prob_3_pares,
    3
  ),
  "%\n"
)

cat(
  "Frequência observada:",
  round(
    100 *
      freq_3_pares,
    3
  ),
  "%\n\n"
)


cat("6 DEZENAS PARES\n")

cat(
  "Probabilidade teórica:",
  round(
    100 *
      prob_6_pares,
    3
  ),
  "%\n"
)

cat(
  "Frequência observada:",
  round(
    100 *
      freq_6_pares,
    3
  ),
  "%\n\n"
)


cat("6 DEZENAS ÍMPARES\n")

cat(
  "Probabilidade teórica:",
  round(
    100 *
      prob_6_impares,
    3
  ),
  "%\n"
)

cat(
  "Frequência observada:",
  round(
    100 *
      freq_6_impares,
    3
  ),
  "%\n\n"
)


cat("TODAS AS DEZENAS <= 30\n")

cat(
  "Probabilidade teórica:",
  round(
    100 *
      prob_todas_ate_30,
    3
  ),
  "%\n"
)

cat(
  "Número observado:",
  n_todas_ate_30,
  "\n"
)

cat(
  "Frequência observada:",
  round(
    100 *
      freq_todas_ate_30,
    3
  ),
  "%\n\n"
)


cat(
  "Número esperado de aparições de cada dezena:",
  n_esperado_dezena,
  "\n\n"
)


cat("DEZENA(S) MAIS FREQUENTE(S)\n")
print(
  dezena_maxima
)


cat("\nDEZENA(S) MENOS FREQUENTE(S)\n")
print(
  dezena_minima
)


cat("\nFAIXA SIMULTÂNEA DE REFERÊNCIA DE 95%\n")

cat(
  "Limite inferior:",
  round(
    100 *
      limite_simultaneo_inferior,
    2
  ),
  "%\n"
)

cat(
  "Limite superior:",
  round(
    100 *
      limite_simultaneo_superior,
    2
  ),
  "%\n"
)


cat("\nDEZENAS FORA DA FAIXA SIMULTÂNEA\n")
print(
  dezenas_fora_faixa
)


cat("\nDISTRIBUIÇÃO DO MÍNIMO\n")

cat("Moda teórica:\n")
print(
  moda_minimo_teorica
)

cat("\nModa observada:\n")
print(
  moda_minimo_observada
)

cat(
  "\nMaior mínimo observado:",
  maior_minimo,
  "\n"
)


cat("\nDISTRIBUIÇÃO DO MÁXIMO\n")

cat("Moda teórica:\n")
print(
  moda_maximo_teorica
)

cat("\nModa observada:\n")
print(
  moda_maximo_observada
)

cat(
  "\nMenor máximo observado:",
  menor_maximo,
  "\n"
)


cat("\nMÉDIA DAS SEIS DEZENAS\n")

print(
  comparacao_media
)

cat(
  "\nMenor média observada:",
  resumo_media$menor_media,
  "\n"
)

cat(
  "Maior média observada:",
  resumo_media$maior_media,
  "\n"
)


cat("\nDEZENAS CONSECUTIVAS\n")

cat(
  "Probabilidade teórica de pelo menos um par consecutivo:",
  round(
    100 *
      prob_com_consecutivas,
    3
  ),
  "%\n"
)

cat(
  "Número observado:",
  n_com_consecutivas,
  "\n"
)

cat(
  "Frequência observada:",
  round(
    100 *
      freq_com_consecutivas,
    3
  ),
  "%\n"
)


cat("\nCOMBINAÇÕES ESPECÍFICAS\n")

cat(
  "{01,02,03,04,05,06}:",
  n_combinacao_1,
  "ocorrências\n"
)

cat(
  "{07,19,28,36,44,59}:",
  n_combinacao_2,
  "ocorrências\n"
)


# ============================================================
# 17. SALVAR TABELAS
# ============================================================

write.csv(
  comparacao_pct,
  file.path(
    DIRETORIO_RESULTADOS,
    "tabela_comparacao.csv"
  ),
  row.names = FALSE
)


write.csv(
  frequencias_dezenas,
  file.path(
    DIRETORIO_RESULTADOS,
    "frequencias_dezenas.csv"
  ),
  row.names = FALSE
)


write.csv(
  distribuicao_pares,
  file.path(
    DIRETORIO_RESULTADOS,
    "distribuicao_pares.csv"
  ),
  row.names = FALSE
)


write.csv(
  comparacao_minimo,
  file.path(
    DIRETORIO_RESULTADOS,
    "distribuicao_minimo.csv"
  ),
  row.names = FALSE
)


write.csv(
  comparacao_maximo,
  file.path(
    DIRETORIO_RESULTADOS,
    "distribuicao_maximo.csv"
  ),
  row.names = FALSE
)


write.csv(
  comparacao_media,
  file.path(
    DIRETORIO_RESULTADOS,
    "comparacao_media.csv"
  ),
  row.names = FALSE
)


write.csv(
  comparacao_consecutivas,
  file.path(
    DIRETORIO_RESULTADOS,
    "comparacao_consecutivas.csv"
  ),
  row.names = FALSE
)


write.csv(
  dezenas_fora_faixa,
  file.path(
    DIRETORIO_RESULTADOS,
    "dezenas_fora_faixa_simultanea.csv"
  ),
  row.names = FALSE
)


# ============================================================
# 18. SALVAR GRÁFICOS
# ============================================================

ggsave(
  file.path(
    DIRETORIO_RESULTADOS,
    "grafico_frequencias_dezenas.png"
  ),
  grafico_dezenas,
  width = 9,
  height = 5,
  dpi = 300
)


ggsave(
  file.path(
    DIRETORIO_RESULTADOS,
    "grafico_distribuicao_pares.png"
  ),
  grafico_pares,
  width = 8,
  height = 5,
  dpi = 300
)


ggsave(
  file.path(
    DIRETORIO_RESULTADOS,
    "grafico_extremos_frequencias.png"
  ),
  grafico_extremos_freq,
  width = 8,
  height = 5,
  dpi = 300
)


ggsave(
  file.path(
    DIRETORIO_RESULTADOS,
    "grafico_minimo_teoria_dados.png"
  ),
  grafico_minimo_teoria_dados,
  width = 8,
  height = 5,
  dpi = 300
)


ggsave(
  file.path(
    DIRETORIO_RESULTADOS,
    "grafico_maximo_teoria_dados.png"
  ),
  grafico_maximo_teoria_dados,
  width = 8,
  height = 5,
  dpi = 300
)


ggsave(
  file.path(
    DIRETORIO_RESULTADOS,
    "grafico_medias.png"
  ),
  grafico_medias,
  width = 8,
  height = 5,
  dpi = 300
)