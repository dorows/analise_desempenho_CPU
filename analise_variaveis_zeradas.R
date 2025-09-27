#################################################
#PREPARAÇÃO DOS DADOS

#COLETANDO SOMENTE AS COLUNAS NECESSARIAS E LENDO O SPEED_int.csv

# Carregar pacotes necessários
library(readr)
library(dplyr)

# Ler o arquivo original
speed_raw <- read_csv("SPEED_int.csv")

# Verificar nomes das colunas
colnames(speed_raw)

# Selecionar colunas de interesse + removendo aqueles Baseline == 0
speed_clean <- speed_raw |>
  filter(Baseline != 0) |>
  select(
    `Hardware Vendor`, System, `# Cores`, `# Chips`, `# Enabled Threads Per Core`,
    Processor, `Processor MHz`, `CPU(s) Orderable`, Parallel,
    `1st Level Cache`, `2nd Level Cache`, `3rd Level Cache`,
    Memory, Storage, `Operating System`, Compiler, `HW Avail`,
    Result, Baseline,
    `600 Peak`, `600 Base`, `602 Peak`, `602 Base`,
    `605 Peak`, `605 Base`, `620 Peak`, `620 Base`,
    `623 Peak`, `623 Base`, `625 Peak`, `625 Base`,
    `631 Peak`, `631 Base`, `641 Peak`, `641 Base`,
    `648 Peak`, `648 Base`, `657 Peak`, `657 Base`,
    `Tested By`, `Test Sponsor`
  )

#################################################

# ENTENDENDO ONDE ESTÃO AS VARIÁVEIS ZERADAS

library(dplyr)
library(lubridate)
library(ggplot2)

# Etapas: classificação, agrupamento e ordenação
resultados_por_semestre <- speed_clean |>
  mutate(
    hw_avail_date = parse_date(`HW Avail`, format = "%b-%Y"),
    semestre = if_else(month(hw_avail_date) <= 6, "1S", "2S"),
    ano = year(hw_avail_date),
    semestre_ano = paste0(semestre, "-", ano),
    result_zero = if_else(Result == 0, "Zerado", "Não zerado"),
    
    # Data fictícia para ordenação: 1º Sem -> jan, 2º Sem -> jul
    semestre_ordem = case_when(
      semestre == "1S" ~ ymd(paste0(ano, "-01-01")),
      semestre == "2S" ~ ymd(paste0(ano, "-07-01"))
    )
  ) |>
  count(semestre_ano, result_zero, semestre_ordem) |>
  arrange(semestre_ordem)

# Gráfico com eixo x ordenado corretamente
ggplot(resultados_por_semestre, aes(x = reorder(semestre_ano, semestre_ordem), y = n, fill = result_zero)) +
  geom_col(position = "dodge") +
  labs(
    title = "Resultados Zerados vs Não Zerados por Semestre (Ordenado)",
    x = "Semestre-Ano (em ordem cronológica)",
    y = "Contagem",
    fill = "Status do Result"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#CONTANDO QUANTAS VEZES RESULT = O COMPARADO EM RESULT > 0 NO MESMO SEMESTRE 


# Etapa 1: Adicionar colunas de semestre
speed_semestre <- speed_clean |>
  mutate(
    hw_avail_date = parse_date(`HW Avail`, format = "%b-%Y"),
    semestre = if_else(month(hw_avail_date) <= 6, "1S", "2S"),
    ano = year(hw_avail_date),
    semestre_ano = paste0(semestre, "-", ano),
    semestre_ordem = case_when(
      semestre == "1S" ~ ymd(paste0(ano, "-01-01")),
      semestre == "2S" ~ ymd(paste0(ano, "-07-01"))
    ),
    result_estado = if_else(Result == 0, "Result == 0", "Result > 0")
  )

# Etapa 2: Contar por semestre e estado de Result
comparabilidade_por_semestre <- speed_semestre |>
  count(semestre_ano, result_estado, semestre_ordem) |>
  arrange(semestre_ordem)

# Etapa 3: Gráfico
ggplot(comparabilidade_por_semestre, aes(x = reorder(semestre_ano, semestre_ordem), y = n, fill = result_estado)) +
  geom_col(position = "dodge") +
  labs(
    title = "Result == 0 vs Result > 0 no mesmo semestre",
    x = "Semestre-Ano",
    y = "Contagem de sistemas",
    fill = "Estado do Result"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#ACABOU O ENTENDIMENTO DE COMO AS VARIAVEIS ESTÃO ZERADAS
#################################################