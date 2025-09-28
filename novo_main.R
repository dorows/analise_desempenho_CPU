#################################################
#PREPARAÇÃO DOS DADOS

#COLETANDO SOMENTE AS COLUNAS NECESSARIAS E LENDO O SPEED_int.csv

# Carregar pacotes necessários
library(readr)
library(dplyr)
library(stringr)

# Ler o arquivo original
speed_raw <- read_csv("SPEED_int.csv")

# Verificar nomes das colunas
colnames(speed_raw)

# Selecionar colunas de interesse + removendo aqueles Baseline == 0
speed_clean <- speed_raw |>
  filter(Baseline != 0, Result != 0) |>
  filter(
    if_all(
      c(
        "600 Peak", "602 Peak", "605 Peak", "620 Peak",
        "623 Peak", "625 Peak", "631 Peak", "641 Peak",
        "648 Peak", "657 Peak"
      ),
      ~ .x != 0
    )
  ) |>
  
  # Função auxiliar interna para extrair cache em KB
  mutate(
    cache_l1_kb = str_split(`1st Level Cache`, "\\s+") |>
      lapply(function(x) {
        valor <- suppressWarnings(as.numeric(x[[1]]))
        unidade <- toupper(x[[2]])
        if (is.na(valor) || is.na(unidade)) return(NA_real_)
        if (unidade == "MB") return(valor * 1024)
        if (unidade == "KB") return(valor)
        return(NA_real_)
      }) |> unlist(),
    
    cache_l2_kb = str_split(`2nd Level Cache`, "\\s+") |>
      lapply(function(x) {
        valor <- suppressWarnings(as.numeric(x[[1]]))
        unidade <- toupper(x[[2]])
        if (is.na(valor) || is.na(unidade)) return(NA_real_)
        if (unidade == "MB") return(valor * 1024)
        if (unidade == "KB") return(valor)
        return(NA_real_)
      }) |> unlist(),
    
    cache_l3_kb = str_split(`3rd Level Cache`, "\\s+") |>
      lapply(function(x) {
        valor <- suppressWarnings(as.numeric(x[[1]]))
        unidade <- toupper(x[[2]])
        if (is.na(valor) || is.na(unidade)) return(NA_real_)
        if (unidade == "MB") return(valor * 1024)
        if (unidade == "KB") return(valor)
        return(NA_real_)
      }) |> unlist()
  ) |>
  
  # Selecionar colunas finais
  select(
    `Hardware Vendor`, System, `# Cores`, `# Chips`, `# Enabled Threads Per Core`,
    Processor, `Processor MHz`, `CPU(s) Orderable`
    , Storage, `Operating System`, Compiler, `HW Avail`,
    Result, Baseline,
    `600 Peak`, `600 Base`, `602 Peak`, `602 Base`,
    `605 Peak`, `605 Base`, `620 Peak`, `620 Base`,
    `623 Peak`, `623 Base`, `625 Peak`, `625 Base`,
    `631 Peak`, `631 Base`, `641 Peak`, `641 Base`,
    `648 Peak`, `648 Base`, `657 Peak`, `657 Base`,
    `Tested By`, `Test Sponsor`,
    cache_l1_kb, cache_l2_kb, cache_l3_kb
  )

# CLASSIFICANDO AS VARIÁVEIS
speed_clean <- speed_clean |>
  mutate(
    # --- Qualitativas nominais ---
    `Hardware Vendor` = as.factor(`Hardware Vendor`),
    System = as.factor(System),
    Processor = as.factor(Processor),
    `CPU(s) Orderable` = as.factor(`CPU(s) Orderable`),
    Storage = as.factor(Storage),
    `Operating System` = as.factor(`Operating System`),
    Compiler = as.factor(Compiler),
    `Tested By` = as.factor(`Tested By`),
    `Test Sponsor` = as.factor(`Test Sponsor`),
    
    # --- Qualitativas ordinais (datas) ---
    `HW Avail` = parse_date(`HW Avail`, format = "%b-%Y"),
    
    # --- Quantitativas discretas ---
    `# Cores` = as.integer(`# Cores`),
    `# Chips` = as.integer(`# Chips`),
    `# Enabled Threads Per Core` = as.integer(`# Enabled Threads Per Core`),
    cache_l1_kb = as.integer(cache_l1_kb),
    cache_l2_kb = as.integer(cache_l2_kb),
    cache_l3_kb = as.integer(cache_l3_kb),
    
    
    # --- Quantitativas contínuas (numeric) ---
    `Processor MHz` = as.numeric(`Processor MHz`),
    Result = as.numeric(Result),
    Baseline = as.numeric(Baseline),
    
    `600 Peak` = as.numeric(`600 Peak`),
    `600 Base` = as.numeric(`600 Base`),
    `602 Peak` = as.numeric(`602 Peak`),
    `602 Base` = as.numeric(`602 Base`),
    `605 Peak` = as.numeric(`605 Peak`),
    `605 Base` = as.numeric(`605 Base`),
    `620 Peak` = as.numeric(`620 Peak`),
    `620 Base` = as.numeric(`620 Base`),
    `623 Peak` = as.numeric(`623 Peak`),
    `623 Base` = as.numeric(`623 Base`),
    `625 Peak` = as.numeric(`625 Peak`),
    `625 Base` = as.numeric(`625 Base`),
    `631 Peak` = as.numeric(`631 Peak`),
    `631 Base` = as.numeric(`631 Base`),
    `641 Peak` = as.numeric(`641 Peak`),
    `641 Base` = as.numeric(`641 Base`),
    `648 Peak` = as.numeric(`648 Peak`),
    `648 Base` = as.numeric(`648 Base`),
    `657 Peak` = as.numeric(`657 Peak`),
    `657 Base` = as.numeric(`657 Base`)
  )

##testes
summary(speed_clean)
glimpse(speed_clean)


#################################################

#INICIO DA ANALISE

#CRIACAO E ESCRITA DA TABELA COM ESTATISTICAS DESCRITIVAS NA PASTA tabela_e_graficos

library(dplyr)
library(tidyr)

# Identificar colunas de benchmark (todas terminam com Base ou Peak e começam com 6)
bench_cols <- grep("^6\\d{2} (Base|Peak)$", names(speed_clean), value = TRUE)

# Juntar colunas de interesse: benchmarks + hardware
colunas_estatisticas <- c(
  grep("^6\\d{2} (Base|Peak)$", names(speed_clean), value = TRUE),
  "Processor MHz", "cache_l1_kb", "cache_l2_kb", "cache_l3_kb"
)

# Criar tabela descritiva
resumo_completo <- speed_clean |>
  select(all_of(colunas_estatisticas)) |>
  pivot_longer(cols = everything(), names_to = "variavel", values_to = "valor") |>
  group_by(variavel) |>
  summarise(
    media = mean(valor, na.rm = TRUE),
    mediana = median(valor, na.rm = TRUE),
    variancia = var(valor, na.rm = TRUE),
    desvio_padrao = sd(valor, na.rm = TRUE),
    q25 = quantile(valor, 0.25, na.rm = TRUE),
    q75 = quantile(valor, 0.75, na.rm = TRUE),
    minimo = min(valor, na.rm = TRUE),
    maximo = max(valor, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(variavel)

# Criar pasta se não existir
if (!dir.exists("tabelas_e_graficos")) {
  dir.create("tabelas_e_graficos")
}

# Salvar a tabela de resumo como CSV
readr::write_csv(resumo_completo, "tabelas_e_graficos/resumo_completo.csv")

#------------------------------------------------
# Criando dois gráficos, cada gráfico mostrará boxplots por benchmark

library(ggplot2)
library(tidyr)
library(dplyr)
library(readr)

# Identificar colunas Base e Peak
col_base <- grep("^6\\d{2} Base$", names(speed_clean), value = TRUE)
col_peak <- grep("^6\\d{2} Peak$", names(speed_clean), value = TRUE)

# Preparar os dados em formato longo (long format)
dados_base <- speed_clean |>
  select(all_of(col_base)) |>
  pivot_longer(cols = everything(), names_to = "benchmark", values_to = "valor")

dados_peak <- speed_clean |>
  select(all_of(col_peak)) |>
  pivot_longer(cols = everything(), names_to = "benchmark", values_to = "valor")

# Gráfico Base
grafico_base <- ggplot(dados_base, aes(x = benchmark, y = valor)) +
  geom_boxplot(fill = "#8ecae6", color = "#023047") +
  labs(
    title = "Distribuição dos Benchmarks - Condição Base",
    x = "Benchmark",
    y = "Pontuação"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Gráfico Peak
grafico_peak <- ggplot(dados_peak, aes(x = benchmark, y = valor)) +
  geom_boxplot(fill = "#ffb703", color = "#fb8500") +
  labs(
    title = "Distribuição dos Benchmarks - Condição Peak",
    x = "Benchmark",
    y = "Pontuação"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Criar pasta se necessário
if (!dir.exists("tabelas_e_graficos")) {
  dir.create("tabelas_e_graficos")
}

# Salvar os gráficos como imagens
ggsave("tabelas_e_graficos/boxplot_base.png", grafico_base, width = 10, height = 6)
ggsave("tabelas_e_graficos/boxplot_peak.png", grafico_peak, width = 10, height = 6)

#------------------------------------------------
#relacionando desempenho do Baseline com frequência, e níveis de cache

library(ggplot2)
library(patchwork) # Para juntar gráficos

# Criar gráfico para cada variável explicativa
p1 <- ggplot(speed_clean, aes(x = `Processor MHz`, y = Baseline)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Baseline vs Processor MHz") +
  theme_minimal()

p2 <- ggplot(speed_clean, aes(x = cache_l1_kb, y = Baseline)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "darkgreen") +
  labs(title = "Baseline vs Cache L1 (KB)") +
  theme_minimal()

p3 <- ggplot(speed_clean, aes(x = cache_l2_kb, y = Baseline)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "purple") +
  labs(title = "Baseline vs Cache L2 (KB)") +
  theme_minimal()

p4 <- ggplot(speed_clean, aes(x = cache_l3_kb, y = Baseline)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Baseline vs Cache L3 (KB)") +
  theme_minimal()

# Juntar os 4 gráficos
(p1 | p2) / (p3 | p4)

ggsave("tabelas_e_graficos/baseline_vs_hardware.png", (p1 | p2) / (p3 | p4), width = 12, height = 8)

library(tibble)
# Calcular a matriz de correlação de Pearson
correlacao <- speed_clean |>
  select(Baseline, `Processor MHz`, cache_l1_kb, cache_l2_kb, cache_l3_kb) |>
  cor(use = "complete.obs", method = "pearson")

# Transformar a matriz em data frame para salvar
correlacao_df <- as.data.frame(correlacao) |>
  rownames_to_column(var = "Variavel")

# Garantir que a pasta exista
if (!dir.exists("tabelas_e_graficos")) {
  dir.create("tabelas_e_graficos")
}

# Salvar a matriz como CSV
readr::write_csv(correlacao_df, "tabelas_e_graficos/pearson_baseline_vs_cpu.csv")

#------------------------------------------------
#adicionando:
#Gráfico 1: mediana do Baseline por semestre (HW Avail)
#Gráfico 2: gráfico de dispersão com Baseline ao longo do tempo

library(lubridate)

# Criar coluna de semestre
speed_clean <- speed_clean |>
  mutate(semestre = paste0(year(`HW Avail`), "-S", if_else(month(`HW Avail`) <= 6, "1", "2")))

# Gráfico 1: Mediana por semestre
grafico_mediana_tempo <- speed_clean |>
  group_by(semestre) |>
  summarise(mediana_baseline = median(Baseline, na.rm = TRUE), .groups = "drop") |>
  ggplot(aes(x = semestre, y = mediana_baseline, group = 1)) +
  geom_line(color = "steelblue", linewidth = 1.2) +
  geom_point(color = "steelblue", size = 2) +
  labs(
    title = "Evolução da Mediana do Baseline por Semestre",
    x = "Semestre",
    y = "Mediana do Baseline"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Gráfico 2: Dispersão de Baseline ao longo do tempo
grafico_dispersao_tempo <- ggplot(speed_clean, aes(x = `HW Avail`, y = Baseline)) +
  geom_point(alpha = 0.4, color = "#219ebc") +
  geom_smooth(method = "loess", se = FALSE, color = "darkblue") +
  labs(
    title = "Dispersão do Baseline ao Longo do Tempo",
    x = "Data de Disponibilidade do Hardware",
    y = "Baseline"
  ) +
  theme_minimal()

# Salvar os gráficos
ggsave("tabelas_e_graficos/baseline_mediana_semestre.png", grafico_mediana_tempo, width = 10, height = 6)
ggsave("tabelas_e_graficos/baseline_dispersao_tempo.png", grafico_dispersao_tempo, width = 10, height = 6)

#------------------------------------------------
#baseline vs cache

library(ggplot2)
library(patchwork)

# Função para adicionar log2 no eixo x
cache_plot <- function(cache_var, cache_label, color) {
  ggplot(speed_clean, aes(x = log2(!!sym(cache_var)), y = Baseline)) +
    geom_point(alpha = 0.4, color = color) +
    geom_smooth(method = "loess", se = FALSE, color = "black") +
    labs(
      title = paste("Baseline vs", cache_label),
      x = paste("log2(", cache_label, "KB)"),
      y = "Baseline"
    ) +
    theme_minimal()
}

# Criar os 3 gráficos
g1 <- cache_plot("cache_l1_kb", "Cache L1", "#219ebc")
g2 <- cache_plot("cache_l2_kb", "Cache L2", "#ffb703")
g3 <- cache_plot("cache_l3_kb", "Cache L3", "#fb8500")

# Juntar os três gráficos lado a lado
grafico_caches <- g1 | g2 | g3

# Mostrar e salvar
print(grafico_caches)
ggsave("tabelas_e_graficos/baseline_vs_caches_log2.png", grafico_caches, width = 14, height = 5)

#------------------------------------------------
# Gráfico: Baseline vs Processor MHz (com eixo log2)

grafico_frequencia <- ggplot(speed_clean, aes(x = log2(`Processor MHz`), y = Baseline)) +
  geom_point(alpha = 0.4, color = "#8ecae6") +
  geom_smooth(method = "loess", se = FALSE, color = "black") +
  labs(
    title = "Baseline vs Processor MHz (Escala log2)",
    x = "log2(Frequência do Processador em MHz)",
    y = "Baseline"
  ) +
  theme_minimal()

# Exibir
print(grafico_frequencia)

# Salvar
ggsave("tabelas_e_graficos/baseline_vs_processor_mhz_log2.png", grafico_frequencia, width = 8, height = 5)

#------------------------------------------------
#quantidade de sistemas por Hardware Vendor
#Selecionar os top 7 vendedores
#Filtrar o speed_clean apenas com esses vendedores
#Calcular estatísticas descritivas do Baseline por vendedor
#Criar um boxplot comparativo para analisar variações

library(dplyr)
library(ggplot2)
library(readr)

# 1. Identificar os top 7 vendedores
top_vendedores <- speed_clean |>
  count(`Hardware Vendor`, sort = TRUE) |>
  slice_head(n = 7) |>
  pull(`Hardware Vendor`)

# 2. Filtrar dados apenas com os top vendedores
dados_vendedores <- speed_clean |>
  filter(`Hardware Vendor` %in% top_vendedores)

# 3. Calcular estatísticas descritivas por vendedor
estatisticas_vendedores <- dados_vendedores |>
  group_by(`Hardware Vendor`) |>
  summarise(
    media = mean(Baseline, na.rm = TRUE),
    mediana = median(Baseline, na.rm = TRUE),
    desvio_padrao = sd(Baseline, na.rm = TRUE),
    q25 = quantile(Baseline, 0.25, na.rm = TRUE),
    q75 = quantile(Baseline, 0.75, na.rm = TRUE),
    minimo = min(Baseline, na.rm = TRUE),
    maximo = max(Baseline, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )

# 4. Salvar a tabela
readr::write_csv(estatisticas_vendedores, "tabelas_e_graficos/estatisticas_top_vendedores.csv")

# 5. Criar boxplot do Baseline por vendedor
grafico_vendedores <- ggplot(dados_vendedores, aes(x = `Hardware Vendor`, y = Baseline, fill = `Hardware Vendor`)) +
  geom_boxplot(show.legend = FALSE) +
  labs(
    title = "Distribuição do Baseline por Vendedor (Top 7)",
    x = "Vendedor",
    y = "Baseline"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

# Exibir e salvar
print(grafico_vendedores)
ggsave("tabelas_e_graficos/boxplot_baseline_por_vendedor.png", grafico_vendedores, width = 10, height = 6)

#------------------------------------------------
# analise de densidade, peak e base, para cada benchmark

library(ggplot2)
library(tidyr)
library(patchwork)

# 1. Selecionar colunas
bench_cols_base <- grep("^6\\d{2} Base$", names(speed_clean), value = TRUE)
bench_cols_peak <- grep("^6\\d{2} Peak$", names(speed_clean), value = TRUE)

# 2. Long format
dados_base <- speed_clean |>
  select(all_of(bench_cols_base)) |>
  pivot_longer(everything(), names_to = "benchmark", values_to = "valor") |>
  mutate(tipo = "Base")

dados_peak <- speed_clean |>
  select(all_of(bench_cols_peak)) |>
  pivot_longer(everything(), names_to = "benchmark", values_to = "valor") |>
  mutate(tipo = "Peak")

# 3. Juntar
dados_benchmarks <- bind_rows(dados_base, dados_peak)

# 4. Função para gráfico de densidade individual
plot_density <- function(df, tipo_filtro, y_lim = 0.5) {
  df |>
    filter(tipo == tipo_filtro) |>
    ggplot(aes(x = valor)) +
    geom_density(fill = ifelse(tipo_filtro == "Base", "#8ecae6", "#ffb703"), alpha = 0.6) +
    facet_wrap(~benchmark, scales = "free", ncol = 5) +
    labs(
      title = paste("Distribuição de Densidade - Benchmarks", tipo_filtro),
      x = "Pontuação",
      y = "Densidade"
    ) +
    coord_cartesian(ylim = c(0, y_lim)) +  # 👈 substitui ylim()
    theme_minimal()
}

# 5. Criar os dois painéis
plot_base <- plot_density(dados_benchmarks, "Base")
plot_peak <- plot_density(dados_benchmarks, "Peak")

# 6. Juntar (4 linhas: 2 base + 2 peak)
painel_densidade <- plot_base / plot_peak

# 7. Salvar
ggsave("tabelas_e_graficos/densidade_benchmarks_base_peak.png", painel_densidade, width = 16, height = 12)

#------------------------------------------------
# histogramas (em gráficos de barra) para as variáveis de cache
library(ggplot2)
library(patchwork)

# Histograma L1 - Barras finas
hist_l1 <- ggplot(speed_clean, aes(x = cache_l1_kb)) +
  geom_bar(fill = "#219ebc", width = 1) +
  labs(title = "Frequência - Cache L1 (KB)", x = "Cache L1 (KB)", y = "Contagem") +
  theme_minimal()

# Histograma L2 - Padrão
hist_l2 <- ggplot(speed_clean, aes(x = cache_l2_kb)) +
  geom_bar(fill = "#219ebc") +
  labs(title = "Frequência - Cache L2 (KB)", x = "Cache L2 (KB)", y = "Contagem") +
  theme_minimal()

# Histograma L3 - Base mais larga e barras mais grossas, limite do eixo y
hist_l3 <- ggplot(speed_clean, aes(x = cache_l3_kb)) +
  geom_histogram(binwidth = 15000, fill = "#219ebc", width = 150000) +
  ylim(0, 400) +
  labs(title = "Frequência - Cache L3 (KB)", x = "Cache L3 (KB)", y = "Contagem") +
  theme_minimal()

# Juntar e salvar
(hist_l1 | hist_l2 | hist_l3)

ggsave("tabelas_e_graficos/histogramas_cache_final.png", (hist_l1 | hist_l2 | hist_l3), width = 20, height = 5)
