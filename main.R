library(dplyr)
library(readr)
library(stringr)

# 1. Ler CSV
bench <- read_csv("SPEC2017.csv")

# 2. Padronizar colunas (caso tenham espaços ou caracteres estranhos)
names(bench) <- make.names(names(bench))

# 3. Criar classificação inicial Servidor vs Desktop
bench <- bench %>%
  mutate(Type = case_when(
    str_detect(Processor, regex("EPYC|Xeon", ignore_case = TRUE)) ~ "Server",
    str_detect(Processor, regex("Ryzen|Core", ignore_case = TRUE)) ~ "Desktop",
    TRUE ~ "Other"
  ))

# 4. Filtrar só os que interessam (EPYC, Xeon, Ryzen, Core)
bench <- bench %>%
  filter(Type %in% c("Server", "Desktop"))

# 5. Selecionar variáveis relevantes
bench_clean <- bench %>%
  select(
    Hardware.Vendor,
    Processor,
    Type,
    X..Cores,
    Processor.MHz,
    Memory,
    Result,
    Baseline,
    HW.Avail,
    `X600.Base`, `X600.Peak`,
    `X602.Base`, `X602.Peak`,
    `X605.Base`, `X605.Peak`
  )

# 6. Criar dois datasets separados
server_data <- bench_clean %>% filter(Type == "Server")
desktop_data <- bench_clean %>% filter(Type == "Desktop")

# Visualizar
head(server_data)
head(desktop_data)

# Ver primeiras linhas
head(bench_clean)

# Resumo estatístico geral
summary(bench_clean)

# Contagem por tipo (Server vs Desktop)
bench_clean %>% count(Type)

# Distribuição por fabricante
bench_clean %>%
  mutate(Vendor = case_when(
    str_detect(Processor, "AMD") ~ "AMD",
    str_detect(Processor, "Intel") ~ "Intel",
    TRUE ~ "Other"
  )) %>%
  count(Type, Vendor)


# Calcular média, mediana, desvio padrão das variáveis principais:
bench_clean %>%
  group_by(Type) %>%
  summarise(
    media_cores = mean(X..Cores, na.rm = TRUE),
    sd_cores = sd(X..Cores, na.rm = TRUE),
    media_mhz = mean(Processor.MHz, na.rm = TRUE),
    media_result = mean(Result, na.rm = TRUE),
    media_baseline = mean(Baseline, na.rm = TRUE)
  )

# quem depende mais de otimização de compilador.

bench_clean %>%
  mutate(Gap = Result - Baseline) %>%
  group_by(Type, Vendor = ifelse(str_detect(Processor, "AMD"), "AMD", "Intel")) %>%
  summarise(
    media_gap = mean(Gap, na.rm = TRUE),
    sd_gap = sd(Gap, na.rm = TRUE)
  )


bench_clean %>%
  mutate(Vendor = case_when(
    str_detect(Processor, "AMD") ~ "AMD",
    str_detect(Processor, "Intel") ~ "Intel",
    TRUE ~ "Other"
  )) %>%
  ggplot(aes(x = Vendor, y = Result, fill = Vendor)) +
  geom_boxplot() +
  facet_wrap(~Type) +
  labs(title = "Distribuição do desempenho (Result) por fabricante e tipo")


# tratando os dias para facilitar os graficos
bench_clean <- bench_clean %>%
  mutate(Date = as.Date(paste0("01-", HW.Avail), format = "%d-%b-%Y"))

#bench_summary <- bench_clean %>%
#  group_by(Processor, HW.Avail, Type) %>%
#  summarise(Avg_Result = mean(Result, na.rm = TRUE)) %>%
#  ungroup() %>%
#  mutate(Date = as.Date(paste0("01-", HW.Avail), format = "%d-%b-%Y"),
#         Year = format(Date, "%Y"))
#
#trend_all <- bench_summary %>%
#  group_by(Year) %>%
#  summarise(Avg_Result = mean(Avg_Result, na.rm = TRUE),
#            Max_Result = max(Avg_Result, na.rm = TRUE),
#            Count = n())
#
#ggplot(trend_all, aes(x = as.integer(Year), y = Avg_Result)) +
#  geom_line(size = 1.2, color = "steelblue") +
#  geom_point(size = 2, color = "steelblue") +
#  labs(title = "Evolução do desempenho médio (Result) geral",
#       x = "Ano de lançamento",
#       y = "Desempenho médio (Result)") +
#  theme_minimal()


top_vendor <- bench_summary %>%
  mutate(Hardware.Vendor = case_when(
    str_detect(Processor, regex("EPYC|Ryzen", ignore_case = TRUE)) ~ "AMD",
    str_detect(Processor, regex("Xeon|Core", ignore_case = TRUE)) ~ "Intel",
    TRUE ~ "Other"
  )) %>%
  group_by(Year, Hardware.Vendor) %>%
  summarise(Max_Result = max(Avg_Result, na.rm = TRUE), .groups = "drop")

ggplot(top_vendor, aes(x = as.integer(Year), y = Max_Result, color = Hardware.Vendor)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  labs(title = "Top de desempenho por ano (AMD vs Intel)",
       x = "Ano de lançamento",
       y = "Maior desempenho (Result)") +
  theme_minimal()

