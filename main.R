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
# desktop_data <- bench_clean %>% filter(Type == "Desktop")

# Visualizar
head(server_data)
head(desktop_data)
