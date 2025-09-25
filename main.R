#################################################

# teste de analise com o teste de compressao de video 
library(ggplot2)
library(dplyr)

# Carregar dados 
rate_int <- read.csv("RATE_int.csv")
speed_int <- read.csv("SPEED_int.csv")

# Selecionar colunas de interesse + benchmarks de compressão
rate_video <- rate_int %>%
  select(Benchmark, Processor, Processor.MHz, X..Cores, X..Chips,
         X..Enabled.Threads.Per.Core, Memory, X3rd.Level.Cache, X525.Base, X525.Peak)

speed_video <- speed_int %>%
  select(Benchmark, Processor., Processor.MHz, X..Cores, X..Chips,
         X..Enabled.Threads.Per.Core, Memory, X3rd.Level.Cache, X625.Base, X625.Peak)

# Exemplo 1: relação núcleos vs 525.x264_r (throughput)
ggplot(rate_video, aes(x = X..Cores, y = X525.Base)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Relação entre # Cores e 525.x264_r (RATE_int)",
       x = "Número de Cores", y = "Resultado 525 Base")

# Exemplo 1 com o Peak
ggplot(rate_video, aes(x = X..Cores, y = X525.Peak)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Relação entre # Cores e 525.x264_r (RATE_int)",
       x = "Número de Cores", y = "Resultado 525 peak")
# Ponto importante: nem todos as CPUs receberam a otimização para aparecerem no Peak.


# Exemplo 2: relação frequência vs 625.x264_s (latência)
ggplot(speed_video, aes(x = Processor.MHz, y = X625.Base)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Relação entre Frequência e 625.264_s (SPEED_int)",
       x = "Frequência (MHz)", y = "Resultado 625 Base")

#################################################