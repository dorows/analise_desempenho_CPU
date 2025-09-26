#   1) Corrigir/classificar variáveis de acordo com a tabela especificada.
#   2) Criar tabela relacionando cada Benchmark com Base e Peak e gerar métricas estatísticas.
#   3) Comparar benchmarks com variáveis de arquitetura (caches, freq. CPU, memória).
#   4) Listar Hardware Vendors e analisar distribuições de Baseline por boxplot.

# Pacotes ----------------------------------------------------------------
if (!requireNamespace("pacman", quietly = TRUE)) {
  install.packages("pacman")
}

pacman::p_load(
  readr,
  dplyr,
  stringr,
  lubridate,
  forcats,
  purrr,
  tidyr,
  ggplot2,
  broom,
)

# Funções auxiliares -----------------------------------------------------
clean_numeric <- function(x) {
  if (is.numeric(x)) return(x)
  y <- as.character(x)
  y <- str_replace_all(y, ",", "")
  y <- str_extract(y, "[-+]?[0-9]*\\.?[0-9]+([Ee][-+]?[0-9]+)?")
  as.numeric(y)
}

safe_date <- function(x) {
  if (inherits(x, "Date")) return(x)
  x_chr <- as.character(x)
  out <- parse_date_time(x_chr, orders = c("Ymd", "ymd", "mdY", "dmy", "Y-m-d", "m/d/Y", "d/m/Y"), quiet = TRUE)
  as_date(out)
}

# Classificação das variáveis -------------------------------------------
nominal_vars <- c(
  "Benchmark", "Hardware Vendor", "System", "Processor", "CPU(s) Orderable", "Parallel",
  "Base Pointer Size", "Peak Pointer Size", "Storage", "Operating System", "File System",
  "Compiler", "License", "Tested By", "Test Sponsor", "Disclosure", "Disclosures"
)

ordinal_vars <- c("HW Avail", "SW Avail", "Test Date", "Published", "Updated")

discrete_vars <- c("# Cores", "# Chips", "# Enabled Threads Per Core")

continuous_vars <- c(
  "Processor MHz", "1st Level Cache", "2nd Level Cache", "3rd Level Cache", "Other Cache",
  "Memory", "Result", "Baseline", "Energy Peak Result", "Energy Base Result",
  "600 Peak", "600 Base", "602 Peak", "602 Base", "605 Peak", "605 Base",
  "620 Peak", "620 Base", "623 Peak", "623 Base", "625 Peak", "625 Base",
  "631 Peak", "631 Base", "641 Peak", "641 Base", "648 Peak", "648 Base",
  "657 Peak", "657 Base"
)

# Leitura dos dados ------------------------------------------------------
fn <- "SPEED_int.csv"
if (!file.exists(fn)) {
  stop("Arquivo SPEED_int.csv não encontrado no diretório de trabalho.")
}

raw <- readr::read_csv(fn, show_col_types = FALSE)
orig_colnames <- names(raw)
clean_names <- orig_colnames |> str_trim() |> str_replace_all("\n", " ")
names(raw) <- clean_names

df <- raw

# Aplicar classificação --------------------------------------------------
for (v in nominal_vars) {
  if (v %in% names(df)) df[[v]] <- df[[v]] |> as.character() |> forcats::as_factor()
}

for (v in ordinal_vars) {
  if (v %in% names(df)) {
    sample_vals <- na.omit(as.character(df[[v]]))
    parsed_dates <- length(sample_vals) > 0 && !all(is.na(safe_date(sample_vals)))
    if (parsed_dates) {
      df[[v]] <- safe_date(df[[v]])
    } else {
      lvls <- unique(na.omit(as.character(df[[v]])))
      df[[v]] <- factor(df[[v]], levels = lvls, ordered = TRUE)
    }
  }
}

for (v in discrete_vars) {
  if (v %in% names(df)) df[[v]] <- as.integer(clean_numeric(df[[v]]))
}

for (v in continuous_vars) {
  if (v %in% names(df)) df[[v]] <- clean_numeric(df[[v]])
}

maybe_numeric <- names(df)[!map_lgl(df, is.numeric)]
maybe_numeric <- maybe_numeric[map_lgl(df[maybe_numeric], ~ any(str_detect(as.character(na.omit(.x)), "[0-9]")))]
for (v in maybe_numeric) {
  if (v %in% c(nominal_vars, ordinal_vars, discrete_vars, continuous_vars)) next
  tmp <- clean_numeric(df[[v]])
  if (sum(!is.na(tmp)) > 0.6 * nrow(df)) df[[v]] <- tmp
}

# Filtro: remover linhas sem baseline ou benchmarks pouco testados -------
if ("Baseline" %in% names(df)) {
  df <- df |> filter(!is.na(Baseline) & Baseline > 0)
}

base_cols <- names(df)[str_detect(names(df), regex("Base", ignore_case = TRUE))]
base_cols <- base_cols[sapply(df[base_cols], is.numeric)]

if ("Benchmark" %in% names(df) && length(base_cols) > 0) {
  benchmark_validity <- df |> 
    select(Benchmark, all_of(base_cols)) |> 
    pivot_longer(-Benchmark, names_to = "metric", values_to = "value") |> 
    mutate(valid = !is.na(value) & value > 0) |> 
    group_by(Benchmark) |> 
    summarise(
      prop_valid_base = mean(valid),
      n_tests = n(),
      .groups = "drop"
    )
  
  valid_benchmarks <- benchmark_validity |> 
    filter(prop_valid_base >= 0.7) |> 
    pull(Benchmark)
  
  df <- df |> filter(Benchmark %in% valid_benchmarks)
}

# Criar pasta de saída ---------------------------------------------------
if (!dir.exists("speed_int_outputs")) dir.create("speed_int_outputs")

# Salvar metadados e dataset limpo ---------------------------------------
meta <- tibble::tibble(
  column = names(df),
  original_name = orig_colnames,
  class_after = map_chr(df, ~ class(.x)[1])
)
readr::write_csv(meta, "speed_int_outputs/column_classes_post_processing.csv")
readr::write_csv(df, "speed_int_outputs/SPEED_int_cleaned.csv")

# Mostrar summary geral --------------------------------------------------
print(summary(df))

# Passo 2: Criar tabela Benchmark × Base/Peak ----------------------------
base_cols <- names(df)[str_detect(names(df), regex("Base", ignore_case = TRUE))]
peak_cols <- names(df)[str_detect(names(df), regex("Peak", ignore_case = TRUE))]

# Manter apenas numéricas
base_cols <- base_cols[sapply(df[base_cols], is.numeric)]
peak_cols <- peak_cols[sapply(df[peak_cols], is.numeric)]

if (length(base_cols) > 0 || length(peak_cols) > 0) {
  bench_long <- df |>
    dplyr::select(Benchmark, dplyr::any_of(c(base_cols, peak_cols))) |> 
    tidyr::pivot_longer(-Benchmark, names_to = "variant", values_to = "value")
  
  bench_summary <- bench_long |> 
    dplyr::group_by(Benchmark, variant) |> 
    dplyr::summarise(
      n = sum(!is.na(value)),
      mean_result   = mean(value, na.rm = TRUE),
      sd_result     = sd(value, na.rm = TRUE),
      var_result    = var(value, na.rm = TRUE),
      median_result = median(value, na.rm = TRUE),
      iqr_result    = IQR(value, na.rm = TRUE),
      min_result    = min(value, na.rm = TRUE),
      max_result    = max(value, na.rm = TRUE),
      .groups = "drop"
    )
  
  readr::write_csv(bench_summary, "speed_int_outputs/bench_summary_base_peak.csv")
  print(head(bench_summary, 10))
} else {
  message("Não foram encontradas colunas numéricas com 'Base' ou 'Peak'.")
}

pacman::p_load(broom, car)

# --- 1) Preparar lista de variantes numéricas disponíveis ----------------
variant_cols <- names(df)[str_detect(names(df), "Base|Peak")]
variant_cols <- variant_cols[sapply(df[variant_cols], is.numeric)]

# helper: ajusta modelo padronizado por variante -------------------------
fit_variant <- function(var_col, arch_vars) {
  dat <- df |>
    select(all_of(c(var_col, arch_vars))) |>
    rename(Result = !!var_col) |>
    filter(!is.na(Result)) |>
    # padronizar preditores: média 0, sd 1
    mutate(across(all_of(arch_vars), scale, .names = "{.col}"))
  
  if (nrow(dat) < 10) return(NULL)  # evita ajustes instáveis
  
  fit <- lm(Result ~ ., data = dat)
  tibble::tibble(
    variant = var_col,
    r2 = summary(fit)$r.squared
  ) |>
    left_join(
      broom::tidy(fit) |>
        filter(term != "(Intercept)") |>
        transmute(variant = var_col,
                  predictor = term,
                  beta_std = estimate,
                  p_value = p.value),
      by = "variant"
    )
}

# --- 2) Ajustar todos os modelos por variante ----------------------------
model_tbl <- purrr::map_dfr(variant_cols, fit_variant, arch_vars = arch_vars_present)

# --- 3) Estatística de referência entre variantes ------------------------
ref_tbl <- model_tbl |>
  group_by(predictor) |>
  summarise(
    beta_mean = mean(beta_std, na.rm = TRUE),
    beta_sd   = sd(beta_std,   na.rm = TRUE),
    .groups = "drop"
  )

# --- 4) Z-scores para 605 Peak e 623 Peak (ex.: troque/adicione se precisar)
focus_variants <- c("605 Peak", "623 Peak")

focus_tbl <- model_tbl |>
  filter(variant %in% focus_variants) |>
  left_join(ref_tbl, by = "predictor") |>
  mutate(z_beta = (beta_std - beta_mean) / beta_sd)

readr::write_csv(focus_tbl, "speed_int_outputs/focus_variants_betas_z.csv")
print(focus_tbl |> arrange(variant, desc(abs(z_beta))))

# --- 5) Visual: mapa de calor dos z-scores --------------------------------
if (nrow(focus_tbl) > 0) {
  p_heat <- ggplot(focus_tbl, aes(x = predictor, y = variant, fill = z_beta)) +
    geom_tile(color = "white") +
    scale_fill_gradient2(low = "steelblue", mid = "white", high = "firebrick",
                         midpoint = 0, name = "z(beta)") +
    labs(title = "Sensibilidade relativa por variante (z-score dos coeficientes padronizados)",
         x = "Preditor (microarquitetura)", y = "Variante") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 30, hjust = 1))
  ggsave("speed_int_outputs/focus_variants_beta_z_heatmap.png", p_heat, width = 9, height = 4)
}

# --- 6) Corroboração: dispersão dos insumos para variantes em foco --------
# Boxplots comparando a distribuição dos insumos (ex.: MHz) entre variantes em foco vs. outras
insumos_long <- df |>
  select(all_of(arch_vars_present), all_of(variant_cols)) |>
  pivot_longer(all_of(variant_cols), names_to = "variant", values_to = "Result") |>
  filter(
    variant %in% c(
      focus_variants,
      sample(
        setdiff(variant_cols, focus_variants),
        min(8, length(variant_cols) - length(focus_variants)),
        replace = FALSE
      )
    )
  ) |>
  pivot_longer(all_of(arch_vars_present), names_to = "predictor", values_to = "value")


p_box_insumos <- ggplot(insumos_long, aes(x = variant, y = value)) +
  geom_boxplot(outlier.alpha = 0.3) +
  facet_wrap(~ predictor, scales = "free_y") +
  coord_flip() +
  labs(title = "Dispersão dos insumos por variante (foco vs. comparáveis)",
       x = "Variante", y = "Valor do insumo") +
  theme_minimal()
ggsave("speed_int_outputs/insumos_box_foco_vs_outros.png", p_box_insumos, width = 11, height = 7)

# --- 7) Teste de Levene: variância do insumo no foco vs. outros ----------
levene_checks <- purrr::map_dfr(arch_vars_present, \(pred) {
  tmp <- insumos_long |> filter(predictor == pred)
  tmp <- tmp |> mutate(group = ifelse(variant %in% focus_variants, "foco", "outros"))
  
  # Usando fligner.test (robusto contra desvios de normalidade)
  fl <- fligner.test(value ~ group, data = tmp)
  
  tibble::tibble(
    predictor = pred,
    fligner_stat = unname(fl$statistic),
    fligner_p = unname(fl$p.value)
  )
})

readr::write_csv(levene_checks, "speed_int_outputs/levene_insumos_foco.csv")
print(levene_checks)

# Passo 3: Comparar benchmarks com variáveis de arquitetura --------------
arch_vars <- c("Processor MHz", "1st Level Cache", "2nd Level Cache", "3rd Level Cache", "Memory")
arch_vars_present <- intersect(arch_vars, names(df))

result_col <- if ("Result" %in% names(df)) "Result" else NULL

if (!is.null(result_col) && length(arch_vars_present) > 0) {
  model_df <- df |> 
    dplyr::select(Benchmark, dplyr::any_of(c(result_col, arch_vars_present))) |> 
    dplyr::filter(!is.na(.data[[result_col]]))
  
  # Correlação usando cor()
  corr_tbl <- model_df |>
    dplyr::select(dplyr::any_of(c(result_col, arch_vars_present))) |>
    dplyr::select(where(is.numeric)) |>
    cor(use = "pairwise.complete.obs")
  readr::write_csv(as.data.frame(corr_tbl), "speed_int_outputs/correlations_result_architecture.csv")
  print(corr_tbl)
  
  # Gráficos
  for (v in arch_vars_present) {
    p <- ggplot2::ggplot(model_df, aes(x = .data[[v]], y = .data[[result_col]])) +
      geom_point(alpha = 0.6) +
      geom_smooth(method = "lm", se = TRUE, color = "blue") +
      labs(x = v, y = result_col, title = paste("Result vs", v)) +
      theme_minimal()
    ggsave(file.path("speed_int_outputs", paste0("result_vs_", str_replace_all(v, "[^A-Za-z0-9]", "_"), ".png")), p, width = 7, height = 5)
  }
} else {
  message("Variáveis de arquitetura ou coluna 'Result' não encontradas.")
}
# Definir explicitamente a métrica a ser usada
metric_col <- "Baseline"

# Passo 4: Vendors e boxplots -------------------------------------------
if ("Hardware Vendor" %in% names(df) && metric_col %in% names(df)) {
  vendor_stats <- df |> 
    dplyr::group_by(hardware_vendor = .data$`Hardware Vendor`) |> 
    dplyr::summarise(
      n = dplyr::n(),
      median_baseline = median(.data[[metric_col]], na.rm = TRUE),
      mean_baseline   = mean(.data[[metric_col]], na.rm = TRUE),
      sd_baseline     = sd(.data[[metric_col]], na.rm = TRUE),
      .groups = "drop"
    ) |> 
    dplyr::arrange(desc(median_baseline))
  
  readr::write_csv(vendor_stats, "speed_int_outputs/hardware_vendor_stats.csv")
  print(head(vendor_stats, 10))
  
  df <- df |> 
    dplyr::mutate(hardware_vendor = forcats::fct_reorder(`Hardware Vendor`, .data[[metric_col]], .fun = median, .desc = TRUE))
  
  p_box <- ggplot2::ggplot(df, aes(x = hardware_vendor, y = .data[[metric_col]])) +
    geom_boxplot(outlier.alpha = 0.4) +
    coord_flip() +
    labs(x = "Hardware Vendor (ordenado pela mediana)", y = metric_col, 
         title = "Distribuição da Baseline por Vendor") +
    theme_minimal()
  
  ggsave("speed_int_outputs/vendor_boxplot_baseline.png", p_box, width = 9, height = 8)
}




####################################
#TESTE PARA COMPROVAR ALTA VARIANCIA
####################################

focus_variants <- c("605 Peak", "623 Peak")
arch_vars_focus <- c("Processor MHz", "2nd Level Cache", "3rd Level Cache", "Memory")

for (var_col in focus_variants) {
  for (comp in arch_vars_focus) {
    plot_df <- df |>
      dplyr::select(Benchmark, all_of(var_col), all_of(comp)) |>
      dplyr::rename(Result = !!var_col) |>
      dplyr::filter(!is.na(Result), !is.na(.data[[comp]]))
    
    if (nrow(plot_df) < 5) next
    
    p <- ggplot2::ggplot(plot_df, aes(x = .data[[comp]], y = Result)) +
      geom_point(alpha = 0.6, color = "darkred") +
      geom_smooth(method = "lm", se = TRUE, color = "blue") +
      labs(
        title = paste(var_col, "- sensibilidade a", comp),
        x = comp, y = "Result"
      ) +
      theme_minimal()
    
    ggsave(
      file.path("speed_int_outputs", paste0("scatter_", gsub(" ", "_", var_col), "_", gsub(" ", "_", comp), ".png")),
      p, width = 7, height = 5
    )
  }
}

