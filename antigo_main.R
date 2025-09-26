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
  ggplot2
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
# Converte descrições de cache em MB somando todos os componentes encontrados
parse_cache <- function(x) {
  x <- tolower(as.character(x))
  # tratar strings vazias/redacted como NA
  x[!is.na(x) & stringr::str_detect(x, "\\bredacted\\b|^\\s*$")] <- NA_character_
  
  m <- stringr::str_match_all(x, "([0-9]+\\.?[0-9]*)\\s*(k|m|g)b")
  
  purrr::map_dbl(m, function(mat) {
    if (is.null(mat) || nrow(mat) == 0) return(NA_real_)
    nums  <- suppressWarnings(as.numeric(mat[, 2]))
    units <- mat[, 3]
    factor <- dplyr::case_when(
      units == "k" ~ 1 / 1024,  # KB -> MB
      units == "m" ~ 1,         # MB -> MB
      units == "g" ~ 1024,      # GB -> MB
      TRUE ~ NA_real_
    )
    sum(nums * factor, na.rm = TRUE)
  })
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
      lvls <- sort(unique(na.omit(as.character(df[[v]]))))
      df[[v]] <- factor(df[[v]], levels = lvls, ordered = TRUE)
    }
  }
}

for (v in discrete_vars) {
  if (v %in% names(df)) df[[v]] <- as.integer(clean_numeric(df[[v]]))
}

# ---- Caches: parse dedicado (em MB) ------------------------------------
cache_cols <- c("1st Level Cache", "2nd Level Cache", "3rd Level Cache", "Other Cache")
for (v in cache_cols) {
  if (v %in% names(df)) df[[v]] <- parse_cache(df[[v]])
}

# ---- Demais contínuas: conversão numérica genérica ---------------------
cont_no_cache <- setdiff(continuous_vars, cache_cols)
for (v in cont_no_cache) {
  if (v %in% names(df)) df[[v]] <- clean_numeric(df[[v]])
}

# Tentar converter outras colunas que parecem numéricas
maybe_numeric <- names(df)[!purrr::map_lgl(df, is.numeric)]
maybe_numeric <- maybe_numeric[purrr::map_lgl(df[maybe_numeric], ~ any(stringr::str_detect(as.character(na.omit(.x)), "[0-9]")))]
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
  
  # Criar pasta para os boxplots
  boxplot_dir <- "speed_int_outputs/boxplots_benchmarks"
  if (!dir.exists(boxplot_dir)) dir.create(boxplot_dir, recursive = TRUE)
  
  # Gerar boxplot para cada benchmark
  benchmarks <- unique(bench_long$Benchmark)
  for (b in benchmarks) {
    plot_df <- bench_long |> dplyr::filter(Benchmark == b, !is.na(value))
    if (nrow(plot_df) == 0) next
    
    p <- ggplot2::ggplot(plot_df, aes(x = variant, y = value, fill = variant)) +
      geom_boxplot(outlier.alpha = 0.4) +
      labs(title = paste("Distribuição de Resultados -", b),
           x = "Variant (Base/Peak)", y = "Valor") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 30, hjust = 1),
            legend.position = "none")
    
    ggsave(file.path(boxplot_dir, paste0("boxplot_", gsub("[^A-Za-z0-9]", "_", b), ".png")),
           p, width = 8, height = 5)
  }
  
} else {
  message("Não foram encontradas colunas numéricas com 'Base' ou 'Peak'.")
}

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
