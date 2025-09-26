# Se já tem 'focus_tbl' na sessão, pode pular esta leitura:
focus_tbl <- readr::read_csv("speed_int_outputs/focus_variants_betas_z.csv", show_col_types = FALSE)

# Ordenações para leitura mais fácil
pred_order <- focus_tbl |>
  dplyr::group_by(predictor) |>
  dplyr::summarise(mabs = mean(abs(z_beta), na.rm = TRUE), .groups = "drop") |>
  dplyr::arrange(dplyr::desc(mabs)) |>
  dplyr::pull(predictor)

var_order <- focus_tbl |>
  dplyr::group_by(variant) |>
  dplyr::summarise(score = sqrt(sum(z_beta^2, na.rm = TRUE)), .groups = "drop") |>
  dplyr::arrange(dplyr::desc(score)) |>
  dplyr::pull(variant)

focus_tbl <- focus_tbl |>
  dplyr::mutate(
    predictor = factor(predictor, levels = pred_order),
    variant   = factor(variant,   levels = var_order),
    sig       = abs(z_beta) >= 2     # regra prática: efeito anormal
  )

p_heat <- ggplot2::ggplot(focus_tbl, ggplot2::aes(x = predictor, y = variant, fill = z_beta)) +
  ggplot2::geom_tile(color = "white") +
  ggplot2::scale_fill_gradient2(
    low = "steelblue", mid = "white", high = "firebrick",
    midpoint = 0, name = "z(beta)"
  ) +
  # marca visual nas células “anormais”
  ggplot2::geom_point(data = dplyr::filter(focus_tbl, sig), shape = 21, size = 3,
                      fill = NA, color = "black", stroke = 1) +
  ggplot2::labs(
    title = "Sensibilidade relativa (z-score) por variante em foco",
    x = "Microarquitetura", y = "Variante"
  ) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 30, hjust = 1))

ggplot2::ggsave("speed_int_outputs/focus_variants_beta_z_heatmap.png", p_heat, width = 10, height = 4)



