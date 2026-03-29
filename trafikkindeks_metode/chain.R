# Chain drift ----
# point_index_chain <-
#   tibble::tibble(
#     base_volume = c(
#       100,
#       200,
#       210,
#       200,
#       150,
#       100,
#       50
#     ),
#     calc_volume = c(
#       200,
#       210,
#       200,
#       150,
#       100,
#       50,
#       100
#     )
#   ) |>
#   dplyr::mutate(
#     base_volume = 100 * base_volume,
#     calc_volume = 100 * calc_volume
#   ) |>
#   dplyr::mutate(
#     pi_i = calc_volume / base_volume,
#     pi_i_chain = 100 * cumprod(pi_i),
#     pi_p = 100 * (calc_volume / base_volume - 1),
#     pi_i_ln = 100 * log(pi_i),
#     pi_i_ln_chain = cumsum(pi_i_ln),
#     f_pi_i_lambda_half = (calc_volume - base_volume) / sqrt(base_volume),
#     F_pi_i_lambda_half = 2 * (sqrt(calc_volume) - sqrt(base_volume)),
#     F_pi_i_lambda_half_chain = cumsum(F_pi_i_lambda_half)
#   )

# Should return to zero change
# Additivity
# sum(point_index_chain$pi_i_ln) # yes
# sum(point_index_chain$f_pi_i_lambda_half) # no
# sum(point_index_chain$F_pi_i_lambda_half) # yes

# Multiplicity
# prod(point_index_chain$pi_i) # yes

# Chain drift is not a problem unless the selection or road network changes,
# but then this would be hard to prove anyway.


# Visualise chained index ----
library(tidygraph)
library(ggraph)

# Need example nodes and links
graph_nj <-
  tibble::tribble(
    ~from, ~to, ~label,
    "2018", "2017", "<",
    "2019", "2017", "<",
    "2020", "2019", "<",
    "2021", "2019", "<",
    "2022", "2019", "<",
    "2023", "2019", "<",
    "2024", "2023", "<",
    "2025", "2023", "<",
    "2026", "2023", "<"
  ) |> 
  tidygraph::as_tbl_graph()

graph_tromso <-
  tibble::tribble(
    ~from, ~to, ~label,
    "2020", "2019", "<",
    "2021", "2019", "<",
    "2022", "2019", "<",
    "2023", "2022", "<",
    "2024", "2022", "<",
    "2025", "2022", "<",
    "2026", "2022", "<"
  ) |> 
  tidygraph::as_tbl_graph()

graph_vti <-
  tibble::tribble(
    ~from, ~to, ~label,
    "2020", "2019", "<",
    "2021", "2020", "<",
    "2022", "2021", "<",
    "2023", "2022", "<",
    "2024", "2023", "<",
    "2025", "2024", "<",
    "2026", "2025", "<"
  ) |> 
  tidygraph::as_tbl_graph()

plot_index_chain_graph <- function(graph_object) {

  graph_object |> 
    ggraph::ggraph(layout = "linear", sort.by = name)  +
    geom_edge_arc(aes(label = label), label_size = 6, color = "#ed9300") +
    geom_node_point() +
    geom_node_label(aes(label = name))

}

# plot_index_chain_graph(graph_nj)
# plot_index_chain_graph(graph_tromso)
# plot_index_chain_graph(graph_vti)
