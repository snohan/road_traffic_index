# Chain drift ----
point_index_chain <-
  tibble::tibble(
    base_volume = c(
      100,
      200,
      210,
      200,
      150,
      100,
      50
    ),
    calc_volume = c(
      200,
      210,
      200,
      150,
      100,
      50,
      100
    )
  ) |>
  dplyr::mutate(
    base_volume = 100 * base_volume,
    calc_volume = 100 * calc_volume
  ) |>
  dplyr::mutate(
    pi_i = calc_volume / base_volume,
    pi_i_chain = 100 * cumprod(pi_i),
    pi_p = 100 * (calc_volume / base_volume - 1),
    pi_i_ln = 100 * log(pi_i),
    pi_i_ln_chain = cumsum(pi_i_ln),
    f_pi_i_lambda_half = (calc_volume - base_volume) / sqrt(base_volume),
    F_pi_i_lambda_half = 2 * (sqrt(calc_volume) - sqrt(base_volume)),
    F_pi_i_lambda_half_chain = cumsum(F_pi_i_lambda_half)
  )

# Should return to zero change
# Additivity
sum(point_index_chain$pi_i_ln) # yes
sum(point_index_chain$f_pi_i_lambda_half) # no
sum(point_index_chain$F_pi_i_lambda_half) # yes

# Multiplicity
prod(point_index_chain$pi_i) # yes

# Chain drift is not a problem unless the selection or road network changes,
# but then this would be hard to prove anyway.