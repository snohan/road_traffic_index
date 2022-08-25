base::Sys.setlocale(locale = "nb.utf8")

source("H:/Programmering/R/byindeks/get_from_trafficdata_api.R")
source("H:/Programmering/R/byindeks/split_road_system_reference.R")


# City data ----
# Choose
index_year <- 2022
reference_year <- 2018
index_month <- 7 # latest published
city_number <- 8952

# Fetch
city_index <-
  get_published_index_for_months(city_number, index_year, index_month)
pointindex <-
  get_published_pointindex_for_months(city_number, index_year, index_month)

city_trps <- pointindex[[1]]
pointindices <- pointindex[[2]]
city_name <- city_index$area_name[1]

# TRP data ----
# Point metadata from Traffic Data API
points <-
  get_points() %>%
  dplyr::distinct(
    trp_id,
    .keep_all = T
  ) %>%
  dplyr::select(
    trp_id,
    name,
    road_reference,
    county_name,
    municipality_name,
    lat, lon, road_link_position
  ) %>%
  dplyr::mutate(
    name = stringr::str_to_title(name, locale = "no")
  )

city_trps_meta <-
  points %>%
  dplyr::filter(
    trp_id %in% city_trps
  ) %>%
  split_road_system_reference() %>%
  dplyr::select(
    trp_id,
    name,
    road_reference,
    road_category_and_number,
    #section_number, subsection_number, meter,
    #intersection_part_number, intersection_meter,
    county_name, municipality_name,
    lat, lon
  )

# AADT ----
adt <- get_aadt_by_length_for_trp_list(city_trps_meta$trp_id)

adt_filtered <-
  adt %>%
  dplyr::filter(
    length_range == "[..,5.6)",
    year >= reference_year
  ) %>%
  dplyr::mutate(
    length_quality = round(aadt_valid_length / aadt_total * 100)
  ) %>%
  #dplyr::filter(length_quality > 90) %>%
  #dplyr::filter(coverage > 50) %>%
  dplyr::select(
    trp_id,
    year,
    aadt = aadt_length_range,
    coverage,
    length_quality
    )

city_aadts <-
  adt_filtered |>
  dplyr::left_join(
    city_trps_meta,
    by = "trp_id"
  )

base::saveRDS(
  city_aadts,
  "aadt.rds"
)


city_aadts <-
  base::readRDS(
    "aadt.rds"
  )

# Index by AADT ----

filter_aadt <- function(aadt_df, year_dbl) {

  aadt_df |>
    dplyr::filter(
      year == year_dbl,
      coverage >= 90,
      length_quality > 99
    ) |>
    dplyr::select(
      trp_id,
      year,
      aadt
    )

}


calculate_bilateral_index <- function(base_year, calc_year, aadt_df) {

  index_df <-
    dplyr::inner_join(
      filter_aadt(aadt_df, base_year),
      filter_aadt(aadt_df, calc_year),
      by = "trp_id"
    ) |>
    dplyr::summarise(
      index_i = sum(aadt.y) / sum(aadt.x),
      index_p = (index_i - 1) * 100,
      n_trp = n()
    ) |>
    dplyr::mutate(
      index_period =
        paste0(
          base_year,
          "-",
          calc_year
        )
    )

}


## Bilateral indices ----
indices <-
  dplyr::bind_rows(
    calculate_bilateral_index(2018, 2019, city_aadts),
    calculate_bilateral_index(2019, 2020, city_aadts),
    calculate_bilateral_index(2020, 2021, city_aadts),
    calculate_bilateral_index(2018, 2021, city_aadts),
    calculate_bilateral_index(2021, 2018, city_aadts)
  )

base::saveRDS(
  indices,
  "indices.rds"
)


## Rolling indices ----

# aadt_df <- city_aadts
# window_length <- 3
# base_year <- 2018

calculate_rolling_indices_by_aadt <-
  function(base_year, last_year, window_length, aadt_df) {

  mean_aadt <-
    aadt_df |>
    dplyr::filter(
      year %in% seq.int(last_year - window_length + 1, last_year)
    ) |>
    dplyr::filter(
      coverage >= 90,
      length_quality > 99
    ) |>
    dplyr::group_by(
      trp_id
    ) |>
    dplyr::summarise(
      n_years = n(),
      mean_aadt = base::mean(aadt)
    ) |>
    dplyr::filter(
      n_years == window_length
    )

  index_df <-
    dplyr::inner_join(
      filter_aadt(aadt_df, base_year),
      mean_aadt,
      by = "trp_id"
    ) |>
    dplyr::summarise(
      index_i = sum(mean_aadt) / sum(aadt),
      index_p = (index_i - 1) * 100,
      n_trp = n()
    ) |>
    dplyr::mutate(
      index_period =
        paste0(
          base_year,
          "-(",
          last_year - window_length + 1,
          "-",
          last_year,
          ")"
        )
    )

}

bergen_rolling <-
  calculate_rolling_indices_by_aadt(
    2018,
    2021,
    3,
    city_aadts
  )


# MDT ----
mdt <-
  get_mdt_by_length_for_trp_list(city_trps_meta$trp_id, 2018)

mdt_filtered <-
  mdt %>%
  dplyr::filter(
    length_range == "[..,5.6)"
  ) %>%
  dplyr::mutate(
    length_quality = round(mdt_valid_length / mdt_total * 100)
  ) %>%
  #dplyr::filter(length_quality > 90) %>%
  #dplyr::filter(coverage > 50) %>%
  dplyr::select(
    trp_id,
    year,
    month,
    mdt = mdt_length_range,
    coverage,
    length_quality
  )

city_mdts <-
  mdt_filtered |>
  dplyr::left_join(
    city_trps_meta,
    by = "trp_id"
  )

base::saveRDS(
  city_mdts,
  "mdt.rds"
)


city_mdts <-
  base::readRDS(
    "mdt.rds"
  )

# Index by MDT ----

filter_mdt <- function(mdt_df, year_dbl) {

  mdt_df |>
    dplyr::filter(
      year == year_dbl,
      coverage >= 90,
      length_quality > 99
    ) |>
    dplyr::select(
      trp_id,
      year,
      month,
      mdt
    ) |>
    dplyr::group_by(
      trp_id
    ) |>
    dplyr::summarise(
      n_months = n()
    ) |>
    dplyr::filter(
      n_months == 12
    )

}

mdt_2018 <-
  filter_mdt(city_mdts, 2018)


## Rolling indices ----

# aadt_df <- city_aadts
# window_length <- 3
# base_year <- 2018

calculate_rolling_indices_by_mdt <-
  function(base_year, last_year_month, window_length, mdt_df) {

    mean_aadt <-
      aadt_df |>
      dplyr::filter(
        year %in% seq.int(last_year - window_length + 1, last_year)
      ) |>
      dplyr::filter(
        coverage >= 90,
        length_quality > 99
      ) |>
      dplyr::group_by(
        trp_id
      ) |>
      dplyr::summarise(
        n_years = n(),
        mean_aadt = base::mean(aadt)
      ) |>
      dplyr::filter(
        n_years == window_length
      )

    index_df <-
      dplyr::inner_join(
        filter_aadt(aadt_df, base_year),
        mean_aadt,
        by = "trp_id"
      ) |>
      dplyr::summarise(
        index_i = sum(mean_aadt) / sum(aadt),
        index_p = (index_i - 1) * 100,
        n_trp = n()
      ) |>
      dplyr::mutate(
        index_period =
          paste0(
            base_year,
            "-(",
            last_year - window_length + 1,
            "-",
            last_year,
            ")"
          )
      )

  }