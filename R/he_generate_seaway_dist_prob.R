# works on a single value or a vector
he_generate_seaway_dist_prob <- function(dist,
                                         farm_to_farm,
                                         vaccine_efficacy) {
  (exp(-farm_to_farm * dist / 1000) * (1 - vaccine_efficacy)) / 30
}
