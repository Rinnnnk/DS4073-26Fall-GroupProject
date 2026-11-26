# A-page denominators are independent of the common sample for Modules D and E.
housing_data <- function(tr, case_file = "data/acs_b25070_cases.csv") {
  stopifnot(!anyNA(tr$GEOID), !anyDuplicated(tr$GEOID))
  rent <- tr[is.finite(rent)]
  borough_rents <- rent[, .(median_rent = median(rent)), by = borough]
  income <- rent[is.finite(income)]
  exact <- income[!is_top_coded(rent) & income != 250001]
  income_stats <- exact[, .(n = .N, rho = cor(income, rent, method = "spearman")), by = borough]

  # Count each eligible tract once per threshold, including those at the threshold.
  budgets <- seq(1000L, 3450L, 50L)
  coverage <- rent[, {
    count <- findInterval(budgets, sort(rent))
    .(budget = budgets, count = count, eligible = .N, share = count / .N)
  }, by = borough]
  coverage_ranges <- rent[, {
    ra <- sort(rent)
    thresholds <- sort(unique(c(min(budgets), ra[ra >= min(budgets) & ra <= max(budgets)], max(budgets))))
    .(budget = thresholds, share = findInterval(thresholds, ra) / .N,
      count = findInterval(thresholds, ra), eligible = .N)
  }, by = borough]

  # Average ranks preserve ties, including the published upper category.
  burden <- rent[!is.na(computable) & computable >= 50 & is.finite(burden_rate)]
  burden[, `:=`(rent_rank = pct_rank(rent), burden_rank = pct_rank(burden_rate))]
  sides <- c("Lower rent / higher burden", "Higher rent / lower burden", "Other eligible tract")
  burden[, contrast := fcase(
    rent_rank <= 25 & burden_rank >= 75, sides[1L],
    rent_rank >= 75 & burden_rank <= 25, sides[2L],
    default = sides[3L]
  )]
  burden[, rank_gap := burden_rank - rent_rank]
  rent_bounds <- c(burden[rent_rank <= 25, max(rent)], burden[rent_rank >= 75, min(rent)])
  burden_bounds <- c(burden[burden_rank <= 25, max(burden_rate)], burden[burden_rank >= 75, min(burden_rate)])
  # Show exact-rent examples only in the ranking, with deterministic tie breaking.
  gaps <- burden[contrast != sides[3L] & !is_top_coded(rent)][order(-abs(rank_gap), GEOID),
    head(.SD, 4L), by = contrast][order(rank_gap)]

  # Compare two uncensored estimates in the same borough, with at least 500
  # computable households in each tract and one example from each corner.
  cases <- burden[match(c("36061021000", "36061002202"), GEOID)]
  stopifnot(!anyNA(cases$GEOID), all(cases$borough == "Manhattan"),
    all(cases$computable >= 500), all(!is_top_coded(cases$rent)), identical(cases$contrast, sides[1:2]))
  case_bands <- housing_case_bands(cases, case_file)

  stopifnot(setequal(rent$borough, borough_order),
    all(coverage$count <= coverage$eligible), all(burden$burden_rate %between% c(0, 1)))
  uncapped <- burden[!is_top_coded(rent)]
  list(rent = rent, borough_rents = borough_rents,
       income = income, income_stats = income_stats, income_exact_n = nrow(exact),
       income_rho = cor(exact$income, exact$rent, method = "spearman"),
       coverage = coverage, coverage_ranges = coverage_ranges, default_budget = 2000L,
       burden = burden, sides = sides, gaps = gaps, cases = cases, case_bands = case_bands,
       rent_bounds = rent_bounds, burden_bounds = burden_bounds,
       burden_rho = cor(burden$rent, burden$burden_rate, method = "spearman"),
       burden_rho_exact = cor(uncapped$rent, uncapped$burden_rate, method = "spearman"))
}

# The bundled two-row extract retains ACS source fields. Pool 002–006 below
# 30%, 007–009 at 30% to under 50%, and 010 at 50% or above. Remove 011 from
# the denominator because the Census could not compute the ratio.
housing_case_bands <- function(cases, path) {
  variables <- sprintf("B25070_E%03d", seq_len(11L))
  raw <- fread(path, colClasses = list(character = "GEO_ID"))
  stopifnot(!anyDuplicated(raw$GEO_ID), setequal(raw$GEO_ID, paste0("1400000US", cases$GEOID)))
  raw <- raw[match(paste0("1400000US", cases$GEOID), GEO_ID)]
  counts <- as.matrix(raw[, ..variables])
  stopifnot(!anyNA(counts), all(counts >= 0))
  bands <- data.table(
    name = cases$NTA,
    under_30 = rowSums(counts[, 2:6, drop = FALSE]),
    moderate = rowSums(counts[, 7:9, drop = FALSE]),
    severe = counts[, 10L],
    computable = counts[, 1L] - counts[, 11L]
  )
  stopifnot(all(bands$under_30 + bands$moderate + bands$severe == bands$computable),
    all(bands$computable == cases$computable),
    isTRUE(all.equal((bands$moderate + bands$severe) / bands$computable, cases$burden_rate)))
  bands
}

# Preserve upper-category labels in tooltips for both ACS measures.
show_income <- function(x) ifelse(x == 250001, "$250,000+", show_dollar(x))
