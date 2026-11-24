# Derive shared measures from the reproducible tract and time tables.
suppressPackageStartupMessages(library(data.table))
borough_order <- c("Bronx", "Brooklyn", "Manhattan", "Queens", "Staten Island")
boro_colors <- setNames(c("#9A5A3B", "#217B78", "#284B75", "#7261A1", "#9C761E"), borough_order)
noise_types <- c("Residential", "Street/sidewalk", "Commercial", "Vehicle", "Other noise")
noise_colors <- setNames(c("#284B75", "#2E8079", "#9E5D46", "#8673A8", "#A78035"), noise_types)
quad_labels <- c("Lower rent / fewer reports", "Lower rent / more reports",
                 "Higher rent / fewer reports", "Higher rent / more reports")
quad_colors <- setNames(c("#558997", "#A4834B", "#507A5A", "#AD594A"), quad_labels)
cluster_colors <- c("#2E8079", "#325C87", "#AA673C", "#826699")
weekdays_short <- c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")
calendar <- data.table(date = seq(as.Date("2024-01-01"), as.Date("2024-12-31"), by = "day"))
calendar[, month := as.integer(format(date, "%m"))]
calendar[, weekday := (as.POSIXlt(date)$wday + 6L) %% 7L + 1L]
show_n <- function(x) formatC(x, digits = 0, format = "f", big.mark = ",")
show_dollar <- function(x) paste0("$", show_n(x))
is_top_coded <- function(x) x == 3501
show_rent <- function(x) ifelse(is_top_coded(x), "$3,500+", show_dollar(x))
show_pct <- function(x) sprintf("%.1f%%", 100 * x)
pct_rank <- function(x) 100 * (frank(x, ties.method = "average") - 1) / (length(x) - 1)

sample_masks <- function(profile) {
  top_coded <- is_top_coded(profile$rent)
  high_reporting <- profile$night_rate > quantile(profile$night_rate, .99) |
                    profile$nypd_rate > quantile(profile$nypd_rate, .99)
  list(
    "All eligible tracts" = rep(TRUE, nrow(profile)),
    "Exclude $3,500+ rents" = !top_coded,
    "Trim the upper tail of both report rates" = !high_reporting,
    "Apply both exclusions" = !top_coded & !high_reporting
  )
}

fit_profile_groups <- function(z, k) {
  set.seed(4073)
  kmeans(z, centers = k, nstart = 25L, iter.max = 100L)
}

adjusted_rand <- function(reference, refit) {
  # Compare co-membership pairs while correcting for agreement expected by chance.
  counts <- table(reference, refit)
  pairs <- function(n) sum(n * (n - 1) / 2)
  expected <- pairs(rowSums(counts)) * pairs(colSums(counts)) / pairs(sum(counts))
  (pairs(counts) - expected) /
    ((pairs(rowSums(counts)) + pairs(colSums(counts))) / 2 - expected)
}

load_analysis <- function() {
  tr <- fread("data/tracts.csv", colClasses = list(character = c("GEOID", "tract")))
  shapes <- suppressWarnings(sf::st_read("data/tract-boundaries.geojson", quiet = TRUE))
  stopifnot(!anyDuplicated(tr$GEOID), !anyDuplicated(shapes$geoid), setequal(tr$GEOID, shapes$geoid))
  # Both ranks use the same sample and ordering, and retain the original rates.
  burden <- tr[!is.na(rent) & computable >= 50 & !is.na(burden_rate)]
  burden[, rank_gap := pct_rank(burden_rate) - pct_rank(rent)]
  profile <- tr[profile == TRUE]
  rent_cut <- median(profile$rent)
  night_cut <- median(profile$night_rate)
  profile[, quad := quad_labels[1L + as.integer(night_rate >= night_cut) + 2L * as.integer(rent >= rent_cut)]]
  profile[, high_rent := rent >= quantile(rent, .75)]
  favorable <- c("Lower rent", "Fewer night reports", "Fewer NYPD reports", "Closer rail")
  profile[, `:=`(cost = pct_rank(-rent), quiet = pct_rank(-night_rate),
                  reports = pct_rank(-nypd_rate), rail = pct_rank(-station_m))]
  profile[, burden_good := NA_real_]
  profile[!is.na(burden_rate) & computable >= 50, burden_good := pct_rank(-burden_rate)]

  # Select two upper-quartile tracts in the same borough whose rents are
  # within USD 100. Maximize separation between the upper/lower night-rate quartiles.
  high <- profile[high_rent == TRUE & !is_top_coded(rent)]
  lows <- high[night_rate <= quantile(high$night_rate, .25), .(borough, low = GEOID, low_rent = rent, low_night = night_rate)]
  highs <- high[night_rate >= quantile(high$night_rate, .75), .(borough, high = GEOID, high_rent = rent, high_night = night_rate)]
  comparable <- merge(lows, highs, by = "borough", allow.cartesian = TRUE)
  comparable <- comparable[abs(high_rent - low_rent) <= 100]
  comparable[, gap := high_night - low_night]
  exemplar <- comparable[order(-gap, abs(high_rent - low_rent), low, high)][1]
  examples <- profile[match(c(exemplar$low, exemplar$high), GEOID)]
  stopifnot(nrow(examples) == 2L, !anyNA(examples$GEOID))

  # Cluster on separate source measures. Log-transform long-tailed spatial
  # rates and rail distance before applying a common standard deviation scale.
  feature <- with(profile, cbind(pmin(rent, 3500), log1p(night_rate), log1p(nypd_rate), log1p(station_m), log1p(density)))
  colnames(feature) <- c("Gross rent", "Night reports", "NYPD reports", "Rail distance", "Population density")
  z <- scale(feature)
  fit <- fit_profile_groups(z, 4L)
  # Label groups in order of mean rent for deterministic display.
  labels <- order(fit$centers[, 1L])
  ordered_cluster <- match(fit$cluster, labels)
  pca <- prcomp(z, center = FALSE, scale. = FALSE)
  distances <- dist(z)
  silhouette <- cluster::silhouette(ordered_cluster, distances)
  candidate_k <- 2:5
  model_check <- data.table(k = candidate_k, mean_width = vapply(candidate_k, function(k) {
    if (k == 4L) return(mean(silhouette[, "sil_width"]))
    groups <- fit_profile_groups(z, k)$cluster
    mean(cluster::silhouette(groups, distances)[, "sil_width"])
  }, numeric(1)))
  samples <- sample_masks(profile)
  tradeoff_check <- rbindlist(lapply(samples, function(keep) {
    s <- profile[keep]
    data.table(tracts = nrow(s),
      correlation = cor(s$rent, s$night_rate, method = "spearman"),
      high_high = mean(s$rent >= median(s$rent) & s$night_rate >= median(s$night_rate)))
  }), idcol = "sample")
  stability_check <- rbindlist(lapply(samples[-1], function(keep) {
    refit <- fit_profile_groups(scale(feature[keep, , drop = FALSE]), 4L)
    data.table(tracts = sum(keep), agreement = adjusted_rand(fit$cluster[keep], refit$cluster))
  }), idcol = "sample")
  profile[, `:=`(cluster = ordered_cluster, PC1 = pca$x[, 1L], PC2 = pca$x[, 2L],
                  silhouette = silhouette[, "sil_width"])]
  centers <- fit$centers[labels, , drop = FALSE]
  scores <- pca$sdev^2 / sum(pca$sdev^2)
  list(tr = tr, shapes = shapes, stations = fread("data/stations.csv"), burden = burden,
       profile = profile, rent_cut = rent_cut, night_cut = night_cut, favorable = favorable,
       examples = examples, centers = centers, scores = scores, audit = fread("data/join-audit.csv"),
       tradeoff_check = tradeoff_check, model_check = model_check, stability_check = stability_check,
       time = fread("data/noise-time.csv"), tract_hour = fread("data/tract-noise-hour.csv",
                colClasses = list(character = "GEOID")),
       law = fread("data/nypd-law.csv"), source_months = fread("data/source-months.csv"))
}

period_data <- function(d, selected) {
  clock <- d$time[borough == "All NYC" & (selected == "All types" | type == selected)]
  matrix_points <- clock[, .(calls = sum(N)), by = .(weekday, hour)]
  matrix_points <- matrix_points[CJ(weekday = 1:7, hour = 0:23), on = .(weekday, hour)]
  matrix_points[is.na(calls), calls := 0L]
  exposure <- calendar[, .(days = .N), by = weekday][order(weekday)]
  matrix_points[, rate := calls / exposure$days[weekday]]
  matrix_points
}
