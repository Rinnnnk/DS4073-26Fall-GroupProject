# D · Same borough, similar rent.
#
# The page answers one question: among tracts in the same borough whose median
# gross rent differs by no more than $100, how far apart can the late-night
# 311 rate, the NYPD report rate and the distance to rail still be?
#
# Every number on this page comes from the same set of tract pairs, so the
# widgets and the text below them cannot drift apart.

pair_gaps <- function(d, threshold = 100) {
  # Enumerate unordered tract pairs inside one borough and keep the ones whose
  # rents are within `threshold` dollars. Top-coded rents ($3,500+) are removed
  # because their true gap is unknown.
  p <- copy(d$profile)[!is_top_coded(rent) & !is.na(rent) & !is.na(night_rate) &
                         !is.na(nypd_rate) & !is.na(station_m),
                       .(GEOID, borough, label, rent, night_rate, nypd_rate, station_m)]
  left <- copy(p)
  right <- copy(p)
  setnames(left, c("GEOID", "label", "rent", "night_rate", "nypd_rate", "station_m"),
                 c("a", "a_label", "a_rent", "a_night", "a_nypd", "a_station"))
  setnames(right, c("GEOID", "label", "rent", "night_rate", "nypd_rate", "station_m"),
                  c("b", "b_label", "b_rent", "b_night", "b_nypd", "b_station"))
  pairs <- merge(left, right, by = "borough", allow.cartesian = TRUE)
  # One row per unordered pair: keep only a < b so each pair appears once.
  pairs <- pairs[a < b]
  pairs[, rent_gap := abs(a_rent - b_rent)]
  pairs <- pairs[rent_gap <= threshold]
  pairs[, `:=`(night_gap = abs(a_night - b_night),
               nypd_gap = abs(a_nypd - b_nypd),
               station_gap = abs(a_station - b_station))]
  pairs[]
}

ecdf_points <- function(x, n = 180) {
  # A compact empirical CDF on a quantile grid keeps the self-contained HTML small.
  probs <- seq(0.01, 0.99, length.out = n)
  data.table(x = as.numeric(quantile(x, probs, type = 1)), y = probs)
}

pair_thresholds <- function(d, thresholds = c(50, 100, 150)) {
  rbindlist(lapply(thresholds, function(t) {
    p <- pair_gaps(d, t)
    data.table(threshold = t, pairs = nrow(p),
               night_median = median(p$night_gap),
               night_p90 = as.numeric(quantile(p$night_gap, .9)),
               nypd_median = median(p$nypd_gap),
               station_median = median(p$station_gap))
  }))
}

pick_example <- function(p, target_quantile = .9) {
  # Representative, not extreme: the pair whose night-report gap sits closest to
  # the 90th percentile, with a rail-distance gap inside the middle half and both
  # rents in the ordinary range.
  target <- as.numeric(quantile(p$night_gap, target_quantile))
  band <- as.numeric(quantile(p$station_gap, c(.25, .75)))
  cand <- p[rent_gap <= 50 & station_gap >= band[1] & station_gap <= band[2] &
            pmin(a_rent, b_rent) >= 1200 & pmax(a_rent, b_rent) <= 2600]
  cand[order(abs(night_gap - target), a, b)][1]
}

pair_summary <- function(d) {
  p <- pair_gaps(d, 100)
  bands <- pair_thresholds(d)
  example <- pick_example(p)
  pct <- function(v, x) 100 * mean(v <= x)
  ex_rows <- d$profile[match(c(example$a, example$b), GEOID)]
  hour <- d$tract_hour[GEOID %in% ex_rows$GEOID]
  hour <- hour[CJ(GEOID = ex_rows$GEOID, hour = 0:23), on = .(GEOID, hour)]
  hour[is.na(N), N := 0L]
  hour[, rate := 1000 * N / ex_rows$population[match(GEOID, ex_rows$GEOID)]]
  wide <- dcast(hour, hour ~ GEOID, value.var = "rate")
  gap <- abs(wide[[2]] - wide[[3]])
  night_hours <- c(22L, 23L, 0:5)
  list(
    pairs = p, thresholds = bands, example = example, example_rows = ex_rows,
    n_pairs = nrow(p), tracts = uniqueN(c(p$a, p$b)),
    night_median = median(p$night_gap), night_p90 = as.numeric(quantile(p$night_gap, .9)),
    share_large = mean(p$night_gap >= 50), share_25 = mean(p$night_gap >= 25),
    nypd_median = median(p$nypd_gap), station_median = median(p$station_gap),
    station_1km = mean(p$station_gap >= 1000), share_station_500 = mean(p$station_gap >= 500),
    max_pairs_tract = max(table(c(p$a, p$b))),
    example_night_pct = pct(p$night_gap, example$night_gap),
    example_nypd_pct = pct(p$nypd_gap, example$nypd_gap),
    example_station_pct = pct(p$station_gap, example$station_gap),
    example_hour = data.table(hour = wide$hour, gap = gap),
    night_share_of_gap = sum(gap[wide$hour %in% night_hours]) / sum(gap)
  )
}

tradeoff_widgets <- function(d) {
  s <- pair_summary(d)
  p <- copy(d$profile)
  p[, night_log := log1p(night_rate)]
  text <- paste0(p$label, "<br>", show_rent(p$rent), " gross rent<br>",
                 sprintf("%.1f late-night requests per 1,000", p$night_rate))
  quad <- points_by(p, "rent", "night_log", "quad", quad_colors, text)
  grid_ticks <- c(0, 5, 15, 35, 75, 150, 300, 750)
  quad <- finish(quad, "d-quadrant", "Median gross rent ($)", "Night requests / 1,000 (log scale)",
    left = 80, y_options = list(tickvals = log1p(grid_ticks), ticktext = grid_ticks),
    shapes = list(
      list(type = "line", x0 = d$rent_cut, x1 = d$rent_cut, y0 = 0, y1 = 1, yref = "paper",
           line = list(color = muted, dash = "dash", width = 1)),
      list(type = "line", y0 = log1p(d$night_cut), y1 = log1p(d$night_cut), x0 = 0, x1 = 1, xref = "paper",
           line = list(color = muted, dash = "dash", width = 1))))
  map <- geo_map(d, "d-quad-map", setNames(p$quad, p$GEOID), "Above/below NYC medians",
                 setNames(paste0(text, "<br>", p$quad), p$GEOID), groups = quad_colors, linked = "d-quadrant")

  # D2 chart 1: how wide are the three gaps among same-borough, similar-rent pairs.
  # Requests per 1,000 and metres cannot share a raw axis, so each gap is divided
  # by that indicator's own median. 1x is then "the typical gap" for all three and
  # the curves can be compared on shape alone; the legend keeps the raw medians.
  measures <- list(
    list(name = "Night requests / 1,000", unit = "", values = s$pairs$night_gap,
         color = teal, raw = sprintf("%.1f", s$night_median)),
    list(name = "NYPD reports / 1,000", unit = "", values = s$pairs$nypd_gap,
         color = clay, raw = sprintf("%.1f", s$nypd_median)),
    list(name = "Rail distance (m)", unit = "", values = s$pairs$station_gap,
         color = navy, raw = sprintf("%.0f", s$station_median)))
  gaps <- plotly::plot_ly()
  for (m in measures) {
    med <- median(m$values)
    pts <- ecdf_points(m$values)
    pts[, x := pmax(x / med, 0.02)]
    gaps <- plotly::add_lines(gaps, x = pts$x, y = pts$y,
      name = m$name,
      line = list(color = m$color, width = 2.4),
      hovertemplate = paste0(m$name, "<br>%{x:.1f}x the median (", m$raw, m$unit,
                             ")<br>%{y:.0%} of pairs<extra></extra>"))
  }
  gaps <- finish(gaps, "d-pair-gaps", "Gap within the pair, relative to the typical gap",
    "Share of pairs at or below", TRUE, left = 84, bottom = 74,
    x_options = list(type = "log", range = c(-1.8, 2.2)),
    shapes = list(list(type = "line", x0 = 1, x1 = 1, y0 = 0, y1 = 1, yref = "paper",
      line = list(color = muted, dash = "dot", width = 1.2))))

  # D2 chart 2: does moving the rent window change the answer.
  stable <- plotly::plot_ly()
  shades <- setNames(c("#9CC3BE", "#2E8079", "#173F52"), c(50, 100, 150))
  for (t in c(50, 100, 150)) {
    pts <- ecdf_points(pair_gaps(d, t)$night_gap)
    pts[, x := pmax(x, 0.05)]
    stable <- plotly::add_lines(stable, x = pts$x, y = pts$y,
      name = paste0("$", t, " window"),
      line = list(color = shades[[as.character(t)]], width = 2.4),
      hovertemplate = paste0("$", t, "<br>%{x:.1f} requests / 1,000<br>%{y:.0%} of pairs<extra></extra>"))
  }
  stable <- finish(stable, "d-pair-threshold", "Night-report gap within the pair (log scale)",
    "Share of pairs at or below", TRUE, left = 84, bottom = 74,
    x_options = list(type = "log", range = c(-0.6, 3.2)))

  # D3 chart 1: put the chosen pair back into the full distribution.
  base <- ecdf_points(s$pairs$night_gap)
  base[, x := pmax(x, 0.05)]
  rank <- plotly::add_lines(plotly::plot_ly(), x = base$x, y = base$y, name = "All qualifying pairs",
    line = list(color = teal, width = 2.4), hoverinfo = "skip")
  rank <- plotly::add_markers(rank, x = s$example$night_gap, y = s$example_night_pct / 100,
    name = "Illustrative pair", marker = list(color = clay, size = 13, symbol = "diamond",
    line = list(color = "#FFFFFF", width = 1.5)),
    hovertemplate = paste0(s$example$a, " vs ", s$example$b, "<br>gap %{x:.1f}<br>%{y:.0%} percentile<extra></extra>"))
  rank <- finish(rank, "d-pair-rank", "Night-report gap within the pair (log scale)",
    "Share of pairs at or below", TRUE, left = 84, bottom = 74,
    x_options = list(type = "log", range = c(-0.6, 3.2)),
    shapes = list(list(type = "line", x0 = s$example$night_gap, x1 = s$example$night_gap,
      y0 = 0, y1 = s$example_night_pct / 100, line = list(color = clay, dash = "dot", width = 1.4))),
    annotations = list(list(x = s$example$night_gap, y = s$example_night_pct / 100,
      text = sprintf("%.0fth percentile", s$example_night_pct), showarrow = FALSE,
      xanchor = "left", yanchor = "bottom", xshift = 8, font = list(size = 11, color = clay))))

  # D3 chart 2: where in the day the two tracts actually differ.
  rows <- s$example_rows
  hour <- d$tract_hour[GEOID %in% rows$GEOID]
  hour <- hour[CJ(GEOID = rows$GEOID, hour = 0:23), on = .(GEOID, hour)]
  hour[is.na(N), N := 0L]
  hour[, rate := 1000 * N / rows$population[match(GEOID, rows$GEOID)]]
  colors <- c("#50917D", clay)
  clock <- plotly::plot_ly()
  for (i in seq_len(nrow(rows))) {
    sub <- hour[GEOID == rows$GEOID[i]][order(hour)]
    clock <- plotly::add_lines(clock, x = sub$hour, y = sub$rate,
      name = paste0(rows$label[i], " · ", show_rent(rows$rent[i])),
      line = list(color = colors[i], width = 2.6),
      hovertemplate = paste0(rows$label[i], "<br>%{x}:00<br>%{y:.1f} requests per 1,000<extra></extra>"))
  }
  clock <- finish(clock, "d-example-hour", "Local hour", "2024 late-night requests per 1,000 residents",
    TRUE, bottom = 93, x_options = list(dtick = 3), y_options = list(rangemode = "tozero"),
    shapes = list(
      list(type = "rect", x0 = 0, x1 = 6, y0 = 0, y1 = 1, yref = "paper", line = list(width = 0),
           fillcolor = "rgba(35,72,95,0.07)", layer = "below"),
      list(type = "rect", x0 = 22, x1 = 24, y0 = 0, y1 = 1, yref = "paper", line = list(width = 0),
           fillcolor = "rgba(35,72,95,0.07)", layer = "below")),
    annotations = list(list(x = 14, y = 1.02, yref = "paper", text = "Shaded: 22:00-05:59",
      showarrow = FALSE, font = list(size = 10.5, color = muted))))
  out <- list(quad, map, gaps, stable, rank, clock)
  attr(out, "summary") <- s
  out
}
