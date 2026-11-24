safety_widgets <- function(d) {
  t <- d$tr[is.finite(nypd_rate)]
  nypd <- geo_map(d, "c-report-map", setNames(t$nypd_rate, t$GEOID), "Reports / 1,000",
    setNames(sprintf("%s · %.1f per 1,000 (%s reports)", t$label, t$nypd_rate, show_n(t$nypd_count)), t$GEOID),
    c(0, 25, 50, 100, 200, Inf))
  law <- copy(d$law)
  law[, share := N/sum(N), by = borough]
  severity <- plotly::plot_ly()
  classes <- c(Felony = clay, Misdemeanor = navy, Violation = teal, `Other/unknown` = "#A6B1B5")
  for (category in names(classes)) {
    sub <- law[law == category][match(borough_order, borough)]
    if (!length(na.omit(sub$share))) next
    severity <- plotly::add_bars(severity, x = sub$borough, y = sub$share, name = category,
      marker = list(color = classes[[category]]), customdata = show_n(sub$N),
      hovertemplate = "%{x}<br>%{y:.1%} · %{customdata} reports<extra>%{fullData.name}</extra>")
  }
  severity <- finish(severity, "c-report-mix", "", "Share of matched NYPD reports", TRUE,
                     bottom = 78, barmode = "stack", y_options = list(tickformat = ".0%"))
  rail <- geo_map(d, "c-station-map", setNames(t$station_m, t$GEOID), "Distance (m)",
    setNames(paste0(t$label, " · ", show_n(t$station_m), " m<br>", t$station_name), t$GEOID),
    c(0, 400, 800, 1200, 2000, Inf), stations = TRUE)
  cdf <- plotly::plot_ly()
  for (b in borough_order) {
    x <- sort(t[borough == b, station_m]) / 1000
    cdf <- plotly::add_lines(cdf, x = c(0, x), y = (0:length(x))/length(x), name = b,
      line = list(color = boro_colors[[b]], width = 2.1, shape = "hv"),
      hovertemplate = "%{x:.2f} km · %{y:.1%}<extra>%{fullData.name}</extra>")
  }
  cdf <- finish(cdf, "c-station-cdf", "Distance to nearest complex (km)", "Cumulative share of tracts",
    TRUE, bottom = 79, y_options = list(tickformat = ".0%", range = c(0, 1)),
    shapes = list(list(type = "line", x0 = .8, x1 = .8, y0 = 0, y1 = 1, yref = "paper",
                       line = list(color = "#9DAFB2", dash = "dash"))))
  t[, `:=`(distance_km = station_m / 1000, report_log = log1p(nypd_rate))]
  association <- points_by(t, "distance_km", "report_log", "borough", boro_colors,
    paste0(t$label, "<br>", show_n(t$station_m), " m<br>", sprintf("%.1f reports / 1,000", t$nypd_rate)))
  rate_ticks <- c(0, 10, 25, 50, 100, 200, 500, 1000, 3000)
  association <- finish(association, "c-rail-report", "Distance to nearest complex (km)",
    "Reports / 1,000 (log scale)", TRUE, bottom = 80,
    y_options = list(tickvals = log1p(rate_ticks), ticktext = rate_ticks))
  ranges <- c("0–399 m", "400–799 m", "800–1,599 m", "1,600 m+")
  t[, distance_band := cut(station_m, c(0, 400, 800, 1600, Inf), labels = ranges, right = FALSE)]
  binned <- t[, .(mid = median(nypd_rate), n = .N), by = .(borough, distance_band)][n >= 5L]
  groups <- plotly::plot_ly()
  for (b in borough_order) {
    cell <- binned[borough == b][order(distance_band)]
    groups <- plotly::add_markers(groups, x = cell$distance_band, y = cell$mid, name = b,
      marker = list(color = boro_colors[[b]], size = 9), customdata = cell$n,
      hovertemplate = "%{x}<br>Median %{y:.1f} per 1,000<br>%{customdata} tracts<extra>%{fullData.name}</extra>")
  }
  groups <- finish(groups, "c-rail-bands", "Distance band", "Median reports per 1,000",
    TRUE, bottom = 79, x_options = list(categoryorder = "array", categoryarray = ranges),
    y_options = list(rangemode = "tozero"))
  list(nypd, severity, rail, cdf, association, groups)
}
