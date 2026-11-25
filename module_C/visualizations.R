safety_widgets <- function(d) {
  # C1/C3 need a stable resident denominator; C2 describes all tracts with
  # a measured station distance and must not inherit the NYPD rate filter.
  t <- copy(d$tr[is.finite(nypd_rate) & is.finite(station_m)])
  rail_tracts <- d$tr[is.finite(station_m)]
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
  # Keep every eligible tract in the borough distribution. The log1p display
  # preserves zero rates and leaves outliers visible while axis labels show
  # the original reports-per-1,000 unit.
  rate_boxes <- plotly::plot_ly()
  rate_ticks <- c(0, 10, 25, 50, 100, 200, 500, 1000, 3000)
  for (b in borough_order) {
    cell <- t[borough == b]
    rate_boxes <- plotly::add_trace(rate_boxes, type = "box",
      x = rep(b, nrow(cell)), y = log1p(cell$nypd_rate), name = b, showlegend = FALSE,
      marker = list(color = boro_colors[[b]], size = 4),
      line = list(color = boro_colors[[b]]), fillcolor = boro_colors[[b]],
      boxpoints = "outliers", quartilemethod = "linear")
  }
  rate_boxes <- finish(rate_boxes, "c-report-box", "", "Reports / 1,000 (log scale)",
    bottom = 88, x_options = list(categoryorder = "array", categoryarray = borough_order,
                                  tickangle = -35),
    y_options = list(tickvals = log1p(rate_ticks), ticktext = rate_ticks))
  rail <- geo_map(d, "c-station-map", setNames(rail_tracts$station_m, rail_tracts$GEOID), "Distance (m)",
    setNames(paste0(rail_tracts$label, " · ", show_n(rail_tracts$station_m),
                    " m<br>", rail_tracts$station_name), rail_tracts$GEOID),
    c(0, 400, 800, 1200, 2000, Inf), stations = TRUE)
  cdf <- plotly::plot_ly()
  for (b in borough_order) {
    x <- sort(rail_tracts[borough == b, station_m]) / 1000
    cdf <- plotly::add_lines(cdf, x = c(0, x), y = (0:length(x))/length(x), name = b,
      line = list(color = boro_colors[[b]], width = 2.1, shape = "hv"),
      hovertemplate = "%{x:.2f} km · %{y:.1%}<extra>%{fullData.name}</extra>")
  }
  cdf <- finish(cdf, "c-station-cdf", "Distance to nearest complex (km)", "Cumulative share of tracts",
    TRUE, bottom = 79, y_options = list(tickformat = ".0%", range = c(0, 1)),
    shapes = list(list(type = "line", x0 = .8, x1 = .8, y0 = 0, y1 = 1, yref = "paper",
                       line = list(color = "#9DAFB2", dash = "dash"))))
  # The ranking excludes very low-resident tracts, where a remote point may
  # represent a park, airport or other tract with little rental relevance.
  far <- copy(rail_tracts[is.finite(population) & population >= 500])
  setorder(far, -station_m, GEOID)
  far <- head(far, 10L)
  boro_short <- c(Bronx = "BX", Brooklyn = "BK", Manhattan = "MN",
                  Queens = "QN", `Staten Island` = "SI")
  far[, chart_label := paste0(boro_short[borough], " · ", tract)]
  far <- far[order(station_m)]
  far_rank <- plotly::plot_ly()
  far_rank <- plotly::add_markers(far_rank, x = far$station_m / 1000,
    y = far$chart_label, text = paste0(far$label, "<br>",
      sprintf("%.2f km to %s", far$station_m / 1000, far$station_name)),
    marker = list(color = unname(boro_colors[far$borough]), size = 10),
    hovertemplate = "%{text}<extra></extra>")
  far_rank <- finish(far_rank, "c-far-tracts", "Nearest station distance (km)", "",
    left = 105, bottom = 60, y_options = list(categoryorder = "array",
      categoryarray = far$chart_label, showgrid = FALSE))
  t[, `:=`(distance_km = station_m / 1000, report_log = log1p(nypd_rate))]
  association <- points_by(t, "distance_km", "report_log", "borough", boro_colors,
    paste0(t$label, "<br>", show_n(t$station_m), " m<br>", sprintf("%.1f reports / 1,000", t$nypd_rate)))
  association <- finish(association, "c-rail-report", "Distance to nearest complex (km)",
    "Reports / 1,000 (log scale)", TRUE, bottom = 80,
    y_options = list(tickvals = log1p(rate_ticks), ticktext = rate_ticks))
  ranges <- c("0–399 m", "400–799 m", "800–1,599 m", "1,600 m+")
  t[, distance_band := cut(station_m, c(0, 400, 800, 1600, Inf), labels = ranges, right = FALSE)]
  binned <- t[, .(mid = median(nypd_rate), q1 = as.numeric(quantile(nypd_rate, .25)),
                  q3 = as.numeric(quantile(nypd_rate, .75)), n = .N),
              by = .(borough, distance_band)][n >= 5L]
  groups <- plotly::plot_ly()
  for (b in borough_order) {
    cell <- binned[borough == b][order(distance_band)]
    groups <- plotly::add_markers(groups, x = cell$distance_band, y = cell$mid, name = b,
      marker = list(color = boro_colors[[b]], size = 9),
      error_y = list(type = "data", symmetric = FALSE, array = cell$q3 - cell$mid,
                     arrayminus = cell$mid - cell$q1, color = boro_colors[[b]], thickness = 1),
      customdata = cbind(cell$n, round(cell$q1, 1), round(cell$q3, 1)),
      hovertemplate = paste0("%{x}<br>Median %{y:.1f} per 1,000",
                             "<br>Middle 50%: %{customdata[1]:.1f}–%{customdata[2]:.1f}",
                             "<br>%{customdata[0]} tracts<extra>%{fullData.name}</extra>"))
  }
  groups <- finish(groups, "c-rail-bands", "Distance band", "Median reports per 1,000",
    TRUE, bottom = 79, x_options = list(categoryorder = "array", categoryarray = ranges),
    y_options = list(rangemode = "tozero"))
  list(nypd, severity, rate_boxes, rail, cdf, far_rank, association, groups)
}
