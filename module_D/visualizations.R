tradeoff_widgets <- function(d) {
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
  h <- p[high_rent == TRUE & !is.na(burden_good)]
  dim_names <- c("Lower rent", "Fewer night reports", "Fewer NYPD reports", "Closer rail", "Lower burden")
  dim_vars <- c("cost", "quiet", "reports", "rail", "burden_good")
  dims <- lapply(seq_along(dim_vars), function(i) list(range = c(0, 100), label = dim_names[i], values = h[[dim_vars[i]]]))
  lines <- plotly::plot_ly(type = "parcoords", dimensions = dims,
    line = list(color = as.integer(h$night_rate >= d$night_cut), cmin = 0, cmax = 1,
                colorscale = list(c(0, "#50917D"), c(1, "#B66656")), showscale = FALSE),
    labelfont = list(size = 11, color = navy), tickfont = list(size = 10))
  lines <- finish(lines, "d-parallel", "", "", left = 28, bottom = 47)
  lines <- plotly::layout(lines, margin = list(l = 29, r = 30, t = 48, b = 42))
  quietbox <- plotly::plot_ly()
  for (b in borough_order) {
    sub <- p[high_rent == TRUE & borough == b]
    quietbox <- plotly::add_boxplot(quietbox, x = rep(b, nrow(sub)), y = sub$night_log, name = b,
      marker = list(color = boro_colors[[b]]), line = list(color = boro_colors[[b]]), boxpoints = "outliers",
      fillcolor = grDevices::adjustcolor(boro_colors[[b]], .23),
      text = sprintf("%.1f requests / 1,000", sub$night_rate), hoverinfo = "text")
  }
  quietbox <- finish(quietbox, "d-luxury-noise", "", "Night requests / 1,000 (log scale)",
     y_options = list(tickvals = log1p(grid_ticks), ticktext = grid_ticks), x_options = list(tickangle = -10))
  examples <- d$examples
  example_colors <- c("#337E70", clay)
  selected <- plotly::plot_ly()
  pattern <- plotly::plot_ly()
  for (i in seq_len(nrow(examples))) {
    row <- examples[i]
    display <- paste0(row$label, " (", show_dollar(row$rent), ")")
    selected <- plotly::add_trace(selected, type = "scatter", mode = "markers", x = unlist(row[, ..dim_vars]),
      y = dim_names, name = display, marker = list(color = example_colors[i], size = 10),
      hovertemplate = "%{y}<br>Percentile %{x:.0f}<extra>%{fullData.name}</extra>")
    observed <- d$tract_hour[GEOID == row$GEOID][data.table(hour = 0:23), on = "hour"]
    observed[is.na(N), N := 0L]
    pattern <- plotly::add_lines(pattern, x = observed$hour, y = 1000 * observed$N/row$population,
      name = display, line = list(color = example_colors[i], width = 2.6),
      hovertemplate = "%{x}:00<br>%{y:.1f} annual requests / 1,000<extra>%{fullData.name}</extra>")
  }
  selected <- finish(selected, "d-example-profile", "Favorable percentile (higher = more favorable)", "",
    TRUE, left = 140, bottom = 94, x_options = list(range = c(0, 100)))
  pattern <- finish(pattern, "d-example-hour", "Local hour", "2024 requests per 1,000 residents",
    TRUE, bottom = 93, x_options = list(dtick = 3), y_options = list(rangemode = "tozero"))
  list(quad, map, lines, quietbox, selected, pattern)
}
