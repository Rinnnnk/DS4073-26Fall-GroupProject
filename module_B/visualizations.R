noise_monthly_summary <- function(time) {
  by_type <- time[borough == "All NYC",
    .(calls = sum(N), night = sum(N[hour >= 22L | hour < 6L])), by = .(month, type)]
  month_days <- tabulate(calendar$month)
  by_type[, rate := calls / month_days[month]]
  overall <- by_type[, .(calls = sum(calls), night = sum(night), rate = sum(rate)), by = month][order(month)]
  overall[, `:=`(night_share = night / calls, night_per_day = night / month_days[month])]
  by_borough <- time[borough %in% borough_order,
    .(calls = sum(N), night = sum(N[hour >= 22L | hour < 6L])), by = .(borough, month)][order(month)]
  by_borough[, `:=`(rate = calls / month_days[month], night_share = night / calls)]
  borough_shift <- by_borough[month %in% 8:9, .(share_change = diff(night_share)), by = borough]

  # Month-over-month changes are additive on the daily-rate scale.
  pair <- overall[month %in% 8:9]
  gain <- diff(pair$rate)
  contributions <- by_type[month %in% 8:9][order(month),
    .(change = diff(rate)), by = type][order(-change)]
  stopifnot(isTRUE(all.equal(sum(contributions$change), gain)))
  list(by_type = by_type, overall = overall, pair = pair, gain = gain,
       by_borough = by_borough, borough_shift = borough_shift,
       residential_share = contributions[type == "Residential", change] / gain)
}

noise_widgets <- function(d, monthly_summary = noise_monthly_summary(d$time)) {
  pop <- d$tr[, .(residents = sum(population, na.rm = TRUE)), by = borough]
  mix <- d$time[borough != "All NYC", .(calls = sum(N)), by = .(borough, type)]
  mix[, share := calls / sum(calls), by = borough]
  mix[pop, rate := i.residents, on = "borough"]
  mix[, rate := calls / rate * 1000]
  stack <- plotly::plot_ly()
  rates <- plotly::plot_ly()
  for (label in noise_types) {
    sub <- mix[type == label][match(borough_order, borough)]
    stack <- plotly::add_bars(stack, x = sub$borough, y = sub$share, name = label,
      marker = list(color = noise_colors[[label]]), customdata = show_n(sub$calls),
      hovertemplate = "%{x}<br>%{y:.1%}<br>%{customdata} requests<extra>%{fullData.name}</extra>")
    rates <- plotly::add_bars(rates, x = sub$borough, y = sub$rate, name = label,
      marker = list(color = noise_colors[[label]]), hovertemplate = "%{x}<br>%{y:.1f} per 1,000<extra>%{fullData.name}</extra>")
  }
  stack <- finish(stack, "b-noise-mix", "", "Share of 311 noise requests", TRUE, bottom = 80,
                  y_options = list(tickformat = ".0%"), barmode = "stack")
  rates <- finish(rates, "b-noise-rate", "", "Requests per 1,000 residents", TRUE, bottom = 80, barmode = "group")
  tracts <- d$tr[is.finite(night_rate)]
  tract_box <- plotly::plot_ly()
  for (borough_name in borough_order) {
    area <- tracts[borough == borough_name]
    color <- boro_colors[[borough_name]]
    tract_box <- plotly::add_boxplot(tract_box, x = area$borough, y = area$night_rate,
      name = borough_name, text = area$label, boxpoints = "outliers",
      marker = list(color = color, size = 4, opacity = .65), line = list(color = color, width = 1.5),
      fillcolor = grDevices::adjustcolor(color, alpha.f = .19))
  }
  short_borough <- c("Bronx", "Brooklyn", "Manhattan", "Queens", "Staten Is.")
  tract_box <- finish(tract_box, "b-noise-tract-box", "", "Late-night requests per 1,000",
    bottom = 70, x_options = list(categoryorder = "array", categoryarray = borough_order,
      tickmode = "array", tickvals = borough_order, ticktext = short_borough, tickangle = -28),
    y_options = list(type = "log", tickvals = c(1, 5, 10, 50, 100, 500, 1000, 5000),
      ticktext = c("1", "5", "10", "50", "100", "500", "1,000", "5,000")))

  initial <- period_data(d, "All types")
  clock <- plotly::plot_ly(type = "heatmap", x = 0:23, y = 1:7,
    z = matrix(initial$rate, nrow = 7L, byrow = TRUE), colorscale = "YlGnBu", reversescale = TRUE, xgap = 1, ygap = 2,
    colorbar = list(title = list(text = "Requests", side = "top"), thickness = 10),
    hovertemplate = "%{x}:00<br>%{z:.1f} per occurrence<extra></extra>")
  clock <- finish(clock, "b-noise-clock", "Local hour", "Day of week",
    x_options = list(tickmode = "linear", dtick = 3, fixedrange = TRUE),
    y_options = list(tickvals = 1:7, ticktext = weekdays_short, range = c(7.5, .5), fixedrange = TRUE))
  hour <- plotly::plot_ly()
  for (period in c("Weekdays", "Weekend", "Selected day")) {
    day_idx <- if (period == "Weekend") 6:7 else 1:5
    weights <- calendar[, .N, by = weekday][match(1:7, weekday), N]
    ag <- initial[weekday %in% day_idx, .(rate = sum(calls) / sum(weights[day_idx])), by = hour][order(hour)]
    hour <- plotly::add_trace(hour, type = "scatter", mode = "lines", x = ag$hour, y = ag$rate, name = period,
      line = list(color = c(Weekdays = navy, Weekend = teal, `Selected day` = clay)[[period]], width = 2.5),
      visible = period != "Selected day",
      hovertemplate = "%{x}:00<br>%{y:.1f} requests<extra>%{fullData.name}</extra>")
  }
  hour <- finish(hour, "b-noise-hour", "Local hour", "Requests per calendar day", TRUE, bottom = 80,
                 x_options = list(dtick = 3), y_options = list(rangemode = "tozero"))
  annual <- monthly_summary$by_type
  overall <- monthly_summary$overall
  pair <- monthly_summary$pair
  gain <- monthly_summary$gain
  monthly <- plotly::plot_ly()
  night <- plotly::plot_ly()
  monthly <- plotly::add_lines(monthly, x = overall$month, y = overall$rate, name = "All requests",
    line = list(color = navy, width = 3.2),
    hovertemplate = "%{y:,.0f} per day<extra>All requests</extra>")
  night <- plotly::add_lines(night, x = overall$month, y = overall$night_share, name = "All requests",
    line = list(color = navy, width = 3.2),
    hovertemplate = "%{y:.1%} at 22:00–05:59<extra>All requests</extra>")
  for (label in noise_types) {
    ag <- annual[type == label][order(month)]
    monthly <- plotly::add_trace(monthly, type = "scatter", mode = "lines+markers", x = ag$month, y = ag$rate,
      name = label, line = list(color = noise_colors[[label]], width = 2.4),
      marker = list(size = 5, color = noise_colors[[label]]),
      hovertemplate = "%{y:.1f} per day<extra>%{fullData.name}</extra>")
    night <- plotly::add_trace(night, type = "scatter", mode = "lines+markers", x = ag$month, y = ag$night/ag$calls,
      name = label, line = list(color = noise_colors[[label]], width = 2.4), marker = list(size = 5, color = noise_colors[[label]]),
      hovertemplate = "%{y:.1%} at 22:00–05:59<extra>%{fullData.name}</extra>")
  }
  monthly <- plotly::add_markers(monthly, x = pair$month, y = pair$rate, name = "Aug–Sep",
    marker = list(color = navy, size = 9, line = list(color = paper, width = 1.5)),
    showlegend = FALSE, hoverinfo = "skip")
  night <- plotly::add_markers(night, x = pair$month, y = pair$night_share, name = "Aug–Sep",
    marker = list(color = navy, size = 9, line = list(color = paper, width = 1.5)),
    showlegend = FALSE, hoverinfo = "skip")

  ticks <- list(type = "linear", tickmode = "array", tickvals = 1:12, ticktext = month.abb,
                range = c(.7, 12.3))
  band <- list(type = "rect", x0 = 7.65, x1 = 9.35, y0 = 0, y1 = 1, yref = "paper",
    fillcolor = "#E8F3EF", layer = "below", line = list(width = 0))
  volume_label <- list(x = 9, y = pair[month == 9, rate], text = paste0("+", show_n(gain), " per day"),
    showarrow = TRUE, arrowcolor = navy, ax = 42, ay = -24, font = list(color = navy, size = 12))
  volume_range <- c(0, max(overall$rate) * 1.17)
  night_labels <- lapply(seq_len(nrow(pair)), function(i) {
    list(x = pair$month[i], y = pair$night_share[i], text = show_pct(pair$night_share[i]),
      showarrow = TRUE, arrowcolor = navy, ax = c(-23, 22)[i], ay = c(-33, 33)[i],
      font = list(color = navy, size = 12), bgcolor = paper, borderpad = 3)
  })

  monthly <- finish(monthly, "b-noise-month", "Month in 2024", "Requests per calendar day", TRUE,
    bottom = 79, x_options = ticks, y_options = list(range = volume_range, separatethousands = TRUE),
    shapes = list(band), annotations = list(volume_label))
  night <- finish(night, "b-noise-night", "Month in 2024", "Late-night share of requests", TRUE, bottom = 82,
    x_options = ticks, y_options = list(tickformat = ".0%", range = c(0, 1)),
    shapes = list(band), annotations = night_labels)

  borough_month <- copy(monthly_summary$by_borough)
  borough_month[pop, daily_per_1000 := 1000 * rate / i.residents, on = "borough"]
  borough_scatter <- plotly::plot_ly()
  for (label in borough_order) {
    area <- borough_month[borough == label]
    focus <- area[month %in% 8:9]
    color <- boro_colors[[label]]
    hover <- "%{text}<br>%{x:.2f} requests/day per 1,000<br>%{y:.1%} late-night<extra></extra>"
    borough_scatter <- plotly::add_markers(borough_scatter, data = area[!(month %in% 8:9)],
      x = ~daily_per_1000, y = ~night_share, text = ~paste(borough, month.name[month]),
      name = label, legendgroup = label, marker = list(color = color, size = 7, opacity = .56),
      hovertemplate = hover)
    borough_scatter <- plotly::add_trace(borough_scatter, data = focus, type = "scatter", mode = "lines+markers",
      x = ~daily_per_1000, y = ~night_share, text = ~paste(borough, month.name[month]),
      name = label, legendgroup = label, showlegend = FALSE,
      marker = list(color = color, size = c(11, 10), symbol = c("circle-open", "diamond")),
      line = list(color = color, width = 2), hovertemplate = hover, inherit = FALSE)
  }
  borough_scatter <- finish(borough_scatter, "b-noise-borough-month",
    "Daily requests per 1,000 residents", "Late-night share", TRUE, bottom = 72,
    x_options = list(rangemode = "tozero"), y_options = list(tickformat = ".0%"))
  list(stack, rates, clock, hour, monthly, night, tract_box, borough_scatter)
}
