noise_widgets <- function(d) {
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
  annual <- d$time[borough == "All NYC",
    .(calls = sum(N), night = sum(N[hour >= 22L | hour < 6L])), by = .(month, type)]
  annual[, rate := calls / tabulate(calendar$month)[month]]
  monthly <- plotly::plot_ly()
  night <- plotly::plot_ly()
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
  ticks <- list(tickvals = 1:12, ticktext = month.abb)
  monthly <- finish(monthly, "b-noise-month", "Month in 2024", "Requests per calendar day", TRUE, bottom = 82, x_options = ticks)
  night <- finish(night, "b-noise-night", "Month in 2024", "Late-night share of requests", TRUE, bottom = 82,
                  x_options = ticks, y_options = list(tickformat = ".0%", range = c(0, 1)))
  list(stack, rates, clock, hour, monthly, night)
}
