housing_widgets <- function(d) {
  t <- d$tr[!is.na(rent)]
  b <- d$burden
  income <- t[is.finite(income)]
  rent_map <- geo_map(d, "a-rent-map", setNames(t$rent, t$GEOID), "Gross rent ($)",
                      setNames(paste(t$label, show_rent(t$rent)), t$GEOID), c(0, 1250, 1750, 2250, 2750, Inf))
  relation <- points_by(income, "income", "rent", "borough", boro_colors,
    paste0(income$label, "<br>Median income: ", show_dollar(income$income), "<br>Gross rent: ", show_rent(income$rent)))
  relation <- finish(relation, "a-income-rent", "Median household income ($)", "Median gross rent ($)", TRUE)
  box <- plotly::plot_ly()
  for (borough_name in borough_order) {
    group <- t[borough == borough_name]
    box <- plotly::add_boxplot(box, y = group$rent, x = rep(borough_name, nrow(group)),
      name = borough_name, boxpoints = "outliers", marker = list(color = boro_colors[[borough_name]]),
      line = list(color = boro_colors[[borough_name]]), fillcolor = grDevices::adjustcolor(boro_colors[[borough_name]], .23))
  }
  box <- finish(box, "a-rent-box", "", "Median gross rent ($)", x_options = list(tickangle = -12))
  coverage <- plotly::plot_ly()
  budgets <- seq(500, 3450, by = 50)
  for (borough_name in borough_order) {
    rents <- sort(t[borough == borough_name, rent])
    counts <- findInterval(budgets, rents)
    coverage <- plotly::add_lines(coverage, x = budgets, y = counts / length(rents),
      name = borough_name, line = list(color = boro_colors[[borough_name]], shape = "hv", width = 2),
      customdata = paste(show_n(counts), "of", show_n(length(rents)), "tracts"),
      hovertemplate = "%{y:.1%} · %{customdata}<extra>%{fullData.name}</extra>")
  }
  coverage <- finish(coverage, "a-rent-coverage", "Monthly rent threshold ($)", "Share of tracts at or below threshold",
    TRUE, left = 80, hovermode = "x unified",
    y_options = list(tickformat = ".0%", range = c(0, 1)))
  burden <- points_by(b, "rent", "burden_rate", "borough", boro_colors,
    paste0(b$label, "<br>", show_rent(b$rent), " gross rent<br>", show_pct(b$burden_rate),
           " at 30%+ of income<br>", show_n(b$computable), " computable renter households"))
  burden <- finish(burden, "a-rent-burden", "Median gross rent ($)", "Rent-burdened renter households", TRUE,
                   y_options = list(tickformat = ".0%"))
  gap <- rbind(b[order(-rank_gap)][1:5], b[order(rank_gap)][1:5])[order(rank_gap)]
  percentiles <- plotly::plot_ly(type = "bar", orientation = "h", x = gap$rank_gap, y = gap$label,
    marker = list(color = ifelse(gap$rank_gap > 0, clay, teal)),
    customdata = paste0("Rent ", show_rent(gap$rent), "<br>Burden ", show_pct(gap$burden_rate)),
    hovertemplate = "%{y}<br>Gap: %{x:+.1f} points<br>%{customdata}<extra></extra>")
  percentiles <- finish(percentiles, "a-rank-gap", "Burden percentile minus rent percentile", "", left = 180,
                        y_options = list(categoryorder = "array", categoryarray = gap$label))
  list(rent_map, relation, box, coverage, burden, percentiles)
}
