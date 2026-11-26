# All housing visuals use the same prepared tract table and R summaries.
housing_widgets <- function(d, a) {
  t <- a$rent
  i <- a$income
  b <- a$burden
  contrast_colors <- setNames(c("#A54D39", "#216F70", "#9BABAE"), a$sides)

  # A1. Fixed dollar bins show location, while positions show the relationship.
  rent_tip <- paste0(t$label, "<br>", t$NTA, "<br>Median gross rent: ", show_rent(t$rent),
    "<br>Median household income: ", ifelse(is.finite(t$income), show_income(t$income), "unavailable"))
  rent_map <- geo_map(d, "a-rent-map", setNames(t$rent, t$GEOID), "Monthly gross rent",
                      setNames(rent_tip, t$GEOID), c(0, 1250, 1750, 2250, 2750, Inf))
  # Use geographic labels on the bundled geometry without loading a base map.
  rent_map <- leaflet::addLabelOnlyMarkers(rent_map,
    lng = c(-73.882, -73.954, -73.970, -73.805, -74.155),
    lat = c(40.862, 40.647, 40.783, 40.744, 40.580), label = borough_order,
    labelOptions = leaflet::labelOptions(noHide = TRUE, direction = "center", textOnly = TRUE,
      className = "a-borough-map-label"))
  rent_axis <- list(tickvals = c(500, 1000, 1500, 2000, 2500, 3000, 3501),
                    ticktext = c("$500", "$1,000", "$1,500", "$2,000", "$2,500", "$3,000", "$3,500+"))
  income_axis <- list(tickvals = c(0, 50000, 100000, 150000, 200000, 250001),
                      ticktext = c("$0", "$50k", "$100k", "$150k", "$200k", "$250k+"))
  relation <- plotly::plot_ly()
  for (boro in borough_order) {
    group <- i[borough == boro]
    details <- paste0(group$label, "<br>", group$NTA, "<br>Annual median household income: ",
      show_income(group$income), "<br>Monthly median gross rent: ", show_rent(group$rent))
    relation <- plotly::add_markers(relation, x = group$income, y = group$rent,
      name = boro, text = details, hovertemplate = "%{text}<extra></extra>",
      marker = list(color = boro_colors[[boro]], opacity = .62, size = 6,
        symbol = ifelse(is_top_coded(group$rent) | group$income == 250001, "diamond-open", "circle")))
  }
  relation <- finish(relation, "a-income-rent", "Annual median household income", "Monthly median gross rent",
    TRUE, left = 85, x_options = income_axis, y_options = rent_axis)

  # A2. Use exact empirical steps for the lines and precompute the slider values in R.
  coverage <- plotly::plot_ly()
  for (boro in borough_order) {
    s <- a$coverage_ranges[borough == boro]
    detail <- paste(show_n(s$count), "of", show_n(s$eligible), "tracts")
    coverage <- plotly::add_lines(coverage, x = s$budget, y = s$share, name = boro,
      line = list(color = boro_colors[[boro]], shape = "hv", width = 2.25),
      customdata = detail,
      hovertemplate = "$%{x:,.0f}<br>%{y:.1%} · %{customdata}<extra>%{fullData.name}</extra>")
  }
  budget_line <- function(x) list(type = "line", xref = "x", yref = "paper", x0 = x, x1 = x,
    y0 = 0, y1 = 1, line = list(color = muted, dash = "dot", width = 1.6))
  coverage <- finish(coverage, "a-rent-coverage", "Monthly gross rent ceiling", "Eligible tracts within budget",
    TRUE, left = 79, x_options = list(tickprefix = "$", separatethousands = TRUE, range = c(900, 3525)),
    y_options = list(tickformat = ".0%", range = c(0, 1)), shapes = list(budget_line(a$default_budget)))

  start <- a$coverage[budget == a$default_budget][match(borough_order, borough)]
  budget_bar <- plotly::plot_ly(type = "bar", orientation = "h", x = start$share, y = start$borough,
    marker = list(color = unname(boro_colors[start$borough])), text = show_pct(start$share),
    textposition = "outside", cliponaxis = FALSE, customdata = paste(start$count, "of", start$eligible),
    hovertemplate = "%{y}<br>%{x:.1%} · %{customdata} tracts<extra></extra>")
  budget_bar <- finish(budget_bar, "a-budget-bar", "Eligible tracts within budget", "", left = 106,
    x_options = list(tickformat = ".0%", tickvals = seq(0, 1, .25), range = c(0, 1.18)),
    y_options = list(categoryorder = "array", categoryarray = rev(borough_order)))
  ordered <- a$coverage[order(budget, match(borough, borough_order))]
  by_budget <- split(ordered, ordered$budget)
  rows <- lapply(by_budget, function(s) list(shares = s$share, labels = show_pct(s$share),
    counts = paste(s$count, "of", s$eligible), total = show_n(sum(s$count))))
  budget_bar <- htmlwidgets::onRender(budget_bar,
    paste(readLines("module_A/budget.js", warn = FALSE), collapse = "\n"), rows)

  # A3. Plot actual estimates. The shaded corners come from citywide rank
  # cutoffs, so they show the same eligibility rules as the rank gap chart.
  shown <- b[order(match(contrast, rev(a$sides)))]
  labels <- paste0(shown$label, "<br>", shown$NTA, "<br>Rent: ", show_rent(shown$rent),
    " (", sprintf("%.1f", shown$rent_rank), "th percentile)<br>Burden: ",
    show_pct(shown$burden_rate), " (", sprintf("%.1f", shown$burden_rank), "th percentile)<br>",
    show_n(shown$computable), " computable renter households")
  burden_scatter <- plotly::plot_ly()
  for (category in rev(a$sides)) {
    keep <- which(shown$contrast == category)
    burden_scatter <- plotly::add_markers(burden_scatter, x = shown$rent[keep], y = shown$burden_rate[keep],
      text = labels[keep], name = category, legendrank = match(category, a$sides),
      marker = list(color = contrast_colors[[category]], size = 6,
        opacity = if (category == a$sides[3L]) .42 else .82),
      hovertemplate = "%{text}<extra></extra>")
  }
  outlines <- plotly::add_markers(burden_scatter, x = a$cases$rent, y = a$cases$burden_rate,
    text = paste0(a$cases$label, "<br>", a$cases$NTA, "<br>Median rent: ", show_rent(a$cases$rent), "<br>Rent burden: ",
      show_pct(a$cases$burden_rate)),
    hovertemplate = "%{text}<extra>Case study</extra>",
    marker = list(color = navy, symbol = "circle-open", size = 14, line = list(width = 2)),
    showlegend = FALSE)
  corner <- function(x0, x1, y0, y1, fill) list(type = "rect", x0 = x0, x1 = x1, y0 = y0, y1 = y1,
    layer = "below", line = list(width = 0), fillcolor = grDevices::adjustcolor(fill, .12))
  burden_scatter <- finish(outlines, "a-rent-burden", "Monthly median gross rent", "Rent-burdened renter households",
    TRUE, left = 78, x_options = c(rent_axis, list(range = c(200, 3640))),
    y_options = list(range = c(0, 1), tickformat = ".0%"),
    shapes = list(corner(200, a$rent_bounds[1L], a$burden_bounds[2L], 1, contrast_colors[1L]),
                  corner(a$rent_bounds[2L], 3640, 0, a$burden_bounds[1L], contrast_colors[2L])))

  gap <- a$gaps
  gap_labels <- sub(" tract ", " ", gap$label)
  rank_gap <- plotly::plot_ly(type = "bar", orientation = "h", x = gap$rank_gap, y = gap_labels,
    marker = list(color = unname(contrast_colors[gap$contrast])),
    customdata = paste0(gap$NTA, "<br>Rent: ", show_rent(gap$rent), "<br>Burden: ", show_pct(gap$burden_rate),
      "<br>", show_n(gap$computable), " computable renter households"),
    hovertemplate = "%{y}<br>Gap: %{x:+.1f} percentile points<br>%{customdata}<extra></extra>")
  rank_gap <- finish(rank_gap, "a-rank-gap", "Burden percentile minus rent percentile", "", left = 133,
    x_options = list(range = c(-105, 105), tickvals = seq(-100, 100, 50), zerolinecolor = muted),
    y_options = list(categoryorder = "array", categoryarray = gap_labels))

  cases <- a$case_bands
  band_fields <- c("severe", "moderate", "under_30")
  bands <- c("50% or more", "30% to under 50%", "Below 30%")
  colors <- c("#9D4F3B", "#D79473", "#BBD9D1")
  composition <- plotly::plot_ly()
  # Begin at the severe category, giving both the severe band and the total
  # 30%+ share a common zero baseline across the two tracts.
  for (j in seq_along(bands)) {
    number <- cases[[band_fields[j]]]
    share <- number / cases$computable
    composition <- plotly::add_bars(composition, x = share, y = cases$name, orientation = "h", name = bands[j],
      marker = list(color = colors[j]), text = show_pct(share), textposition = "inside", textangle = 0,
      insidetextfont = list(color = if (j == 1L) "#FFFFFF" else "#173743", size = 12),
      customdata = paste(show_n(number), "of", show_n(cases$computable), "households"),
      hovertemplate = "%{y}<br>%{fullData.name} of income<br>%{x:.1%} · %{customdata}<extra></extra>")
  }
  composition <- finish(composition, "a-case-composition", "Share of renter households", "",
    TRUE, left = 143, x_options = list(tickformat = ".0%", range = c(0, 1)),
    y_options = list(categoryorder = "array", categoryarray = rev(cases$name)), barmode = "stack", bargap = .55,
    uniformtext = list(minsize = 10, mode = "hide"))
  composition <- plotly::layout(composition, legend = list(traceorder = "normal"))
  composition$height <- 255
  list(rent_map = rent_map, income_rent = relation, coverage = coverage, budget_bar = budget_bar,
       burden_scatter = burden_scatter, rank_gap = rank_gap, case_composition = composition)
}
