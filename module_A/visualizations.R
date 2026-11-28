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
  rent_map <- housing_borough_labels(rent_map)
  rent_map <- htmlwidgets::onRender(rent_map,
    paste(readLines("module_A/tract-link.js", warn = FALSE), collapse = "\n"), list(ids = d$shapes$geoid))
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
      name = boro, text = details, customdata = group$GEOID, hovertemplate = "%{text}<extra></extra>",
      marker = list(color = boro_colors[[boro]], opacity = .62, size = 6,
        symbol = ifelse(is_top_coded(group$rent) | group$income == 250001, "diamond-open", "circle")))
  }
  # A dedicated overlay keeps the selected tract visible without changing
  # the borough legend or the upper-category markers in the base traces.
  relation <- plotly::add_markers(relation, x = NA_real_, y = NA_real_,
    name = "Selected tract", showlegend = FALSE, hoverinfo = "skip",
    marker = list(color = "#173743", symbol = "circle-open", size = 15, line = list(width = 2.5)))
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
  start <- a$coverage[budget == a$default_budget][match(borough_order, borough)]
  coverage <- plotly::add_markers(coverage, x = rep(a$default_budget, nrow(start)), y = start$share,
    name = "Selected budget", showlegend = FALSE,
    text = paste0(start$borough, "<br>", show_pct(start$share), " within budget"),
    marker = list(color = unname(boro_colors[start$borough]), size = 8,
      line = list(color = "#FFFFFF", width = 1)),
    hovertemplate = "%{text}<extra></extra>")
  budget_line <- function(x) list(type = "line", xref = "x", yref = "paper", x0 = x, x1 = x,
    y0 = 0, y1 = 1, line = list(color = muted, dash = "dot", width = 1.6))
  coverage <- finish(coverage, "a-rent-coverage", "Monthly gross rent ceiling", "Eligible tracts within budget",
    TRUE, left = 79, x_options = list(tickprefix = "$", separatethousands = TRUE, range = c(900, 3525)),
    y_options = list(tickformat = ".0%", range = c(0, 1)), shapes = list(budget_line(a$default_budget)))
  coverage <- plotly::layout(coverage, legend = list(itemclick = FALSE, itemdoubleclick = FALSE))

  budget_matrix <- housing_budget_matrix(a$coverage, a$default_budget, length(borough_order))

  # A3. The scatterplot corners and the map use the same classifications.
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

  # Map every eligible tract. Keep the two contrasting groups prominent,
  # and retain actual estimates and percentile details in the tooltip.
  map_colors <- contrast_colors
  map_colors[a$sides[3L]] <- "#BDCDC7"
  burden_tip <- paste0(b$label, "<br>", b$NTA, "<br>", b$contrast,
    "<br>Monthly rent: ", show_rent(b$rent), " · rank ", sprintf("%.1f", b$rent_rank),
    "<br>Rent burden: ", show_pct(b$burden_rate), " · rank ", sprintf("%.1f", b$burden_rank),
    "<br>Burden minus rent: ", sprintf("%+.1f", b$rank_gap), " percentile points")
  burden_map <- geo_map(d, "a-burden-map", setNames(b$contrast, b$GEOID), "Rent and burden",
    setNames(burden_tip, b$GEOID), groups = map_colors)
  burden_map <- housing_borough_labels(burden_map)

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
      customdata = paste0(show_n(number), " of ", show_n(cases$computable), " households",
        "<br>Median rent: ", show_rent(a$cases$rent),
        "<br>Total at 30%+: ", show_pct(a$cases$burden_rate)),
      hovertemplate = "%{y}<br>%{fullData.name} of income<br>%{x:.1%} · %{customdata}<extra></extra>")
  }
  totals <- lapply(seq_len(nrow(cases)), function(k) list(
    x = a$cases$burden_rate[k], y = cases$name[k], ax = 0, ay = -31,
    text = paste(show_pct(a$cases$burden_rate[k]), "burdened"), xanchor = "center", showarrow = TRUE,
    arrowhead = 0, arrowwidth = 1, arrowcolor = navy, font = list(size = 11, color = navy)
  ))
  composition <- finish(composition, "a-case-composition", "Share of renter households", "",
    TRUE, left = 143, x_options = list(tickformat = ".0%", range = c(0, 1)),
    y_options = list(categoryorder = "array", categoryarray = rev(cases$name), tickmode = "array",
      tickvals = cases$name, ticktext = paste0(cases$name, "<br>", show_rent(a$cases$rent), " / month")),
    barmode = "stack", bargap = .62, uniformtext = list(minsize = 10, mode = "hide"), annotations = totals)
  composition <- plotly::layout(composition,
    legend = list(traceorder = "normal", itemclick = FALSE, itemdoubleclick = FALSE))
  composition$height <- 255
  list(rent_map = rent_map, income_rent = relation, coverage = coverage, budget_matrix = budget_matrix,
       burden_scatter = burden_scatter, burden_map = burden_map, case_composition = composition)
}

housing_borough_labels <- function(map) {
  leaflet::addLabelOnlyMarkers(map,
    lng = c(-73.882, -73.954, -73.970, -73.805, -74.155),
    lat = c(40.862, 40.647, 40.783, 40.744, 40.580), label = borough_order,
    labelOptions = leaflet::labelOptions(noHide = TRUE, direction = "center", textOnly = TRUE,
      className = "a-borough-map-label"))
}

housing_budget_matrix <- function(coverage, default_budget, marker_trace) {
  # All counts and percentages come from the same R coverage table as the curve.
  ordered <- coverage[order(budget, match(borough, borough_order))]
  by_budget <- split(ordered, ordered$budget)
  rows <- lapply(by_budget, function(s) list(shares = s$share, labels = show_pct(s$share),
    counts = paste(show_n(s$count), "of", show_n(s$eligible), "tracts"),
    total = show_n(sum(s$count)), title = show_dollar(s$budget[1L])))
  available <- as.integer(names(rows))
  # At the lowest or highest selectable budget, use three distinct columns
  # on the available side of the boundary. Keep the selected column marked.
  scenarios <- lapply(available, function(budget) {
    limits <- range(available)
    if (budget == limits[1L]) return(budget + c(0, 250, 500))
    if (budget == limits[2L]) return(budget + c(-500, -250, 0))
    pmin(limits[2L], pmax(limits[1L], budget + c(-250, 0, 250)))
  })
  names(scenarios) <- names(rows)
  first <- scenarios[[as.character(default_budget)]]
  snapshots <- rows[as.character(first)]
  n_boroughs <- length(borough_order)
  z <- vapply(snapshots, function(s) s$shares, numeric(n_boroughs))
  labels <- vapply(snapshots, function(s) s$labels, character(n_boroughs))
  hover <- vapply(snapshots, function(s) {
    paste0(borough_order, " · ", s$title, "<br>", s$labels, "<br>", s$counts)
  }, character(n_boroughs))
  coordinates <- which(matrix(TRUE, n_boroughs, length(first)), arr.ind = TRUE)
  label_cutoff <- .631
  annotations <- lapply(seq_len(nrow(coordinates)), function(k) list(
    x = coordinates[k, "col"] - 1L, y = coordinates[k, "row"] - 1L,
    text = labels[k], showarrow = FALSE, font = list(size = 13, color = if (z[k] >= label_cutoff) "#FFFFFF" else "#000000")
  ))
  selected <- match(default_budget, first) - 1L
  outline <- list(type = "rect", x0 = selected - .48, x1 = selected + .48,
    y0 = -.48, y1 = n_boroughs - .52, line = list(color = "#173743", width = 2))
  palette <- list(list(0, "#F1F8F5"), list(.45, "#9DD0C2"), list(.64, "#327E78"), list(1, "#124A51"))
  figure <- plotly::plot_ly(type = "heatmap", x = 0:2, y = seq_len(n_boroughs) - 1L,
    z = z, zmin = 0, zmax = 1, colorscale = palette, xgap = 3, ygap = 3,
    showscale = FALSE, text = hover, hovertemplate = "%{text}<extra></extra>")
  figure <- finish(figure, "a-budget-matrix", "", "", left = 102, bottom = 42,
    x_options = list(range = c(-.5, 2.5), tickvals = 0:2, showgrid = FALSE, fixedrange = TRUE,
      ticktext = vapply(snapshots, function(s) s$title, character(1L))),
    y_options = list(range = c(n_boroughs - .5, -.5), tickvals = seq_len(n_boroughs) - 1L,
      ticktext = borough_order, showgrid = FALSE, fixedrange = TRUE),
    annotations = annotations, shapes = list(outline))
  htmlwidgets::onRender(figure, paste(readLines("module_A/budget.js", warn = FALSE), collapse = "\n"),
    list(rows = rows, scenarios = scenarios, boroughs = borough_order, markerTrace = marker_trace,
      labelCutoff = label_cutoff))
}
