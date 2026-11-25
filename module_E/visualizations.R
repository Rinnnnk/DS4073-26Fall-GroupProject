# Module E: Multi-dimensional tract profiles and same-budget preference turnover.
# Author: Weiye Xu (Student ID: 2430026242)

profile_names <- c(
  "Profile 1: Budget Rail-Close",
  "Profile 2: Quiet Outer-Rail",
  "Profile 3: Quiet Rail-Access",
  "Profile 4: High-Rent Core"
)

profile_widgets <- function(d) {
  p <- copy(d$profile)
  p[, group := factor(profile_names[cluster], levels = profile_names)]
  groups <- setNames(cluster_colors, profile_names)

  # --- E1.1: Interactive Leaflet Choropleth with Layer Control & Popups -------
  detail_tip <- paste0(
    p$label, "<br>", p$group, "<br>",
    show_rent(p$rent), " gross rent | ", show_n(p$station_m), " m to rail"
  )
  detail_pop <- paste0(
    "<strong>", htmltools::htmlEscape(p$label), "</strong><br>",
    "GEOID: ", p$GEOID, " | NTA: ", htmltools::htmlEscape(p$NTA), "<br>",
    "<strong>", p$group, "</strong><br>",
    "Median gross rent: ", show_rent(p$rent), "<br>",
    "Nighttime noise: ", sprintf("%.1f", p$night_rate), " / 1,000<br>",
    "NYPD complaints: ", sprintf("%.1f", p$nypd_rate), " / 1,000<br>",
    "Nearest rail: ", show_n(p$station_m), " m (",
    htmltools::htmlEscape(p$station_name), ")"
  )

  shapes <- d$shapes
  idx <- match(shapes$geoid, p$GEOID)
  shapes$group_label <- ifelse(is.na(idx), "Excluded", as.character(p$group[idx]))
  shapes$tip_html <- ifelse(
    is.na(idx),
    paste0(d$tr$label[match(shapes$geoid, d$tr$GEOID)], " - missing or excluded"),
    detail_tip[idx]
  )
  shapes$pop_html <- ifelse(
    is.na(idx),
    paste0("<strong>", htmltools::htmlEscape(d$tr$label[match(shapes$geoid, d$tr$GEOID)]),
           "</strong><br>Excluded from joint profile sample (&lt;500 residents or missing rent)."),
    detail_pop[idx]
  )

  pal <- leaflet::colorFactor(unname(groups), levels = profile_names, na.color = "#E4E8E5")
  map <- leaflet::leaflet(
    shapes,
    options = leaflet::leafletOptions(preferCanvas = TRUE, zoomSnap = 0.25),
    height = 344
  )

  bg <- shapes[shapes$group_label == "Excluded", ]
  if (nrow(bg) > 0L) {
    bg_tips <- lapply(bg$tip_html, htmltools::HTML)
    map <- leaflet::addPolygons(
      map, data = bg, layerId = bg$geoid, group = "Excluded tracts",
      fillColor = "#E4E8E5", fillOpacity = 0.65, color = "#FFFFFF", weight = 0.5,
      label = bg_tips,
      labelOptions = leaflet::labelOptions(direction = "auto", className = "nyc-tooltip"),
      popup = bg$pop_html
    )
  }

  for (gname in profile_names) {
    sub_sf <- shapes[shapes$group_label == gname, ]
    if (nrow(sub_sf) == 0L) next
    sub_tips <- lapply(strsplit(sub_sf$tip_html, "<br>", fixed = TRUE), function(lines) {
      htmltools::HTML(paste(htmltools::htmlEscape(lines), collapse = "<br>"))
    })
    map <- leaflet::addPolygons(
      map, data = sub_sf, layerId = sub_sf$geoid, group = gname,
      fillColor = groups[[gname]], fillOpacity = 0.90, color = "#FFFFFF", weight = 0.6,
      label = sub_tips,
      labelOptions = leaflet::labelOptions(direction = "auto", className = "nyc-tooltip"),
      popup = sub_sf$pop_html,
      highlightOptions = leaflet::highlightOptions(color = navy, weight = 2.2, bringToFront = TRUE)
    )
  }

  st <- d$stations
  map <- leaflet::addCircleMarkers(
    map, lng = st$longitude, lat = st$latitude, radius = 2.5,
    group = "MTA Stations",
    label = paste(st$display_name, "-", st$daytime_routes),
    fillColor = "#C35C3C", fillOpacity = 0.80, color = "#FFFFFF", weight = 0.45
  )

  map <- leaflet::addLayersControl(
    map,
    overlayGroups = c(profile_names, "MTA Stations"),
    options = leaflet::layersControlOptions(collapsed = TRUE)
  )
  map <- leaflet::hideGroup(map, "MTA Stations")
  map <- leaflet::addLegend(
    map, pal = pal, values = factor(profile_names, levels = profile_names),
    title = "Tract Profile", position = "bottomright", opacity = 0.92
  )
  map <- leaflet::fitBounds(map, -74.26, 40.49, -73.69, 40.92)
  map$elementId <- "e-cluster-map"
  map <- htmlwidgets::onRender(map, "function(el, x, config) {
    const map = this;
    window.nycMaps = window.nycMaps || {};
    window.nycMaps[el.id] = map;
    config.ids.forEach(function(id) {
      const shape = map.layerManager.getLayer('shape', id);
      if (shape) {
        shape.on('click', function() {
          document.dispatchEvent(
            new CustomEvent('nyc-tract-select', { detail: { id: id, map: el.id, plot: config.plot } })
          );
        });
      }
    });
  }", list(ids = p$GEOID, plot = "e-pca"))

  # --- E1.2: Borough Profile Composition (100% Stacked Bar) -------------------
  boro_mix <- p[, .N, by = .(borough, group)]
  boro_mix[, share := N / sum(N), by = borough]
  boro_fig <- plotly::plot_ly()
  for (gname in profile_names) {
    sub <- boro_mix[group == gname][match(borough_order, borough)]
    sub[is.na(N), `:=`(N = 0L, share = 0)]
    boro_fig <- plotly::add_bars(
      boro_fig, x = borough_order, y = sub$share, name = gname,
      marker = list(color = groups[[gname]]),
      customdata = show_n(sub$N),
      hovertemplate = "%{x}<br>%{y:.1%} (%{customdata} tracts)<extra>%{fullData.name}</extra>"
    )
  }
  boro_fig <- finish(
    boro_fig, "e-boro-mix", "", "Share of profile tracts", TRUE,
    bottom = 78, barmode = "stack",
    x_options = list(categoryorder = "array", categoryarray = borough_order, tickangle = -15),
    y_options = list(tickformat = ".0%")
  )

  # --- E1.3: PCA Biplot (Tract Projection + Feature Loading Vectors) --------
  feature <- with(p, cbind(pmin(rent, 3500), log1p(night_rate), log1p(nypd_rate), log1p(station_m), log1p(density)))
  colnames(feature) <- c("Gross rent", "Night noise", "NYPD reports", "Rail distance", "Pop. density")
  z <- scale(feature)
  pca_fit <- prcomp(z, center = FALSE, scale. = FALSE)
  loadings <- pca_fit$rotation[, 1:2, drop = FALSE]
  arrow_scale <- 4.5
  short_load <- c("Rent", "Night noise", "NYPD", "Rail dist.", "Density")
  label_nudge <- list(
    c(-0.35,  0.35),
    c( 0.45, -0.35),
    c( 0.55,  0.25),
    c( 0.45,  0.30),
    c(-0.45, -0.35)
  )

  pca_fig <- points_by(p, "PC1", "PC2", "group", groups, detail_tip)
  biplot_annots <- list()
  for (i in seq_len(nrow(loadings))) {
    lx <- loadings[i, 1] * arrow_scale
    ly <- loadings[i, 2] * arrow_scale
    biplot_annots[[length(biplot_annots) + 1L]] <- list(
      x = lx, y = ly, ax = 0, ay = 0,
      xref = "x", yref = "y", axref = "x", ayref = "y",
      text = "", showarrow = TRUE, arrowhead = 2, arrowsize = 1.0, arrowwidth = 1.8,
      arrowcolor = "#173743"
    )
    biplot_annots[[length(biplot_annots) + 1L]] <- list(
      x = lx + label_nudge[[i]][1], y = ly + label_nudge[[i]][2],
      xref = "x", yref = "y", showarrow = FALSE,
      text = paste0("<b>", short_load[i], "</b>"),
      font = list(size = 10, color = "#173743"),
      bgcolor = "rgba(255,255,255,0.86)",
      bordercolor = "rgba(23,55,67,0.22)", borderwidth = 1, borderpad = 2
    )
  }
  pca_fig <- finish(
    pca_fig, "e-pca",
    sprintf("PC1 (%.0f%% of variance)", d$scores[1] * 100),
    sprintf("PC2 (%.0f%% of variance)", d$scores[2] * 100),
    TRUE, bottom = 75, annotations = biplot_annots
  )



  # --- E2.1: Multi-Indicator Standardized Profile Signature -----------------
  med_tbl <- p[, .(
    rent = median(as.numeric(rent)),
    night = median(night_rate),
    nypd = median(nypd_rate),
    rail = median(station_m),
    dens = median(density)
  ), by = cluster][order(cluster)]

  short_feats <- c("Gross rent", "Night noise", "NYPD reports", "Rail distance", "Pop. density")
  raw_strings <- list(
    show_dollar(med_tbl$rent),
    sprintf("%.1f / 1k", med_tbl$night),
    sprintf("%.1f / 1k", med_tbl$nypd),
    paste0(show_n(med_tbl$rail), " m"),
    paste0(show_n(med_tbl$dens), " / km2")
  )

  centers_fig <- plotly::plot_ly()
  for (k in 1:4) {
    z_vals <- as.numeric(d$centers[k, ])
    raw_k <- vapply(raw_strings, function(v) v[k], character(1))
    centers_fig <- plotly::add_trace(
      centers_fig, type = "scatter", mode = "lines+markers",
      x = short_feats, y = z_vals, name = profile_names[k],
      line = list(color = cluster_colors[k], width = 2.6),
      marker = list(color = cluster_colors[k], size = 8),
      customdata = raw_k,
      hovertemplate = paste0(
        "<b>%{fullData.name}</b><br>",
        "%{x}: %{y:+.2f} SD<br>",
        "Cluster median: %{customdata}<extra></extra>"
      )
    )
  }
  centers_fig <- finish(
    centers_fig, "e-centers", "", "Standardized z-score (SD)", TRUE, bottom = 75,
    x_options = list(categoryorder = "array", categoryarray = short_feats, tickangle = -12),
    shapes = list(list(
      type = "line", x0 = 0, x1 = 1, xref = "paper", y0 = 0, y1 = 0,
      line = list(color = muted, dash = "dash", width = 1.2)
    ))
  )

  # --- E2.2: Silhouette Distribution & Blurry Thresholds --------------------
  fit_fig <- plotly::plot_ly()
  for (k in 1:4) {
    gname <- profile_names[k]
    s <- p[cluster == k, silhouette]
    fit_fig <- plotly::add_boxplot(
      fit_fig, x = rep(gname, length(s)), y = s, name = gname, showlegend = FALSE,
      marker = list(color = cluster_colors[k], size = 4),
      line = list(color = cluster_colors[k]), boxpoints = "outliers",
      fillcolor = grDevices::adjustcolor(cluster_colors[k], 0.25)
    )
  }
  fit_fig <- finish(
    fit_fig, "e-fit", "", "Silhouette width", FALSE, bottom = 82,
    x_options = list(categoryorder = "array", categoryarray = profile_names, tickangle = -14),
    shapes = list(
      list(type = "line", x0 = 0, x1 = 1, xref = "paper", y0 = 0, y1 = 0,
           line = list(color = clay, dash = "dash", width = 1.3)),
      list(type = "line", x0 = 0, x1 = 1, xref = "paper", y0 = 0.10, y1 = 0.10,
           line = list(color = muted, dash = "dot", width = 1.2))
    ),
    annotations = list(list(
      x = 0.99, y = 0.12, xref = "paper", yref = "y", xanchor = "right",
      text = "Blurry boundary (< 0.10)", showarrow = FALSE,
      font = list(size = 10, color = muted)
    ))
  )

  # --- E2.3: Blurry Boundary Pairs Diagnostic -------------------------------
  dist_mat <- sapply(1:4, function(k) {
    ctr <- d$centers[k, ]
    sqrt(rowSums(sweep(z, 2L, ctr, "-")^2))
  })
  own_dist <- dist_mat[cbind(seq_len(nrow(p)), p$cluster)]
  other_mat <- dist_mat
  other_mat[cbind(seq_len(nrow(p)), p$cluster)] <- Inf
  sec_cluster <- max.col(-other_mat, ties.method = "first")
  sec_dist <- other_mat[cbind(seq_len(nrow(p)), sec_cluster)]

  p[, `:=`(
    dist_ratio = own_dist / sec_dist,
    neighbor_cluster = sec_cluster,
    is_blurry = silhouette < 0.10
  )]

  pair_dt <- p[is_blurry == TRUE, .(
    blurry_n = .N,
    neg_n = sum(silhouette < 0),
    mean_ratio = mean(dist_ratio)
  ), by = .(
    c_low = pmin(cluster, neighbor_cluster),
    c_high = pmax(cluster, neighbor_cluster)
  )][order(-blurry_n)]
  pair_dt[, pair_label := paste0("P", c_low, " <-> P", c_high)]
  pair_dt <- pair_dt[order(blurry_n)]

  boundary_fig <- plotly::plot_ly(
    type = "bar", orientation = "h",
    x = pair_dt$blurry_n, y = pair_dt$pair_label,
    marker = list(color = ifelse(pair_dt$blurry_n >= 100, clay, teal)),
    customdata = cbind(
      pair_dt$neg_n,
      sprintf("%.2f", pair_dt$mean_ratio),
      sprintf("%.1f%%", 100 * pair_dt$blurry_n / sum(pair_dt$blurry_n))
    ),
    hovertemplate = paste0(
      "<b>%{y}</b><br>",
      "Blurry tracts (sil < 0.10): %{x} (%{customdata[2]} of transition zone)<br>",
      "Negative silhouette: %{customdata[0]} tracts<br>",
      "Mean d_own / d_second: %{customdata[1]}<extra></extra>"
    )
  )
  boundary_fig <- finish(
    boundary_fig, "e-boundary",
    "Tracts in blurry transition zone (silhouette < 0.10)", "",
    FALSE, left = 96, bottom = 60,
    y_options = list(categoryorder = "array", categoryarray = pair_dt$pair_label)
  )

  # --- E3.1: Same-Budget Preference Switching (Retained / Entered / Exited) -
  budget_default <- 2000
  top_n_default <- 100L
  sub <- copy(p[rent <= budget_default])
  sub[, `:=`(
    s_bal   = (quiet + reports + rail) / 3,
    s_quiet = 0.70 * quiet + 0.15 * reports + 0.15 * rail,
    s_safe  = 0.15 * quiet + 0.70 * reports + 0.15 * rail,
    s_rail  = 0.15 * quiet + 0.15 * reports + 0.70 * rail
  )]
  base_ids <- sub[order(-s_bal, GEOID)][1:top_n_default, GEOID]
  scen_cols <- c("s_quiet", "s_safe", "s_rail")
  scen_labels <- c("Less Nighttime Noise", "Fewer NYPD Reports", "Closer to Station")

  turn_rows <- lapply(seq_along(scen_cols), function(i) {
    cur_ids <- sub[order(-get(scen_cols[i]), GEOID)][1:top_n_default, GEOID]
    data.table(
      scenario = scen_labels[i],
      retained = length(intersect(base_ids, cur_ids)),
      entered  = length(setdiff(cur_ids, base_ids)),
      exited   = length(setdiff(base_ids, cur_ids))
    )
  })
  turn_dt <- rbindlist(turn_rows)

  turnover_fig <- plotly::plot_ly()
  turnover_fig <- plotly::add_bars(
    turnover_fig, x = turn_dt$scenario, y = turn_dt$retained, name = "Retained (Core)",
    marker = list(color = teal),
    hovertemplate = "%{x}<br>Retained: %{y} tracts<extra></extra>"
  )
  turnover_fig <- plotly::add_bars(
    turnover_fig, x = turn_dt$scenario, y = turn_dt$entered, name = "Newly Entering",
    marker = list(color = navy),
    hovertemplate = "%{x}<br>Newly Entering: %{y} tracts<extra></extra>"
  )
  turnover_fig <- plotly::add_bars(
    turnover_fig, x = turn_dt$scenario, y = turn_dt$exited, name = "Exiting Balanced",
    marker = list(color = clay),
    hovertemplate = "%{x}<br>Exiting Balanced: %{y} tracts<extra></extra>"
  )
  turnover_fig <- finish(
    turnover_fig, "e-turnover", "", "Number of candidate tracts", TRUE,
    bottom = 78, barmode = "group",
    x_options = list(categoryorder = "array", categoryarray = scen_labels, tickangle = -10)
  )

  # --- E3.2: Borough Shift & Metric Trade-off Across Preferences ------------
  all_scen_cols <- c("s_bal", "s_quiet", "s_safe", "s_rail")
  all_scen_labels <- c("Balanced", "Less Nighttime Noise", "Fewer NYPD Reports", "Closer to Station")
  shift_list <- lapply(seq_along(all_scen_cols), function(i) {
    pool <- sub[order(-get(all_scen_cols[i]), GEOID)][1:top_n_default]
    cnt <- pool[, .(
      n = .N,
      med_rent = median(as.numeric(rent)),
      med_night = median(night_rate),
      med_nypd = median(nypd_rate),
      med_rail = median(station_m)
    ), by = borough]
    cnt[, scenario := all_scen_labels[i]]
    cnt
  })
  shift_dt <- rbindlist(shift_list)

  boro_shift_fig <- plotly::plot_ly()
  for (b in borough_order) {
    b_sub <- shift_dt[borough == b][match(all_scen_labels, scenario)]
    b_sub[is.na(n), n := 0L]
    tip_txt <- ifelse(
      b_sub$n == 0L,
      paste0(b, ": 0 tracts"),
      sprintf("%s: %d tracts<br>Med rent: %s | Night: %.1f/1k<br>NYPD: %.1f/1k | Rail: %s m",
              b, b_sub$n, show_dollar(b_sub$med_rent), b_sub$med_night, b_sub$med_nypd, show_n(b_sub$med_rail))
    )
    boro_shift_fig <- plotly::add_bars(
      boro_shift_fig, x = all_scen_labels, y = b_sub$n, name = b,
      marker = list(color = boro_colors[[b]]),
      customdata = tip_txt, hovertemplate = "%{customdata}<extra></extra>"
    )
  }
  boro_shift_fig <- finish(
    boro_shift_fig, "e-boro-shift", "", "Shortlist tracts by borough", TRUE,
    bottom = 78, barmode = "stack",
    x_options = list(categoryorder = "array", categoryarray = all_scen_labels, tickangle = -10)
  )

  # --- E3.3 & E3.4: Interactive Weight Sliders & Rank-Shift Slope (Tab 2) ---
  p[, overall := (cost + quiet + reports + rail) / 4]
  top8 <- p[order(-overall, GEOID)][1:8]
  rank_fig <- plotly::plot_ly(
    type = "bar", orientation = "h", x = top8$overall, y = top8$label,
    marker = list(color = unname(boro_colors[top8$borough])),
    text = sprintf("%.1f", top8$overall), textposition = "auto",
    hovertemplate = "%{y}<br>Score %{x:.1f}<extra></extra>"
  )
  rank_fig <- finish(
    rank_fig, "e-ranking", "Weighted percentile score (0-100)", "", FALSE, left = 166,
    y_options = list(autorange = "reversed"), x_options = list(range = c(0, 100))
  )

  slope_fig <- plotly::plot_ly()
  for (i in 1:8) {
    slope_fig <- plotly::add_trace(
      slope_fig, type = "scatter", mode = "lines+markers", x = c(1, 2), y = c(i, i),
      name = top8$label[i], line = list(color = unname(boro_colors[top8$borough[i]]), width = 2),
      hovertemplate = "Rank %{y}<extra>%{fullData.name}</extra>"
    )
  }
  slope_fig <- finish(
    slope_fig, "e-rank-shifts", "", "Rank (1 is highest)", FALSE,
    x_options = list(tickvals = c(1, 2), ticktext = c("Equal weights", "Your weights"), range = c(0.8, 2.2))
  )

  list(
    map, boro_fig, pca_fig,
    centers_fig, fit_fig, boundary_fig,
    turnover_fig, boro_shift_fig,
    rank_fig, slope_fig
  )
}

turnover_explorer_table <- function(d, budget_cap = 2000, top_n = 100L) {
  sub <- copy(d$profile[rent <= budget_cap])
  sub[, `:=`(
    s_bal   = (quiet + reports + rail) / 3,
    s_quiet = 0.70 * quiet + 0.15 * reports + 0.15 * rail,
    s_safe  = 0.15 * quiet + 0.70 * reports + 0.15 * rail,
    s_rail  = 0.15 * quiet + 0.15 * reports + 0.70 * rail
  )]
  sub[order(-s_bal, GEOID),   rk_bal   := .I]
  sub[order(-s_quiet, GEOID), rk_quiet := .I]
  sub[order(-s_safe, GEOID),  rk_safe  := .I]
  sub[order(-s_rail, GEOID),  rk_rail  := .I]

  sub[, `:=`(
    in_bal   = rk_bal   <= top_n,
    in_quiet = rk_quiet <= top_n,
    in_safe  = rk_safe  <= top_n,
    in_rail  = rk_rail  <= top_n
  )]

  tbl <- sub[in_bal | in_quiet | in_safe | in_rail, .(
    Tract = label,
    Borough = borough,
    Profile = paste0("P", cluster),
    `Rent ($)` = rent,
    `Night (/1k)` = round(night_rate, 1),
    `NYPD (/1k)` = round(nypd_rate, 1),
    `Rail (m)` = round(station_m, 0),
    `Bal. Rank` = rk_bal,
    `Quiet Rank` = rk_quiet,
    `Safety Rank` = rk_safe,
    `Rail Rank` = rk_rail,
    Status = fifelse(
      in_bal & in_quiet & in_safe & in_rail, "Core (All 4)",
      fifelse(
        in_bal & (in_quiet | in_safe | in_rail), "Retained (Partial)",
        fifelse(
          in_bal, "Exits Balanced",
          paste0(
            "Enters: ",
            trimws(paste0(
              fifelse(in_quiet, "Quiet ", ""),
              fifelse(in_safe, "Safety ", ""),
              fifelse(in_rail, "Rail", "")
            ))
          )
        )
      )
    )
  )][order(`Bal. Rank`)]

  DT::datatable(
    tbl,
    rownames = FALSE,
    filter = "top",
    class = "compact stripe hover nyc-dt",
    options = list(
      pageLength = 10,
      lengthMenu = c(10, 25, 50),
      scrollX = TRUE,
      autoWidth = FALSE
    )
  )
}
