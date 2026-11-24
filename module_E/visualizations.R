profile_widgets <- function(d) {
  p <- copy(d$profile)
  p[, group := paste("Profile", cluster)]
  groups <- setNames(cluster_colors, paste("Profile", 1:4))
  detail <- paste0(p$label, "<br>", p$group, "<br>", show_rent(p$rent), " median gross rent")
  pca <- points_by(p, "PC1", "PC2", "group", groups, detail)
  pca <- finish(pca, "e-pca", sprintf("PC1 (%.0f%% of variance)", d$scores[1] * 100),
    sprintf("PC2 (%.0f%% of variance)", d$scores[2] * 100), TRUE, bottom = 75)
  map <- geo_map(d, "e-cluster-map", setNames(p$group, p$GEOID), "Cluster",
                 setNames(detail, p$GEOID), groups = groups, linked = "e-pca")
  limit <- max(abs(d$centers))
  heat <- plotly::plot_ly(type = "heatmap", z = d$centers, x = colnames(d$centers), y = names(groups),
    zmin = -limit, zmax = limit, colorscale = list(c(0, "#356C92"), c(.5, "#F3F4F0"), c(1, clay)),
    xgap = 3, ygap = 3, colorbar = list(title = "z-score", thickness = 10),
    hovertemplate = "%{y}<br>%{x}: %{z:+.2f} SD<extra></extra>")
  heat <- finish(heat, "e-centers", "", "", bottom = 61, x_options = list(tickangle = -14))
  fit <- plotly::plot_ly()
  for (g in names(groups)) {
    s <- p[group == g, silhouette]
    fit <- plotly::add_boxplot(fit, x = rep(g, length(s)), y = s, name = g,
      marker = list(color = groups[[g]]), line = list(color = groups[[g]]), boxpoints = "outliers",
      fillcolor = grDevices::adjustcolor(groups[[g]], .23))
  }
  fit <- finish(fit, "e-fit", "", "Silhouette width",
    shapes = list(list(type = "line", x0 = 0, x1 = 1, xref = "paper", y0 = 0, y1 = 0,
                       line = list(color = muted, dash = "dash"))))
  p[, overall := (cost + quiet + reports + rail) / 4]
  top <- p[order(-overall, GEOID)][1:8]
  rank <- plotly::plot_ly(type = "bar", orientation = "h", x = top$overall, y = top$label,
    marker = list(color = unname(boro_colors[top$borough])),
    text = sprintf("%.1f", top$overall), textposition = "auto",
    hovertemplate = "%{y}<br>Score %{x:.1f}<extra></extra>")
  rank <- finish(rank, "e-ranking", "Weighted percentile score (0–100)", "", left = 166,
    y_options = list(autorange = "reversed"), x_options = list(range = c(0, 100)))
  slope <- plotly::plot_ly()
  for (i in 1:8) {
    slope <- plotly::add_trace(slope, type = "scatter", mode = "lines+markers", x = c(1, 2), y = c(i, i),
      name = top$label[i], line = list(color = unname(boro_colors[top$borough[i]]), width = 2),
      hovertemplate = "Rank %{y}<extra>%{fullData.name}</extra>")
  }
  slope <- finish(slope, "e-rank-shifts", "", "Rank (1 is highest)",
    x_options = list(tickvals = c(1, 2), ticktext = c("Equal weights", "Your weights"), range = c(.8, 2.2)))
  list(pca, map, heat, fit, rank, slope)
}
