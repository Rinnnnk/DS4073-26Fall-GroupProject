# Reusable, locally rendered Plotly and Leaflet charts for the course dashboard.
navy <- "#23485F"
teal <- "#217B78"
clay <- "#B35F49"
muted <- "#607582"
line <- "#DEE7E8"
paper <- "#FFFFFF"

finish <- function(fig, id, x, y, legend = FALSE, left = 72, bottom = 66,
                   x_options = list(), y_options = list(), ...) {
  fig <- plotly::layout(fig,
    font = list(family = "Arial, sans-serif", size = 12, color = navy),
    hoverlabel = list(align = "left"), paper_bgcolor = paper, plot_bgcolor = paper,
    margin = list(l = left, r = 23, t = if (legend) 58 else 17, b = bottom), autosize = TRUE,
    xaxis = modifyList(list(title = x, gridcolor = line, zerolinecolor = line, automargin = TRUE), x_options),
    yaxis = modifyList(list(title = y, gridcolor = line, zerolinecolor = line, automargin = TRUE), y_options),
    showlegend = legend, legend = list(orientation = "h", y = 1.055, yanchor = "bottom", x = 0,
                                       font = list(size = 11)), ...)
  fig$elementId <- id
  fig$height <- 344
  plotly::config(fig, displaylogo = FALSE, responsive = TRUE,
                 modeBarButtonsToRemove = c("lasso2d", "select2d"))
}

points_by <- function(tbl, x, y, group, colors, detail) {
  fig <- plotly::plot_ly()
  for (category in names(colors)) {
    keep <- which(tbl[[group]] == category)
    if (!length(keep)) next
    fig <- plotly::add_markers(fig, x = tbl[[x]][keep], y = tbl[[y]][keep],
      name = category, text = detail[keep], customdata = tbl$GEOID[keep],
      marker = list(color = colors[[category]], opacity = .65, size = 5.5),
      hovertemplate = "%{text}<extra></extra>")
  }
  fig
}

geo_map <- function(d, id, value, title, labels, bins = NULL, groups = NULL, stations = FALSE, linked = NULL) {
  # Use the bundled geometry for a fully offline map.
  p <- d$shapes
  values <- unname(value[p$geoid])
  filled <- "#E4E8E5"
  palette <- if (!is.null(groups)) {
    leaflet::colorFactor(unname(groups), levels = names(groups), na.color = filled)
  } else if (!is.null(bins)) {
    leaflet::colorBin(c("#E5F2EE", "#B9D9CC", "#75AEA5", "#327B7A", "#13434F"),
                      bins = bins, domain = values, na.color = filled)
  } else {
    leaflet::colorNumeric(c("#E4F1EC", "#8EBEB4", "#174B5C"), domain = values, na.color = filled)
  }
  display <- unname(labels[p$geoid])
  default_label <- d$tr$label[match(p$geoid, d$tr$GEOID)]
  display[is.na(display)] <- paste(default_label[is.na(display)], "· missing or excluded")
  tooltip <- lapply(strsplit(display, "<br>", fixed = TRUE), function(lines) {
    htmltools::HTML(paste(htmltools::htmlEscape(lines), collapse = "<br>"))
  })
  map <- leaflet::leaflet(p, options = leaflet::leafletOptions(preferCanvas = TRUE, zoomSnap = .25), height = 344)
  map <- leaflet::addPolygons(map, layerId = p$geoid, fillColor = palette(values), fillOpacity = .93,
    color = "#FFFFFF", weight = .6, label = tooltip,
    labelOptions = leaflet::labelOptions(direction = "auto", className = "nyc-tooltip"),
    highlightOptions = leaflet::highlightOptions(color = navy, weight = 2, bringToFront = TRUE))
  if (stations) {
    s <- d$stations
    map <- leaflet::addCircleMarkers(map, lng = s$longitude, lat = s$latitude, radius = 2.5,
      label = paste(s$display_name, "·", s$daytime_routes), fillColor = "#C35C3C", fillOpacity = .8,
      color = "#FFFFFF", weight = .45)
  }
  bin_labels <- function(type, cuts, ...) {
    if (type != "bin") return(leaflet::labelFormat()(type, cuts, ...))
    low <- show_n(head(cuts, -1L))
    high <- tail(cuts, -1L)
      ifelse(is.infinite(high), paste0(low, "+"), paste(low, show_n(high), sep = "-"))
  }
  map <- leaflet::addLegend(map, pal = palette, values = values, title = title, position = "bottomright",
                            opacity = .92, na.label = "Missing / excluded", labFormat = bin_labels)
  map <- leaflet::fitBounds(map, -74.26, 40.49, -73.69, 40.92)
  map$elementId <- id
  if (!is.null(linked)) {
    map <- htmlwidgets::onRender(map, "function(el, x, config) {
      const map = this;
      window.nycMaps = window.nycMaps || {};
      window.nycMaps[el.id] = map;
      config.ids.forEach(id => {
        const shape = map.layerManager.getLayer('shape', id);
        if (shape) shape.on('click', () => document.dispatchEvent(
          new CustomEvent('nyc-tract-select', {detail:{id, map:el.id, plot:config.plot}})));
      });
    }", list(ids = p$geoid[!is.na(values)], plot = linked))
  }
  map
}
