function (el, x, data) {
  const map = this;
  const outline = L.layerGroup().addTo(map);
  const readout = document.getElementById("a-tract-selected");
  const clear = document.getElementById("a-clear-tract");

  // Wait until Plotly has bound the companion widget, including its event API.
  HTMLWidgets.addPostRenderHandler(function () {
    const plot = document.getElementById("a-income-rent");
    const overlay = plot.data.length - 1;
    const points = new Map();
    plot.data.slice(0, overlay).forEach(trace => {
      trace.customdata.forEach((id, i) => points.set(id, {
        x: trace.x[i], y: trace.y[i], label: trace.text[i].split("<br>")[0]
      }));
    });

    function select(id) {
      const point = points.get(id);
      const layer = map.layerManager.getLayer("shape", id);
      if (!layer) return;
      outline.clearLayers();
      L.geoJSON(layer.toGeoJSON(), {
        interactive: false, style: { color: "#173743", weight: 3, fill: false }
      }).addTo(outline);
      if (!map.getBounds().intersects(layer.getBounds())) {
        map.panTo(layer.getBounds().getCenter());
      }
      Plotly.restyle(plot, { x: [[point?.x ?? null]], y: [[point?.y ?? null]] }, [overlay]);
      readout.textContent = point?.label ?? "Rent or household income unavailable";
      clear.disabled = false;
    }

    data.ids.forEach(id => {
      const layer = map.layerManager.getLayer("shape", id);
      if (layer) layer.on("click", () => select(id));
    });
    plot.on("plotly_click", ({ points }) => {
      const id = points[0]?.customdata;
      if (id) select(id);
    });
    clear.addEventListener("click", () => {
      outline.clearLayers();
      Plotly.restyle(plot, { x: [[null]], y: [[null]] }, [overlay]);
      readout.textContent = "";
      clear.disabled = true;
    });
  });
}
