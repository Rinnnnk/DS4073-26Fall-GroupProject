function (matrix, x, data) {
  const slider = document.getElementById("a-budget");
  const curve = document.getElementById("a-rent-coverage");
  const amount = document.getElementById("a-budget-value");
  const status = document.getElementById("a-budget-status");
  let pending;

  function update() {
    const budget = Number(slider.value);
    const selected = data.rows[budget];
    const ceilings = data.scenarios[budget];
    const columns = ceilings.map(ceiling => data.rows[ceiling]);
    const active = ceilings.indexOf(budget);
    const rows = data.boroughs.map((borough, i) => columns.map(column => column.shares[i]));
    const hover = data.boroughs.map((borough, i) => columns.map(column =>
      borough + " · " + column.title + "<br>" + column.labels[i] + "<br>" + column.counts[i]));
    const labels = data.boroughs.flatMap((borough, i) => columns.map((column, j) => ({
      x: j, y: i, text: column.labels[i], showarrow: false,
      font: { size: 13, color: column.shares[i] >= data.labelCutoff ? "#FFFFFF" : "#000000" }
    })));
    amount.textContent = selected.title;
    status.textContent = selected.total + " eligible tracts citywide within budget";
    Plotly.update(matrix, { z: [rows], text: [hover] }, {
      "xaxis.ticktext": columns.map(column => column.title), annotations: labels,
      "shapes[0].x0": active - .48, "shapes[0].x1": active + .48
    }, [0]);
    Plotly.update(curve, {
      x: [data.boroughs.map(() => budget)], y: [selected.shares],
      text: [data.boroughs.map((borough, i) => borough + "<br>" + selected.labels[i] + " within budget")]
    }, { "shapes[0].x0": budget, "shapes[0].x1": budget }, [data.markerTrace]);
  }

  function select(budget) {
    slider.value = budget;
    update();
  }

  slider.addEventListener("input", function () {
    cancelAnimationFrame(pending);
    pending = requestAnimationFrame(update);
  });
  document.querySelectorAll("#a-budget-presets button").forEach(function (button) {
    button.addEventListener("click", () => select(button.value));
  });
  matrix.on("plotly_click", function (event) {
    const column = event.points[0].x;
    select(data.scenarios[Number(slider.value)][column]);
  });
}
