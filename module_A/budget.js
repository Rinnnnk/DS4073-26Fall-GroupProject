function (el, x, rows) {
  const slider = document.getElementById("a-budget");
  const curve = document.getElementById("a-rent-coverage");
  const amount = document.getElementById("a-budget-value");
  const status = document.getElementById("a-budget-status");
  const money = new Intl.NumberFormat("en-US", {style: "currency", currency: "USD", maximumFractionDigits: 0});
  let pending;
  function update() {
    const value = Number(slider.value);
    const row = rows[value];
    amount.textContent = money.format(value);
    status.textContent = row.total + " eligible tracts citywide within budget";
    Plotly.restyle(el, {x: [row.shares], text: [row.labels], customdata: [row.counts]}, [0]);
    if (curve && curve.layout) {
      Plotly.relayout(curve, {"shapes[0].x0": value, "shapes[0].x1": value});
    }
  }
  slider.addEventListener("input", function () {
    cancelAnimationFrame(pending);
    pending = requestAnimationFrame(update);
  });
  document.querySelectorAll("#a-budget-presets button").forEach(function (button) {
    button.addEventListener("click", function () {
      slider.value = this.value;
      update();
    });
  });
}
