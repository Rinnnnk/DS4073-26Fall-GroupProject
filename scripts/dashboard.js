/* Cross-view selection and preference weighting run entirely in the browser. */
window.HTMLWidgets.addPostRenderHandler(function () {
  const byId = id => document.getElementById(id);
  const payloadNode = byId('nyc-payload');
  if (!payloadNode || payloadNode.dataset.ready) return;
  const payload = JSON.parse(payloadNode.textContent);

  const clock = byId('b-noise-clock');
  const profile = byId('b-noise-hour');
  const noiseType = byId('noise-type');
  const days = ['Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday'];
  let selectedDay = -1;

  function profileFor(indices, counts) {
    const denominator = indices.reduce((sum, i) => sum + payload.days[i], 0);
    return counts[0].map((_, hour) => indices.reduce((sum, i) => sum + counts[i][hour], 0) / denominator);
  }

  function showNoise() {
    const data = payload.noise[noiseType.value];
    const weekday = profileFor([0, 1, 2, 3, 4], data.counts);
    const weekend = profileFor([5, 6], data.counts);
    const daily = selectedDay < 0 ? weekday : data.rate[selectedDay];
    const shapes = selectedDay < 0 ? [] : [{
      type: 'rect', xref: 'paper', yref: 'y', x0: 0, x1: 1,
      y0: selectedDay + .55, y1: selectedDay + 1.45, line: { color: '#A67834', width: 2.5 }
    }];
    Plotly.update(clock, { z: [data.rate] }, { shapes });
    Plotly.restyle(profile, {
      y: [weekday, weekend, daily],
      name: ['Weekdays', 'Weekend', days[selectedDay] || 'Selected day'],
      visible: [true, true, selectedDay >= 0]
    });
    byId('noise-status').textContent = selectedDay < 0 ? noiseType.value : `${noiseType.value} · ${days[selectedDay]}`;
  }

  noiseType.addEventListener('change', showNoise);
  byId('noise-clear').addEventListener('click', () => { selectedDay = -1; showNoise(); });
  clock.on('plotly_click', event => {
    selectedDay = Number(event.points[0].y) - 1;
    showNoise();
  });

  const tractInfo = new Map(payload.links.map(t => [t.GEOID, t]));
  const highlights = new Map();

  function showTract(id, mapID, plotID) {
    const tract = tractInfo.get(id);
    if (!tract) return;
    const map = window.nycMaps?.[mapID];
    if (map) {
      const old = highlights.get(mapID);
      if (old) old.setStyle({ color: '#FFFFFF', weight: .6 });
      const current = map.layerManager.getLayer('shape', id);
      if (current) {
        current.bringToFront();
        current.setStyle({ color: '#BA7930', weight: 3 });
        highlights.set(mapID, current);
      }
    }
    const plot = byId(plotID);
    const selected = plot.data.map(trace => {
      const index = trace.customdata?.indexOf(id) ?? -1;
      return index < 0 ? [] : [index];
    });
    Plotly.restyle(plot, { selectedpoints: selected, 'selected.marker.opacity': 1, 'unselected.marker.opacity': .17 });
    const isMismatch = plotID === 'd-quadrant';
    byId(isMismatch ? 'd-selection' : 'e-selection').textContent =
      `${tract.label} · ${isMismatch ? tract.quad : 'Profile ' + tract.cluster}`;
  }
  document.addEventListener('nyc-tract-select', event => {
    const choice = event.detail;
    showTract(choice.id, choice.map, choice.plot);
  });
  for (const [plot, map] of [['d-quadrant', 'd-quad-map'], ['e-pca', 'e-cluster-map']]) {
    byId(plot).on('plotly_click', ({ points }) => {
      if (points[0].customdata) showTract(points[0].customdata, map, plot);
    });
  }

  const dimensions = ['cost', 'quiet', 'reports', 'rail'];
  const preferences = {
    equal: [25, 25, 25, 25],
    quiet: [20, 60, 10, 10],
    rail: [20, 10, 10, 60]
  };
  const score = (row, weights) => dimensions.reduce((sum, key, i) => sum + row[key] * weights[i], 0);
  const order = (a, b) => b.score - a.score || a.GEOID.localeCompare(b.GEOID);
  const profiles = payload.profiles;
  const baseline = profiles.map(row => ({ ...row, score: score(row, [.25, .25, .25, .25]) })).sort(order);
  const baselineRank = new Map(baseline.map((row, i) => [row.GEOID, i + 1]));
  const rankPlot = byId('e-ranking');
  const slope = byId('e-rank-shifts');
  let pending = null;

  function showRank() {
    const values = dimensions.map(key => Number(byId('w-' + key).value));
    const sum = values.reduce((a, b) => a + b, 0);
    const weights = sum > 0 ? values.map(v => v / sum) : [.25, .25, .25, .25];
    dimensions.forEach((key, i) => { byId('v-' + key).textContent = (100 * weights[i]).toFixed(1) + '%'; });
    const current = profiles.map(row => ({ ...row, score: score(row, weights) })).sort(order).slice(0, 8);
    const earlier = current.map(row => baselineRank.get(row.GEOID));
    const colors = current.map(row => payload.colors[row.borough]);
    Plotly.restyle(rankPlot, {
      x: [current.map(row => row.score)],
      y: [current.map(row => row.label)],
      text: [current.map(row => row.score.toFixed(1))],
      customdata: [current.map(row => `Rent $${row.rent.toLocaleString('en-US')}<br>Night requests/1,000: ${row.night_rate.toFixed(1)}<br>Rail: ${Math.round(row.station_m)} m`)],
      hovertemplate: '%{y}<br>Score %{x:.1f}<br>%{customdata}<extra></extra>',
      'marker.color': [colors]
    });
    const maximum = Math.max(...earlier, 8);
    Plotly.update(slope, {
      y: earlier.map((value, i) => [value, i + 1]),
      name: current.map(row => row.label),
      'line.color': colors,
      'marker.color': colors
    }, {
      'yaxis.type': maximum > 60 ? 'log' : 'linear',
      'yaxis.autorange': 'reversed',
      'yaxis.title.text': maximum > 60 ? 'Rank (log scale)' : 'Rank'
    });
    const same = earlier.filter(rank => rank <= 8).length;
    byId('e-rank-status').textContent = `${same} of 8 tracts also appear in the equal-weight shortlist`;
  }

  function scheduleRank() {
    if (pending !== null) cancelAnimationFrame(pending);
    pending = requestAnimationFrame(() => { pending = null; showRank(); });
  }
  dimensions.forEach(key => byId('w-' + key).addEventListener('input', scheduleRank));
  byId('e-presets').addEventListener('click', event => {
    const weights = preferences[event.target.value];
    if (!weights) return;
    dimensions.forEach((key, i) => { byId('w-' + key).value = weights[i]; });
    scheduleRank();
  });
  showRank();
  payloadNode.dataset.ready = 'true';
});
