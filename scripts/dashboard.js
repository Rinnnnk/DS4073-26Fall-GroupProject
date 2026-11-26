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
      if (old && old._map) old.setStyle({ color: '#FFFFFF', weight: .6 });
      const current = map.layerManager.getLayer('shape', id);
      if (current) {
        if (current._map && typeof current.bringToFront === 'function') current.bringToFront();
        current.setStyle({ color: '#BA7930', weight: 3 });
        highlights.set(mapID, current);
      }
    }
    const plot = byId(plotID);
    if (plot && plot.data) {
      const selected = plot.data.map(trace => {
        const index = trace.customdata?.indexOf(id) ?? -1;
        return index < 0 ? [] : [index];
      });
      Plotly.restyle(plot, { selectedpoints: selected, 'selected.marker.opacity': 1, 'unselected.marker.opacity': .17 });
    }
    const isMismatch = plotID === 'd-quadrant';
    const statusEl = byId(isMismatch ? 'd-selection' : 'e-selection');
    if (statusEl) {
      statusEl.textContent = `${tract.label} · ${isMismatch ? tract.quad : 'Profile ' + tract.cluster}`;
    }
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
    Plotly.update(rankPlot, {
      x: [current.map(row => row.score)],
      y: [current.map(row => row.label)],
      text: [current.map(row => row.score.toFixed(1))],
      customdata: [current.map(row => `Rent $${row.rent.toLocaleString('en-US')}<br>Night requests/1,000: ${row.night_rate.toFixed(1)}<br>Rail: ${Math.round(row.station_m)} m`)],
      hovertemplate: '%{y}<br>Score %{x:.1f}<br>%{customdata}<extra></extra>',
      'marker.color': [colors]
    }, {
      'yaxis.categoryorder': 'array',
      'yaxis.categoryarray': current.map(row => row.label),
      'yaxis.autorange': 'reversed'
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

  // --- Module E3: Same-budget preference switching & turnover ---------------
  const budgetSelect = byId('e-budget-cap');
  const poolSelect = byId('e-pool-size');
  const turnoverPlot = byId('e-turnover');
  const boroShiftPlot = byId('e-boro-shift');
  const eBudgetStatus = byId('e-budget-status');

  const eScenarios = [
    { key: 'bal',   label: 'Balanced',            w: [0, 1 / 3, 1 / 3, 1 / 3] },
    { key: 'quiet', label: 'Less Nighttime Noise', w: [0, 0.70, 0.15, 0.15] },
    { key: 'safe',  label: 'Fewer NYPD Reports',  w: [0, 0.15, 0.70, 0.15] },
    { key: 'rail',  label: 'Closer to Station',   w: [0, 0.15, 0.15, 0.70] }
  ];

  function medianVal(arr) {
    if (!arr.length) return 0;
    const s = arr.slice().sort((a, b) => a - b);
    const m = Math.floor(s.length / 2);
    return s.length % 2 ? s[m] : (s[m - 1] + s[m]) / 2;
  }

  function updateBudgetTurnover() {
    if (!budgetSelect || !poolSelect || !turnoverPlot || !boroShiftPlot) return;
    const capVal = budgetSelect.value;
    const topN = Number(poolSelect.value) || 100;
    const eligible = profiles.filter(row => {
      if (capVal === 'boro') return row.rent <= (payload.boro_med?.[row.borough] ?? 99999);
      return row.rent <= Number(capVal);
    });
    const k = Math.min(topN, eligible.length);
    const sets = {};
    const pools = {};
    eScenarios.forEach(sc => {
      const ranked = eligible.map(row => ({ ...row, score: score(row, sc.w) })).sort(order).slice(0, k);
      pools[sc.key] = ranked;
      sets[sc.key] = new Set(ranked.map(r => r.GEOID));
    });

    const baseSet = sets.bal;
    const switchScens = eScenarios.slice(1);
    const retained = switchScens.map(sc => [...sets[sc.key]].filter(id => baseSet.has(id)).length);
    const entered  = switchScens.map(sc => [...sets[sc.key]].filter(id => !baseSet.has(id)).length);
    const exited   = switchScens.map(sc => [...baseSet].filter(id => !sets[sc.key].has(id)).length);

    Plotly.restyle(turnoverPlot, {
      y: [retained, entered, exited]
    });

    const boroNames = Object.keys(payload.colors);
    const yTraces = boroNames.map(b => eScenarios.map(sc => pools[sc.key].filter(r => r.borough === b).length));
    const textTraces = boroNames.map(b => eScenarios.map(sc => {
      const sub = pools[sc.key].filter(r => r.borough === b);
      if (!sub.length) return `${b}: 0 tracts`;
      const mRent = Math.round(medianVal(sub.map(r => r.rent))).toLocaleString('en-US');
      const mNight = medianVal(sub.map(r => r.night_rate)).toFixed(1);
      const mNypd = medianVal(sub.map(r => r.nypd_rate)).toFixed(1);
      const mRail = Math.round(medianVal(sub.map(r => r.station_m))).toLocaleString('en-US');
      return `${b}: ${sub.length} tracts<br>Med rent: $${mRent} | Night: ${mNight}/1k<br>NYPD: ${mNypd}/1k | Rail: ${mRail} m`;
    }));

    Plotly.restyle(boroShiftPlot, {
      y: yTraces,
      customdata: textTraces
    });

    if (eBudgetStatus) {
      const pcts = entered.map((v, idx) => `${switchScens[idx].label}: ${Math.round(100 * v / k)}%`);
      const capLabel = capVal === 'boro' ? 'Borough-median cap' : `<= $${Number(capVal).toLocaleString('en-US')}`;
      eBudgetStatus.textContent = `${eligible.length.toLocaleString('en-US')} eligible tracts (${capLabel}) · Top-${k} turnover: ${pcts.join(' | ')}`;
    }
  }

  if (budgetSelect && poolSelect) {
    budgetSelect.addEventListener('change', updateBudgetTurnover);
    poolSelect.addEventListener('change', updateBudgetTurnover);
    updateBudgetTurnover();
  }

  // Ensure hidden tab Plotly charts and DataTables resize cleanly when revealed.
  function resizeTabWidgets(container) {
    if (!container) return;
    const plots = container.querySelectorAll('.js-plotly-plot');
    plots.forEach(el => {
      if (window.Plotly && Plotly.Plots) Plotly.Plots.resize(el);
    });
    if (window.jQuery && window.jQuery.fn && window.jQuery.fn.dataTable) {
      window.jQuery(container).find('table.dataTable').each(function () {
        window.jQuery(this).DataTable().columns.adjust();
      });
    }
    window.dispatchEvent(new Event('resize'));
  }

  if (window.jQuery) {
    window.jQuery(document).on('shown.bs.tab', 'a[data-toggle="tab"]', function (e) {
      const targetSel = e.target.getAttribute('href');
      if (targetSel) resizeTabWidgets(document.querySelector(targetSel));
    });
  }

  payloadNode.dataset.ready = 'true';
});
