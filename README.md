# Rental Trade-offs in New York City

A Census Tract Analysis of Rent, Noise Complaints, Police Reports, and Rail Access

DS4073 Group 6 的协作起点。研究问题是，在纽约市同一个行政区内，租金相近的普查区在夜间噪音举报率、NYPD 报案率和到最近车站的直线距离上会相差多少，以及不同偏好会怎样改变候选名单。

下载仓库后在浏览器打开 [Dashboard.html](Dashboard.html)。它包含五页、15 个任务和 39 个图表，地图和交互可离线运行。在 GitHub 网页打开 HTML 文件时，可以用 Download raw file 下载后在本地查看。

[当前版本与可深化方向](VERSION.md)

## 五人分工

| 模块 | 分析问题 |
| --- | --- |
| [A · 租金和负担](module_A/README.md) | 同一预算能覆盖哪些地方？租金和租房负担在哪些区不同步？ |
| [B · 噪音举报](module_B/README.md) | 不同类型的举报在哪里、什么时候集中？ |
| [C · 报案与轨道交通](module_C/README.md) | 同一行政区内，不同站距组的报案率有什么差异？ |
| [D · 同区同价](module_D/README.md) | 租金相近的地方，夜间举报率能相差多少？ |
| [E · 画像与偏好](module_E/README.md) | 常见的指标组合有哪些？改变偏好后候选名单如何变化？ |

每行最多两张图，各任务第三张另起一行。

各模块的 `visualizations.R` 由 `Dashboard.Rmd` 加载。改自己模块的图时，从该目录的 R 文件入手。修改标题、图注和布局时编辑 `Dashboard.Rmd` 对应页面。共享分析在 `scripts/analysis.R`，通用图表函数在 `scripts/widgets.R`，页面联动在 `scripts/dashboard.js`。修改后从仓库根目录重新渲染并检查。

C 部分的八张 Dashboard 图及分析口径见 [module_C](module_C/README.md)。

## 用 R 重新生成 Dashboard

从仓库根目录在 RStudio 打开 `Dashboard.Rmd` 执行 Knit，或在 R 控制台运行以下代码。R Markdown 需要 Pandoc，RStudio 自带。

```r
install.packages(c("data.table", "sf", "units", "jsonlite", "rmarkdown", "knitr",
                   "flexdashboard", "plotly", "leaflet", "htmltools", "htmlwidgets", "cluster", "DT"))
rmarkdown::render("Dashboard.Rmd")
```

## 数据与复跑

仓库包含完整的项目输入快照以及生成网页用的处理结果。

- `raw-events/` 是 2024 年逐月的 311 噪音举报和 NYPD 报案，分别有 753,278 和 574,192 条。记录按项目用到的日期和字段提取，保留原始下载内容。各月行数见 `event_counts.json`。
- `source/` 是 ACS 2020–2024 五年估计原表、普查区原始边界和 MTA 车站原始快照。体积较大的 ACS B25070 以 `acs-b25070.zip` 保存，压缩包内是原始 `.dat`，R 会自动解压。其余 ACS `.dat` 是以竖线分列的纯文本。
- `data/` 是 Dashboard 读取的普查区合并表、简化地图和汇总表。`tracts.csv` 共 2,325 个区，`profile = TRUE` 的 2,178 个区为联合分析样本。`join-audit.csv` 记录空间匹配行数。

安装上面的依赖后，在仓库根目录运行 `Rscript scripts/prepare.R raw-events source`，即可用包内同一批快照生成 `data/`。预期有 749,472 条 311 举报和 574,101 条 NYPD 报案匹配到唯一普查区。`scripts/fetch_events.py` 留作需要更新在线事件数据时使用。项目匹配统一到纽约州平面坐标系 EPSG:2263，地图显示使用匹配完以后简化的普查区几何。若本地的 PROJ 数据库路径有问题，可用环境变量 `NYC_PROJ_DIR` 指向有效目录。

## 官方来源

| Source | Input | Role |
|---|---|---|
| [U.S. Census ACS Summary File](https://www.census.gov/programs-surveys/acs/data/summary-file.2024.html) | 2020–2024 5-year tables B25064, B25070, B19013 and B01003 | Median gross rent, renter burden, household income and population |
| [NYC 311](https://data.cityofnewyork.us/d/erm2-nwe9) | 2024 service requests whose problem begins with Noise | Report counts and hourly patterns |
| [NYPD historic complaint data](https://data.cityofnewyork.us/d/qgea-i56i) | Reports where complaint occurrence start date is in 2024 | Geocoded report counts and offense levels |
| [MTA station complexes](https://data.ny.gov/d/5f5g-n3cz) | Public station snapshot, including Staten Island Railway | Reference locations for straight-line distance and station markers |
| [NYC DCP tract boundaries](https://data.cityofnewyork.us/d/63ge-mke6) | Shoreline-clipped 2020 census tracts | Spatial key and offline map geometry |

## 分析口径

- ACS rent is paid monthly gross rent estimated over a five-year period. The Census upper rent category is $3,500+. Rent burden is the share of computable renter households at or above 30% of income. Burden comparisons require at least 50 computable households.
- Spatial event rates use at least 500 ACS residents. Time analysis retains every 311 request for citywide totals. Borough figures use the tract borough when available and the reported borough otherwise. The heatmap adjusts for how often each weekday occurred in 2024. Monthly curves adjust for calendar days.
- NYPD complaint and 311 reporting rates depend on recording, incident locations, daytime populations and reporting activity. The station snapshot provides a straight-line distance proxy. Tract-level comparisons are descriptive.
- The rent versus night-reporting quadrant uses the separate sample medians. The paired example uses uncensored rents within $100 in one borough and illustrates the largest eligible contrast. The dashboard recomputes Spearman's rho and the share above both sample medians after excluding top-coded rent, tracts above the full profile sample's 99th percentile for either report rate, and both together.
- The exploratory four-group model standardizes capped rent and log-transformed event rates, distance and density. The dashboard compares mean silhouette widths at K = 2–5 with seed 4073, 25 starts and a 100-iteration limit. Four-group fits on the filtered samples are re-standardized and compared with the baseline using the adjusted Rand index on retained tracts. PCA is a projection of the model inputs.
- Preference sliders combine four favorable percentiles for the common eligible sample. Weights normalize to 100%. Equal weights apply if every slider is zero. Ranks change with the scenario and ties sort by GEOID.
