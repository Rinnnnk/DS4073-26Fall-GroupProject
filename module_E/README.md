# Module E: Tract Profiles & Same-Budget Preference Switching

Author: **Weiye Xu (Student ID: 2430026242)**

Module E examines multi-dimensional census tract profiles (`2,178` joint-analysis tracts) and measures how candidate rental pools shift when renters change priorities on the same rent budget.

## Structure
- **Row 1 (Task E1)**: Interactive `leaflet` cluster map with profile layer controls and MTA station overlay (`e-cluster-map`), 100% stacked borough profile composition (`e-boro-mix`), and PCA biplot with feature loading vectors (`e-pca`).
- **Row 2 (Task E2)**: Standardized multi-indicator profile signature (`e-centers`), silhouette distribution with `0` and `0.10` blurry-zone thresholds (`e-fit`), and cross-profile blurry boundary pair diagnostic (`e-boundary`).
- **Row 3 (Task E3)**: Same-budget preference switching (`<= $1,800`, `<= $2,000`, `<= $2,400`, or borough-median budget tier) comparing Retained, Newly Entering, and Exiting tracts (`e-turnover`) and borough/metric shifts (`e-boro-shift`).
- **Row 4 (Explorer & Sandbox)**: Searchable `DT::datatable` candidate turnover table alongside the interactive 4-slider custom weight rank-shift sandbox (`e-ranking`, `e-rank-shifts`).

