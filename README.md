# GeoSpatial Climate Analytics: Forest Fires & Global GHG Emissions

This repository collects several small geospatial analytics projects focused on climate and environmental risk:
1) predicting forest-fire burned area in Portugal from meteorological indices, and  
2) analysing international greenhouse gas emissions by gas, country, and year using R, ggplot2, and Shiny.

---

## Projects

### 1. Forest Fire Burned-Area Regression (Portugal)

**Goal**  
Predict the burned area (in hectares) of forest fires in the Montesinho park (NE Portugal) using spatial coordinates and meteorological indices.

**Data**  
UCI “Forest Fires” dataset with 517 records and 13 attributes:

- Spatial: `X`, `Y` (grid coordinates inside Montesinho park).
- Temporal: `month`, `day`.
- Fire weather indices (FWI system): `FFMC`, `DMC`, `DC`, `ISI`.
- Weather: `temp` (°C), `RH` (%), `wind` (km/h), `rain` (mm/m²).
- Target: `area` – burned area in hectares (highly right-skewed, many zeros).

**Key steps**

- Exploratory stats: `describe()`, distribution inspection, skewness and kurtosis for `area`.
- Found extreme skew (`Skew ≈ 12.85`) and heavy tails (`Kurtosis ≈ 194`) → most fires burn < 50 ha, with few very large events.
- Outlier analysis on `area` using z-score (|z| ≥ 3) to inspect extreme events.
- Categorical encoding: one-hot encoding for `month` and `day` via `pd.get_dummies(..., drop_first=True)`.
- Outlier handling:
  - Identified skew/kurtosis issues in `FFMC`, `ISI`, `rain`.
  - Applied `np.log1p` transformations to selected columns.
  - Removed extreme outliers in `FFMC` using z-score filtering.
- Target transformation: `area` transformed with `log1p` to stabilise variance and reduce skew.
- Models:
  - Baseline linear regression (multiple variants of feature subsets) with low explanatory power (R² ≈ 0.02 on train).
  - Polynomial regression (degree 4) on transformed features → R² ≈ 0.99 on training set, indicating strong non-linearity and potential overfitting, but also that the relationship between weather indices and burned area is highly non-linear.

**Tech stack (Python)**  
`pandas`, `numpy`, `matplotlib`, `seaborn`, `scipy.stats`, `scikit-learn`

---

### 2. International Greenhouse Gas Emissions (1990–2018)

**Goal**  
Turn multi-decade greenhouse gas emission tables into country-wise and gas-wise insights, and expose them via clean visualisations and a Shiny-based interface. :contentReference[oaicite:0]{index=0}  

**Data**

Greenhouse Gas (GHG) Inventory Data (1990–2018):

- Countries: 40+ (after dropping the EU aggregate).
- Gases / categories (subset):  
  `CO2`, `CH4`, `N2Os`, `HFC`, `PFCs`, `SF6`, `NF3`, `GHG`, `GHG_indirect_CO2`, `HFC-PFC-mix`.
- Columns: country, year, category, value (kiloton CO₂ equivalent). :contentReference[oaicite:1]{index=1}  

**Data preparation**

- Renamed very long `category` strings into compact gas codes using `dplyr::mutate(..., recode(...))`.
- Normalised country names:
  - Lowercased all country labels.
  - Collapsed variants: e.g. `russian federation → russia`, `united kingdom → uk`, `united states of america → usa`.
  - Removed `european union` aggregate to keep analysis purely country-level.
- Cleaned numeric field: removed thousands separators with `gsub(",", "", value)` and cast to `numeric`. :contentReference[oaicite:2]{index=2}  

**Analysis**

- Gas-wise totals:
  - Computed total emissions per gas (1990–2018) via `group_by(category)` + `summarise(total_value = sum(value))`.
  - Found `GHG`, `GHG_indirect_CO2`, `CO2`, and `CH4` dominate global emissions. :contentReference[oaicite:3]{index=3}  
- Temporal trends:
  - Grouped by `category, year` to plot emission trajectories over time.
  - Observed sustained high levels for `GHG` and `CO2`, with notable declines for some gases in later years for specific regions.
- Country-wise gas analysis:
  - For each of the top 4 gases (`GHG`, `GHG_indirect_CO2`, `CO2`, `CH4`), computed total emissions by country and ranked them.
  - USA, Russia, Japan, and Germany emerged as principal emitters of the major gases; Russia leads CH₄ emissions. :contentReference[oaicite:4]{index=4}  
- Deep dives for top emitters:
  - Built country-specific time series for USA, Russia, Japan, Germany, comparing per-gas curves.
  - Computed each country’s per-gas percentage share across 1990–2018 (e.g., USA GHG share vs its own total emissions).
  - Aggregated “top 4 countries” vs “all others” to quantify how much of global emissions is concentrated in a small set of nations. :contentReference[oaicite:5]{index=5}  

**Visualisation and app**

- Static plots with `ggplot2`:
  - Total emissions by gas.
  - Gas trends over time.
  - Country-level bar charts and pie charts for top emitters.
  - Combined comparison of top countries across major gases.
- Shiny app:
  - Wraps the key plots into a simple interactive UI so users can explore emissions by gas and country without touching the code. :contentReference[oaicite:6]{index=6}  

**Tech stack (R)**  
`dplyr`, `ggplot2`, `shiny`, `rmarkdown`, `knitr`

---

## Suggested repo structure

```text
/forest_fires/
  forest_fires_analysis.ipynb
