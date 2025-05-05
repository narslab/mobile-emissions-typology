# mobile-emissions-typology
This repository contains the code and data used in the research mobile greenhouse gas emissions typology. We collected per-capita mobile greenhouse gas emissions (PMGE) data and 55 indicators spanning the categories of road network, buildings, socioeconomics, car ownership, commute mode share, transit energy, and climate across 349 metropolitan statistical areas (MSAs). The goal is to understand how PMGE vary by metro area and gain deeper insights into the indicators most relevant for explaining emissions outcome, as well as generate type-specific mitigation strategies and insights.

## Overview
In this reasearch, we present a novel typology of 349 MSAs in the United States and demonstrate its implication for understanding and mitigating PMGE. The following steps were undertaken to complete the research:

1. **Data Collection**
- Obtained  GHG emissions data from the National Emissions Inventory (NEI) of the United States Environmental Protection Agency, which reports emissions data for various GHG (carbon dioxide, methane, nitrous oxide) across all counties in the U.S. We then mapped these counties to their associated MSAs to determine the emissions for each MSA. 
- Identified 55 indicators that are relevant to mobile combustion emissions from across 8 categories

2. **Typology Development**
- Used exploratory factor analysis (EFA) to reduce the observable indicators to 8 underlying factors.
- Used the Gaussian Mixture Model (GMM) approach to perform the clustering using the above 8 factors. 

3. **Emissions prediction**
- Fitted an eXtreme Gradient Boosting (XGBoost) model to predict and explain PMGE outcomes using 54 indicators as inputs (excluding population and replacing GDP with GDP per capita).
- Analyzed the importance of the indicators for predicting PMGE using the (SHapley Additive exPlanations) SHAP framework.

4. **Results and visualization**
- Generated comprehensive analysis and visualizations for emissions distribution, factor loadings, typology, and emissions prediction. 



<p align="center">
  <img src="https://github.com/user-attachments/assets/c7a4f727-7c1e-40fe-8bea-5ad463cdff0c" alt="Scatterplot" width="400"/>
  <img src="https://github.com/user-attachments/assets/49105710-c1a4-4b00-8ad0-9084b769040e" alt="Cluster map" width="600"/>
</p>


*Scatter plots of PMGE against the log scale of 6 most important indicators as
extracted from the SHAP analysis. NY, LA, and SF are short for New York, Log Angeles, and
San Francisco. Local regression trend lines are shown in blue, with shaded areas representing
95% confidence intervals. Typology map of metro areas, with notable principal cities labeled.
*

## Repository Structure

| Directory                    | Description                                                                               |
| ---------------              | ----------------------------------------------------------------------------------------- |
| `bin/jupyter/models`         | Jupyter notebooks for analyzing clustering and prediction model results.                  |
| `bin/jupyter/plots`          | Jupyter notebooks for visualization.                                                      |
| `bin/jupyter/preprocessing`  | Jupyter notebooks for data cleaning and pre-processing.                                   |
| `data/`                      | Contains raw and cleaned data.                                                            |
| `figures/`                   | Visualizations and plots generated from the analysis, such as heatmaps and scatterplots.  |
| `results/`                   | Output matrices, model validation results, and analysis outcomes.                         |

## Usage

 **Emissions Prediction Modeling:**  
   Use the models provided to predict emissions for different MSAs, grouping a MSA to the specific type:

   ```bash
   XGboost-prediction-model.ipynb

```

## Data Sources
- The street network features were obtained using the OSMnx package from OpenStreetMap.
- Commute transit mode, median salary, housing units, employment to population ratio, unemployment rate, household car ownership, and population were obtained from the American Community Survey.
- Gross domestic product across MSAs were obtained from U.S. Bureau of Economic Analysis.
- Transit fuel consumption and mileage were obtained from Federal Transit Administration.
- The climate and weather data were obtained from the National Centers for Environmental Information.

## Key Results
The study found seven distinct types of MSAs and indicated that population density, cooling degree days, the commute mode share of transit and car, total street length, and built-up area proportion are most relevant for predicting per-capita emissions at the MSA level. In particular, we observe that density and transit use play a mitigatory role for PMGE, while car use and cooling degree days have an exarcerbatory impact. By analyzing the relevant indicators and their impacts in each type, we propose potential pathways that could serve as starting points for metro areas in each type to pursue.

## Acknowledgments
This study was supported via the US EPA Climate Pollution Reduction Grants and was partially supported by NSF Grant $\#$2325956.
