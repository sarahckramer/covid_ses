# Socioeconomic correlates of COVID-19 outcomes in Germany

Here we explore various demographic, socioeconomic, and healthcare-related variables and their associations with COVID-19 incidence and case fatality rates over the first five waves of the pandemic in Germany. We employ an ecological study design at the district (_Landkreis_) level, and model the data using generalized additive models (GAMs).

Directory Structure
-------------------
* src
  * format_data
  * fit_models
  * functions

Required Packages
-----------------
The following packages are used in this repository:

* tidyverse (version 2.0.0)
* testthat (version 3.2.1.1)
* gridExtra (version 2.3)
* ISOweek (version 0.6.2)
* jsonlite (version 1.8.8)
* mgcv (version 1.9.1)
* sf (version 1.0.16)
* pomp (version 5.8)
* spdep (version 1.3.3)
* Rcpp (version 1.0.12)
* DHARMa (version 0.4.6)
* patchwork (version 1.2.0)
* viridis (version 0.6.5)
* psych (version 2.4.3)
* geosphere (version 1.5.18)
* vegan (version 2.6.4)
* ncf (version 1.3.2)

All analyses were conducted in R version 4.4.0.

Data
----
#### COVID-19 Data

Data on COVID-19 cases and deaths by district and age group, as well as data on population sizes, were obtained from the [Corona Data Platform](https://www.healthcare-datenplattform.de/).

#### German Index of Socioeconomic Deprivation (GISD)

District-level socioeconomic position was measured using the GISD, which encompasses indicators on employment, income, and educational attainment. Data can be downloaded [here](https://github.com/robert-koch-institut/German_Index_of_Socioeconomic_Deprivation_GISD).

#### Other demographic and socioeconomic data

Data on all other key predictor variables were obtained from the database "Indicators and Maps for Spatial and Urban Development" (INKAR), available [here](https://www.inkar.de/).

#### Vaccination data

Vaccination rates for each district were estimated from vaccination rates at a larger, regional spatial scale, as described in [Koslow et al. (2022)](https://journals.plos.org/ploscompbiol/article?id=10.1371/journal.pcbi.1010054). Code to obtain these estimates can be found [here](https://github.com/SciCompMod/memilio/tree/main). Original vaccination data were obtained from the [Robert Koch Institute](https://github.com/robert-koch-institut/COVID-19-Impfungen_in_Deutschland).

#### Commuting data

Code to obtain the formatted commuting data, which include both data collected by Germany's Federal Employment Agency and estimates for district-district pairs with very little commuting, can be found [here](https://github.com/SciCompMod/memilio/tree/main) (the same repository as for the vaccination data).

Formatting Data
---------------
All data formatting code can be found in the folder "format_data." Files are numbered in the order in which they should be run.

* COVID-19 case and death data are formatted and age-standardized by "01_format_case_data_AGE_STAND.R" and "02_format_mortality_data_AGE_STAND.R", respectively. The two resulting datasets are then joined by "03_combine_death_case_data.R".
  * Key functions for data processing are contained in "functions/data_processing_fxns.R".
* In "04_define_waves.R", data are split into discrete waves, and the incidence and case fatality rates for each wave are calculated.
* Data on all independent variables are formatted in "05_format_covariate_data.R".

Model Fitting and Evaluation
----------------------------
Code to conduct the main analyses are found in "fit_models."

* Because past work identified a change in the association between socioeconomic position (SEP) and COVID-19 outcomes occurring partway through the first pandemic wave, we split wave 1 into two partial waves for all analyses. The ideal timepoint for this split is found in "00_choose_cutoff_wave1.R".
  * Code to load, format, and merge all data, including map data, is found in "functions/load_data.R".
* In "01_choose_k.R", we explore initial choices for the value of k, the basis dimension, for the spatial smooth. Appropriate choice of the k-value is important because values that are too low may fail to adquately account for the lack of spatial independence in the data, while values that are too high may lead to overfitting.
* All model fitting is conducted in "02_fit_models.R". Note that this code has been updated to reflect the final model equations, after performing all model checks described in the next two steps.
* In "03_model_comparison.R", we explored various possible model improvement, including interaction terms and alternative spatial smoothers.
* In "04_check_residuals.R", we check the quality of the model fit to the data using the [DHARMa package](https://cran.r-project.org/web/packages/DHARMa/vignettes/DHARMa.html), and confirm that there is no remaining spatial autocorrelation in the model residuals using Moran's I.
  * A function to run several analyses from the DHARMa package, as well as functions to conduct analyses included in the next step, can be found in "functions/assess_results_fxns.R".
* Associations between predictors of interest and both incidence and case fatality rates are calculated and plotted in "05_assess_and_plot_results.R". Some basic plots of the data and descriptive analyses are also included here.

Spatial Analysis
----------------
In "06_assess_spatial_relationships.R", we test whether the underlying patterns in COVID-19 incidence, as inferred from the models fit above, are associated with between-district commuting flows, after controlling for geographic distance.

Sensitivity Analysis
--------------------
To check whether shifts in the direction of the assocation between SEP and COVID-19 incidence or case fatality rates occurred during any waves other than wave 1, we also split all other waves into two partial waves, then fit our models to each of these partial models. Code to do this and to evaluate the results is found in "07_model_partial_waves.R".

To test whether the associations between our predictors and outcomes of interest varied by age group, we also attempted an age-stratified analysis. Code for this sensitivity analysis is found in "08_comp_age_stratified.R".

Finally, we tested the effect of excluding the smooth on latitude and longitude on our results. Code for this analysis is found in "09_comp_without_spatial.R".
