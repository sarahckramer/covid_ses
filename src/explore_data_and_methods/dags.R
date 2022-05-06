# ---------------------------------------------------------------------------------------------------------------------
# Code to create DAGs describing the possible relationships between outcomes and covariates
# ---------------------------------------------------------------------------------------------------------------------
#
# To view the DAG, uncomment and copy the code in lines 9-37, go to http://dagitty.net/dags.html, paste the
# code in the box marked "Model Code," and click "Update DAG."
#
#  # Hypothesized relationships:
# dag {
#   bb="-6.844,-5.745,7.992,5.415"
#   "% Aged 18-64" [exposure,pos="1.552,1.502"]
#   "% Employees in Production Jobs" [exposure,pos="-5.364,2.550"]
#   "% Employees in Service Jobs" [exposure,pos="-5.581,0.804"]
#   "% Less than 18" [exposure,pos="1.035,0.539"]
#   "COVID-19 Cases" [outcome,pos="-2.260,0.844"]
#   "COVID-19 Deaths (Given Cases)" [outcome,pos="3.465,-1.454"]
#   "Distance to pharmacies" [exposure,pos="-4.980,-1.002"]
#   "Hospital Beds per Population" [exposure,pos="3.457,-3.698"]
#   "Living Area (m^2) per Population" [exposure,pos="-1.947,4.163"]
#   "Population Density" [exposure,pos="-3.943,3.902"]
#   "SE Deprivation" [exposure,pos="-0.363,-2.495"]
#   "Spots in Care Homes" [exposure,pos="-0.052,-1.363"]
#   "% Aged 18-64" -> "COVID-19 Cases"
#   "% Aged 18-64" -> "COVID-19 Deaths (Given Cases)"
#   "% Employees in Production Jobs" -> "COVID-19 Cases"
#   "% Employees in Service Jobs" -> "COVID-19 Cases"
#   "% Less than 18" -> "COVID-19 Cases"
#   "% Less than 18" -> "COVID-19 Deaths (Given Cases)"
#   "Distance to pharmacies" -> "COVID-19 Cases"
#   "Hospital Beds per Population" -> "COVID-19 Deaths (Given Cases)"
#   "Living Area (m^2) per Population" -> "COVID-19 Cases"
#   "Population Density" -> "COVID-19 Cases"
#   "SE Deprivation" -> "COVID-19 Cases"
#   "SE Deprivation" -> "COVID-19 Deaths (Given Cases)"
#   "Spots in Care Homes" -> "COVID-19 Cases"
#   "Spots in Care Homes" -> "COVID-19 Deaths (Given Cases)"
# }
