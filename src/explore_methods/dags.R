# ---------------------------------------------------------------------------------------------------------------------
# Code to create DAGs describing the possible relationships between outcomes and covariates
# ---------------------------------------------------------------------------------------------------------------------

# # Load libraries:
# library(dagitty)
# 
# # Initial attempt:
# dag {
#   bb="-6.844,-5.745,7.992,5.415"
#   "% 65+" [pos="6.756,-0.963"]
#   "% Apartments in Multi-Family Housing" [exposure,pos="-0.016,3.551"]
#   "% Employees in Service Industry Jobs" [exposure,pos="-5.607,2.303"]
#   "% Female" [pos="5.379,1.359"]
#   "% Foreign Nationals" [exposure,pos="0.719,-0.326"]
#   "COVID-19 Cases" [outcome,pos="-2.260,0.844"]
#   "COVID-19 Deaths (Given Cases)" [outcome,pos="3.465,-1.454"]
#   "Distance to pharmacies" [pos="-5.571,-0.012"]
#   "Employment Ratio (Immigrant:Overall)" [exposure,pos="1.363,1.104"]
#   "Hospital Beds per Population" [pos="3.020,-4.815"]
#   "Living Area (m^2) per Population" [exposure,pos="-1.845,4.485"]
#   "Population Density" [exposure,pos="-3.943,3.902"]
#   "SE Deprivation" [exposure,pos="0.099,-1.982"]
#   "Spots in Care Homes" [pos="5.755,-3.874"]
#   "Stringency/Containment Index" [pos="-4.629,-1.960"]
#   "Within-LK Mobility" [pos="-2.599,-2.702"]
#   "% 65+" -> "COVID-19 Deaths (Given Cases)"
#   "% Apartments in Multi-Family Housing" -> "COVID-19 Cases"
#   "% Employees in Service Industry Jobs" -> "COVID-19 Cases"
#   "% Female" -> "COVID-19 Deaths (Given Cases)"
#   "% Foreign Nationals" -> "COVID-19 Cases"
#   "% Foreign Nationals" -> "COVID-19 Deaths (Given Cases)"
#   "% Foreign Nationals" -> "SE Deprivation"
#   "Distance to pharmacies" -> "COVID-19 Cases"
#   "Employment Ratio (Immigrant:Overall)" -> "COVID-19 Cases"
#   "Employment Ratio (Immigrant:Overall)" -> "COVID-19 Deaths (Given Cases)"
#   "Hospital Beds per Population" -> "COVID-19 Deaths (Given Cases)"
#   "Living Area (m^2) per Population" -> "COVID-19 Cases"
#   "Population Density" -> "COVID-19 Cases"
#   "SE Deprivation" -> "COVID-19 Cases"
#   "SE Deprivation" -> "COVID-19 Deaths (Given Cases)"
#   "Spots in Care Homes" -> "COVID-19 Deaths (Given Cases)"
#   "Stringency/Containment Index" -> "COVID-19 Cases"
#   "Within-LK Mobility" -> "COVID-19 Cases"
# }
# 
# # Controlling for urbanicity?:
# dag {
#   bb="-6.844,-5.745,7.992,5.415"
#   "% 65+" [pos="6.756,-0.963"]
#   "% Apartments in Multi-Family Housing" [exposure,pos="-0.016,3.551"]
#   "% Employees in Service Industry Jobs" [exposure,pos="-5.607,2.303"]
#   "% Female" [pos="5.379,1.359"]
#   "% Foreign Nationals" [exposure,pos="0.719,-0.326"]
#   "COVID-19 Cases" [outcome,pos="-2.260,0.844"]
#   "COVID-19 Deaths (Given Cases)" [outcome,pos="3.465,-1.454"]
#   "Distance to pharmacies" [pos="-5.571,-0.012"]
#   "Employment Ratio (Immigrant:Overall)" [exposure,pos="1.363,1.104"]
#   "Hospital Beds per Population" [pos="3.020,-4.815"]
#   "Living Area (m^2) per Population" [exposure,pos="-1.845,4.485"]
#   "Population Density" [pos="-3.943,3.902"]
#   "SE Deprivation" [exposure,pos="0.099,-1.982"]
#   "Spots in Care Homes" [pos="5.755,-3.874"]
#   "Stringency/Containment Index" [pos="-4.629,-1.960"]
#   "Within-LK Mobility" [pos="-2.599,-2.702"]
#   Urban [pos="-3.967,-5.329"]
#   "% 65+" -> "COVID-19 Deaths (Given Cases)"
#   "% Apartments in Multi-Family Housing" -> "COVID-19 Cases"
#   "% Employees in Service Industry Jobs" -> "COVID-19 Cases"
#   "% Female" -> "COVID-19 Deaths (Given Cases)"
#   "% Foreign Nationals" -> "COVID-19 Cases"
#   "% Foreign Nationals" -> "COVID-19 Deaths (Given Cases)"
#   "% Foreign Nationals" -> "SE Deprivation"
#   "Distance to pharmacies" -> "COVID-19 Cases"
#   "Employment Ratio (Immigrant:Overall)" -> "COVID-19 Cases"
#   "Employment Ratio (Immigrant:Overall)" -> "COVID-19 Deaths (Given Cases)"
#   "Hospital Beds per Population" -> "COVID-19 Deaths (Given Cases)"
#   "Living Area (m^2) per Population" -> "COVID-19 Cases"
#   "Population Density" -> "% Apartments in Multi-Family Housing"
#   "Population Density" -> "COVID-19 Cases"
#   "Population Density" -> "Living Area (m^2) per Population"
#   "SE Deprivation" -> "COVID-19 Cases"
#   "SE Deprivation" -> "COVID-19 Deaths (Given Cases)"
#   "Spots in Care Homes" -> "COVID-19 Deaths (Given Cases)"
#   "Stringency/Containment Index" -> "COVID-19 Cases"
#   "Within-LK Mobility" -> "COVID-19 Cases"
#   Urban -> "% 65+"
#   Urban -> "% Foreign Nationals"
#   Urban -> "Distance to pharmacies"
#   Urban -> "Population Density"
#   Urban -> "Within-LK Mobility"
# }
# 
# # Include tentative relationships:
# dag {
#   bb="-6.844,-5.745,7.992,5.415"
#   "% 65+" [pos="6.756,-0.963"]
#   "% Apartments in Multi-Family Housing" [pos="-0.016,3.551"]
#   "% Employees in Service Industry Jobs" [exposure,pos="-5.607,2.303"]
#   "% Female" [pos="5.379,1.359"]
#   "% Foreign Nationals" [exposure,pos="0.719,-0.326"]
#   "COVID-19 Cases" [outcome,pos="-2.260,0.844"]
#   "COVID-19 Deaths (Given Cases)" [outcome,pos="3.465,-1.454"]
#   "Distance to pharmacies" [pos="-5.571,-0.012"]
#   "Employment Ratio (Immigrant:Overall)" [exposure,pos="1.363,1.104"]
#   "Hospital Beds per Population" [pos="3.020,-4.815"]
#   "Living Area (m^2) per Population" [pos="-1.845,4.485"]
#   "Population Density" [pos="-3.943,3.902"]
#   "SE Deprivation" [exposure,pos="0.099,-1.982"]
#   "Spots in Care Homes" [pos="5.755,-3.874"]
#   "Stringency/Containment Index" [pos="-4.629,-1.960"]
#   "Within-LK Mobility" [pos="-2.599,-2.702"]
#   Urban [pos="-3.967,-5.329"]
#   "% 65+" -> "COVID-19 Cases"
#   "% 65+" -> "COVID-19 Deaths (Given Cases)"
#   "% 65+" -> "Spots in Care Homes"
#   "% Apartments in Multi-Family Housing" -> "COVID-19 Cases"
#   "% Employees in Service Industry Jobs" -> "COVID-19 Cases"
#   "% Female" -> "COVID-19 Deaths (Given Cases)"
#   "% Foreign Nationals" -> "COVID-19 Cases"
#   "% Foreign Nationals" -> "COVID-19 Deaths (Given Cases)"
#   "% Foreign Nationals" -> "SE Deprivation"
#   "Distance to pharmacies" -> "COVID-19 Cases"
#   "Employment Ratio (Immigrant:Overall)" -> "COVID-19 Cases"
#   "Employment Ratio (Immigrant:Overall)" -> "COVID-19 Deaths (Given Cases)"
#   "Hospital Beds per Population" -> "COVID-19 Deaths (Given Cases)"
#   "Living Area (m^2) per Population" -> "COVID-19 Cases"
#   "Population Density" -> "% Apartments in Multi-Family Housing"
#   "Population Density" -> "COVID-19 Cases"
#   "Population Density" -> "Living Area (m^2) per Population"
#   "SE Deprivation" -> "COVID-19 Cases"
#   "SE Deprivation" -> "COVID-19 Deaths (Given Cases)"
#   "SE Deprivation" -> "Within-LK Mobility"
#   "Spots in Care Homes" -> "COVID-19 Cases"
#   "Spots in Care Homes" -> "COVID-19 Deaths (Given Cases)"
#   "Stringency/Containment Index" -> "COVID-19 Cases"
#   "Within-LK Mobility" -> "COVID-19 Cases"
#   Urban -> "% 65+"
#   Urban -> "% Foreign Nationals"
#   Urban -> "Distance to pharmacies"
#   Urban -> "Population Density"
#   Urban -> "SE Deprivation"
#   Urban -> "Within-LK Mobility"
# }
# 
# # Note: If urban areas cause higher mobility, no way to get direct effects of everything w/o controlling for
# # urban or no; if it does not, can control for:
# # %65+, distance to pharmacies, population density (Cases)
# # %65+
# # Can check whether urban areas have higher travel or not