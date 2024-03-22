<b>Final Project</b>

The purpose of this project is to provide the viewer with a strong foundational understanding of general disparities across Chicago, the effect of indicators on SNAP enrollment, and how to address barriers to SNAP enrollment in their own community. To view the visualizations, run the R file titled ui.R or server.R which can be found in the snap_enrollment folder. The app can also be viewed here: https://amelmer.shinyapps.io/snap_enrollment/

------

<b>Description of included files</b>: <br/>
Chicago Health Atlas Data Download - Community areas.csv: the data downloaded from the Chicago Health Atlas <br/>
data cleaning.R: the R file containing the code used to clean the downloaded data <br/>

Within national data folder:<br/>
ACSDP1Y2022.DP02-2024-03-02T185128.csv: demographic data downloaded from the American Community Survey <br/>
ACSDP1Y2022.DP03-2024-03-02T185055.csv: public benefits data downloaded from the American Community Survey <br/>
ACSDP1Y2022.DP05-2024-03-02T185027.csv: race and ethnicity data downloaded from the American Community Survey <br/>
ACSST1Y2022.S1701-2024-03-02T185232.csv: poverty rate data from the American Community Survey <br/>
data.csv: Medicaid enrollment data downloaded from the USDA
household_pie2022.xlsx: food insecurity rate data downloaded from Data.Medicaid.gov

Within snap_enrollment folder: <br/>
ui.R: the ui code to run the Shiny app <br/>
server.R: the server code to run the Shiny app <br/>
chicago_community_areas.geojson: the JSON file used to generate the map of Chicago <br/>
chicago_community_data.csv: the csv exported at the end of the data cleaning process <br/>
lists-and-dictionaries: the R file containing all of the dictionaries and lists referenced by the Shiny app code

Within www: <br/>
mytheme.css: the custom css for the Shiny app <br/>

--------

<b>Required R packages</b>: <br/>
tidyverse <br/>
shiny <br/>
sf <br/>
colorspace <br/>
ggiraph <br/>
stargazer
