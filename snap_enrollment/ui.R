library(shiny)
library(tidyverse)
library(sf)
library(colorspace)
library(ggiraph)
library(stargazer)

# Read in and clean data
chicago_data <- read_csv("community_data.csv")
chicago_data <- chicago_data %>% 
  mutate(geoid = as.character(geoid))

chicago_json <- st_read("chicago_community_areas.geojson")
chicago_json <- chicago_json %>% 
  rename(geoid = area_num_1)

chicago_cp <- left_join(x = chicago_json, y = chicago_data, by = "geoid")

pivot_data <- chicago_data %>%
  select(!c(geoid, population, latitude, longitude, facet_area) & !ends_with("count")) %>% 
  pivot_longer(!c(neighborhood, chicago_area), names_to = "predictor", values_to = "percent") %>%
  group_by(predictor) %>% 
  mutate(avg_percent = mean(percent[neighborhood != "United States"], na.rm = TRUE))

# read in dictionaries
source("lists-and-dictionaries.R")

# UI
ui <- fluidPage(
  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "mytheme.css")),
                titlePanel("Targeting SNAP Enrollment in Chicago"),
                
                tabsetPanel(
                  id = "snap_enrollment",
                  type = "tabs",
                  tabPanel("Welcome",
                           h3("An Introduction to Food Insecurity and SNAP in Chicago"),
                           p("As of 2018, an estimated 57.4% of eligible households in Illinois were enrolled in the Supplemental Nutrition Assistance Program (SNAP).",tags$sup("[2]"),
                           "Eligibility for SNAP in Illinois is based on gross monthly income and household size. Illinois has dropped the eligibility asset limits for most households.",tags$sup("[9][10]")),
                           p("According to data from 2021, the food insecurity rate in Cook County is 9.2%,",tags$sup("[1]"), " but when looking at food insecurity rates by Chicago neighborhood, that rate can get as high as almost 48%."),
                           tags$blockquote("SNAP improves food security, offers benefits that enable families to purchase healthier diets, and frees up resources that can be used for health-promoting activities and needed medical care.", HTML("<br/>"),
                                           a(href = "https://www.cbpp.org/research/food-assistance/snap-is-linked-with-improved-health-outcomes-and-lower-health-care-costs", h5("Steven Carlson and Joseph Llobrera, Center on Budget and Policy Priorities"), target="_blank")),
                           p("Food-insecure households spend on average, 45% more on health care costs than food-secure households per year. Food insecurity is also linked to higher rates of chronic health conditions. SNAP has been shown to reduce overall food insecurity by 30% and is even more effective for children. 
                           SNAP participants are also more likely to report higher quality of health than eligible non-participants. In addition, children in SNAP-participating households are more likely to visit a doctor for regular checkups than children in eligible non-participant households, and participating adults 
                           spend on average 25% less on health care than those who are eligible but are not receiving SNAP benefits.",tags$sup("[11]")),
                           p("The percent of eligible households that are enrolled in SNAP varies widely throughout Chicago, with rates reaching as high as 87% and as low as 0.1%."),
                           p("In order to increase the rates of SNAP enrollment, it is necessary to target outreach programs to address neighborhood-specific barriers to enrollment. The purpose of this dashboard is to explore data on how barriers to SNAP enrollment vary throughout the city in order to better target SNAP outreach programming."),
                           column(12, actionButton("link_to_map", "Get Started!"), align = "center")),
                  tabPanel("Get to Know Chicago",
                           HTML("<br/>"),
                           sidebarPanel(p("To begin designing a SNAP enrollment outreach program, it is important to understand that Chicago is the most segregated big city in the United States.",tags$sup("[3]"), 
                                          "This means that experiences with food insecurity and other community well-being indicators vary widely throughout the city."), 
                                        p("Use this interactive map to explore how different community well-being indicators and demographics vary throughout Chicago neighborhoods."),
                                        selectInput("indicator",
                                                    label = tags$b("Choose a community well-being indicator:"),
                                                    choices = wellbeing_indicators,
                                                    selected = "Adult Loneliness Rate"),
                                        tags$u("Information About the Chosen Indicator:"),
                                        tags$i(textOutput("indicator_description")),
                                        HTML("<br/>"),
                                        p("Currently, most of the research on predictors of SNAP enrollment has been conducted at the national level. Therefore, an important next step is to gain an understanding of how rates of important enrollment predictors at the national level compare to rates in Chicago."),
                                        p(actionButton("link_to_nation", "Compare Chicago to the Nation"), align = "center")),
                           mainPanel(girafeOutput("chicago_map_full", width = "auto", height = "75%"))),
                  tabPanel("National Enrollment Predictors",
                           HTML("<br/>"),
                           sidebarPanel(p("Indicators that can accurately predict individual-level SNAP enrollment across the country, may not be as effective when predicting SNAP enrollment in Chicago at the community level."),
                                        p("Use this interactive barplot to explore how rates of SNAP enrollment predictors across Chicago compare to rates at the national level."),
                                        radioButtons("facet_select",
                                                     label = tags$b("Choose a view:"),
                                                     choices = c("Full City", "Split by Area"),
                                                     selected = "Full City"),
                                        selectInput("chicago_national_predictor",
                                                    label = tags$b("Choose a predictor:"),
                                                    choices = c("College Graduation Rate", "Disability", "Food Insecurity Rate", 
                                                                "Limited English Proficiency", "Medicaid", "Non-Citizens", "Poverty Rate", 
                                                                "Public Assistance", "Single Parent Households", "Unemployment Rate", 
                                                                "White (Non-Hispanic)"),
                                                    selected = "College Graduation Rate"),
                                        tags$b("Predictor Description:"),
                                        HTML("<br/>"),
                                        tags$i(uiOutput("predictor_description_national")),
                                        HTML("<br/>"),
                                        p("With this preliminary understanding of the distribution of these predictors in Chicago, the next step is to analyze the correlation between these predictors and SNAP enrollment across the city."),
                                        p(actionButton("link_to_regression", "Explore Trends Across Chicago"), align = "center")),
                           mainPanel(conditionalPanel("input.facet_select == 'Split by Area'",
                                                      girafeOutput("chicago_national_facet")),
                                     conditionalPanel("input.facet_select == 'Full City'",
                                                      girafeOutput("chicago_national")))),
                  tabPanel("Enrollment Predictors in Chicago",
                           HTML("<br/>"),
                           sidebarPanel(p("Statistically significant predictors of SNAP enrollment at the individual level for individuals living across the country, may not continue to be significant at the community level in Chicago."),
                                        p("Statistical significance is a minimum level of confidence in the relationship between two variables. In this case, statistical significance will be defined as an at most 5% chance that the correlation between the two variables (percent SNAP-eligible, not enrolled and the selected predictor) is due to chance."),
                                        p("Use this interactive scatter plot to explore the relationship between these predictors and SNAP enrollment across the city."),
                                        selectInput("predictor",
                                                    label = tags$b("Choose a predictor:"),
                                                    choices = c("College Graduation Rate", "Community Belonging", "Disability", 
                                                                "Eviction Rate", "Food Insecurity Rate", "Foreign Born",
                                                                "Limited English Proficiency", "Medicaid", "Ninth Grade Education Rate", 
                                                                "Non-Citizens", "Poverty Rate", "Public Assistance", 
                                                                "Single Parent Households", "Unemployment Rate", "White (Non-Hispanic)"),
                                                    selected = "College Graduation Rate"),
                                        radioButtons("zoom",
                                                     label = tags$b("Choose a view:"),
                                                     choices = c("Full City", "Split by Area"),
                                                     selected = "Full City"),
                                        tags$u("Information About the Chosen Predictor:"),
                                        tags$i(uiOutput("predictor_description")),
                                        HTML("<br/>"),
                                        p("With an understanding of the effect of these predictors at the Chicago community level, the last step in designing a targeted outreach program is to explore which barriers to enrollment are the most prevalent in the area of interest."),
                                        p(actionButton("link_to_barplots", "Explore Neighborhood-level Barriers"), align = "center")),
                           mainPanel(conditionalPanel("input.zoom == 'Split by Area'",
                                                      girafeOutput("noenroll_predictor_facet")),
                                     conditionalPanel("input.zoom == 'Full City'",
                                                      girafeOutput("noenroll_predictor_full")))),
                  tabPanel("Putting it Together",
                           HTML("<br/>"),
                           sidebarPanel("Use this interactive barplot to explore how your community area compares to the city average for predictors of SNAP enrollment across Chicago.", 
                                        br(""),
                                        "The following six predictors have a statistically significant effect on the SNAP enrollment rate in Chicago. Together they explain almost 90% of the variation in SNAP enrollment across the city.*",
                                        br(""),
                                        p(checkboxGroupInput("community_predictor", 
                                                           tags$b("Select your predictor(s) of interest:"),
                                                           choices = c("College Graduation Rate", "Medicaid", "Non-Citizens", 
                                                                       "Poverty Rate", "Public Assistance", "White (Non-Hispanic)"),
                                                           inline = TRUE, selected = c("Medicaid", "Public Assistance", "White (Non-Hispanic)"), width = "80%"),
                                          align = "left"),
                                        selectInput("community_area",
                                                    label = tags$b("Choose your community area(s) of interest:"),
                                                    choices = neighborhoods,
                                                    multiple = TRUE,
                                                    selected = c("Hyde Park")),
                                        HTML("<br/>"),
                                        uiOutput("community_snapenroll"),
                                        br(""),
                                        tags$i("* See References for full regression results.")),
                           mainPanel(girafeOutput("community_area_bar"),
                                     br(""),
                                     wellPanel(
                                       h4("Effect of Predictors:"),
                                       p("In a model where all six of the predictors are included, ", 
                                         tags$b("increases in the public assistance enrollment rate, Medicaid enrollment rate, and the college graduation rate predict decreases in the percentage of SNAP-eligible households in a neighborhood that are not enrolled."), 
                                         "If a household is already receiving other forms of public assistance and is enrolled in Medicaid, it is likely that they meet the qualifications for SNAP and are familiar with how the enrollment process works for federal assistance programs. 
                                         It then follows that if rates of enrollment in these programs are high, enrollment in SNAP will be high as well. If your community area has below average rates of enrollment in public assistance and Medicaid, there may be a lack of knowledge around the qualifications for SNAP and the enrollment process. 
                                         While when just the relationship between college graduation rates and SNAP enrollment is analyzed, an increase in the college graduation rate is correlated with an increase in the percent of SNAP-eligible households that are not enrolled, in combination with the other predictors, this negative effect is likely captured by the percentage of the population that is white. 
                                         An increase in the college graduation rate is likely capturing institutional knowledge and the time and resources necessary to enroll in SNAP."),
                                       p(tags$b("Increases in the non-citizen population, the white (non-Hispanic) population, and the poverty rate predict increases in the percentage of SNAP-eligible households that are not enrolled."), "In areas where a high percentage of the population is non-citizens, individuals may be hesitant to interact with the federal government regardless of if they are eligible for SNAP benefits. 
                                         In addition, there may be language barriers preventing individuals from understanding eligibility requirements and the application process. In areas where there are larger white populations, there tends to be a higher degree of stigma around applying for federal benefits programs and as a result there is also less knowledge around the qualifications and the application process. 
                                         While when the relationship between just SNAP enrollment and the poverty rate is analyzed, an increase in the poverty rate is associated with a decrease in the percent of SNAP-eligible households that are not enrolled, in combination with the other predictors, this positive impact is likely captured by public assistance and Medicaid enrollment rates. 
                                         An increased poverty rate is likely predicting that there is a higher rate of SNAP-eligibility in the area and potentially a lack of the time and resources necessary to enroll in SNAP.")
                                     ))
                           ),
                  tabPanel("References",
                           HTML("<br/>"),
                           tags$b("Research Sources:"),
                           HTML("<br/>"),
                           "[1]", a(href = "https://map.feedingamerica.org/county/2021/overall/illinois/organization/greater-chicago-food-depository", "Feeding America: Food Insecurity Rate in Cook County", target="_blank"), 
                           HTML("<br/>"),
                           "[2]", a(href = "https://www.urban.org/sites/default/files/2023-01/What%20Portion%20of%20Illinois%20Residents%20Eligible%20for%20Safety%20Net%20Benefits%20Receive%20Those%20Benefits.pdf", "Urban Institute: What Portion of Illinois Residents Eligible for Safety Net Benefits Receive Those Benefits?", target="_blank"),
                           HTML("<br/>"),
                           "[3]", a(href = "https://www.wbez.org/stories/chicago-remains-the-most-segregated-big-city-in-america/2ec026b3-b11b-4c68-872b-3a1011b6f457", "WBEZ: Chicago Remains Most Segregated Big City in America", target="_blank"),
                           HTML("<br/>"),
                           "[4]", a(href = "https://doi.org/10.1016/j.jneb.2014.10.005", "Reconsidering the Supplemental Nutrition Assistance Program as Community Development", target="_blank"),
                           HTML("<br/>"),
                           "[5]", a(href = "https://econpapers.repec.org/RePEc:mpr:mprres:10c9952fedd34e34bfd540bf82d64502", "Determinants of Supplemental Nutrition Assistance Program (SNAP) Participation from 2008 to 2012", target="_blank"),
                           HTML("<br/>"),
                           "[6]", a(href = "https://doi.org/10.1096/fasebj.31.1_supplement.445.2", "Association of Supplemental Nutrition Assistance Program Participation with Social Cohesion and Financial Worry based on Race and Ethnicity among U.S. Adults", target="_blank"),
                           HTML("<br/>"),
                           "[7]", a(href = "https://doi.org/10.1080/19320248.2016.1146196", "Factors Associated With Supplemental Nutrition Assistance Program Participation Among the Working Poor: Findings From 2012 American Community Survey", target="_blank"),
                           HTML("<br/>"),
                           "[8]", a(href = "http://dx.doi.org/10.1080/19320248.2016.1146194", "What Factors Influence SNAP Participation? Literature Reflecting Enrollment in Food Assistance Programs From a Social and Behavioral Science Perspective", target="_blank"),
                           HTML("<br/>"),
                           "[9]", a(href = "https://www.google.com/url?q=https://www.dhs.state.il.us/page.aspx?item%3D30357&sa=D&source=docs&ust=1709663430161690&usg=AOvVaw2YUlgNhWMGlzGrV1E9L8yV", "Illinois Department of Human Services: Supplemental Nutrition Assistance Program - SNAP"),
                           HTML("<br/>"),
                           "[10]", a(href = "https://www.dhs.state.il.us/page.aspx?item=15070", "Illinois Department of Human Services: PM 07-04-01: Asset Limits"),
                           HTML("<br/>"),
                           "[11]", a(href = "https://www.google.com/url?q=https://www.cbpp.org/research/food-assistance/snap-is-linked-with-improved-health-outcomes-and-lower-health-care-costs&sa=D&source=docs&ust=1709662990563617&usg=AOvVaw39qYxyysvL40SdrPnFmfM4", "SNAP Is Linked With Improved Health Outcomes and Lower Health Care Costs"),
                           br(""),
                           tags$b("Data Sources:"),
                           tags$ul(
                             tags$li("Chicago-level data was obtained from the", a(href = "https://chicagohealthatlas.org/download", "Chicago Health Atlas", target="_blank")),
                             tags$li("The JSON file used for the map of Chicago was obtained from", a(href = "https://data.cityofchicago.org/Facilities-Geographic-Boundaries/Boundaries-Community-Areas-current-/cauq-8yn6", "The Chicago Data Portal", target="_blank")),
                             tags$li("National-level data on the rates of college graduation, high school graduation, disability, foreign born residents, non-citizen residents, poverty, public assistance, single-parent households, unemployment, and white residents were obtained from ",
                                     a(href = "https://data.census.gov/table", "The United States Census Bureau Data Tables", target="_blank")),
                             tags$li("National-level data on the food insecurity rate was obtained from ",
                                     a(href = "https://www.ers.usda.gov/topics/food-nutrition-assistance/food-security-in-the-u-s/key-statistics-graphics/#foodsecure", "The US Department of Agriculture", target="_blank")),
                             tags$li("National-level data on Medicaid enrollment was obtained from ",
                                     a(href = "https://data.medicaid.gov/dataset/6165f45b-ca93-5bb5-9d06-db29c692a360/data", "Data.Medicaid.gov", target="_blank"))),
                           tags$b("R Packages:"),
                           tags$ul(
                             tags$li("Chang W, Cheng J, Allaire J, Sievert C, Schloerke B, Xie Y, Allen J, McPherson J, Dipert A, Borges B (2022). _shiny: WebApplication Framework for R_. R package version 1.7.4, ", a(href = "https://CRAN.R-project.org/package=shiny", "https://CRAN.R-project.org/package=shiny", target="_blank")),
                             tags$li("Wickham H, Averick M, Bryan J, Chang W, McGowan LD, François R, Grolemund G, Hayes A, Henry L, Hester J, Kuhn M, Pedersen
                                     TL, Miller E, Bache SM, Müller K, Ooms J, Robinson D, Seidel DP, Spinu V, Takahashi K, Vaughan D, Wilke C, Woo K, Yutani H
                                     (2019). “Welcome to the tidyverse.” _Journal of Open Source Software_, *4*(43), 1686. ", a(href = "https://doi.org/10.21105/joss.01686", "doi:10.21105/joss.01686", target="_blank")),
                             tags$li("Pebesma, E., & Bivand, R. (2023). Spatial Data Science: With Applications in R. Chapman and Hall/CRC. ", a(href = "https://doi.org/10.1201/9780429459016", "https://doi.org/10.1201/9780429459016", target="_blank")),
                             tags$li("Zeileis A, Fisher JC, Hornik K, Ihaka R, McWhite CD, Murrell P, Stauffer R, Wilke CO (2020). “colorspace: A Toolbox for
                                     Manipulating and Assessing Colors and Palettes.” _Journal of Statistical Software_, *96*(1), 1-49. ", a(href = "https://doi.org/10.18637/jss.v096.i01", "doi:10.18637/jss.v096.i01", target="_blank")),
                             tags$li("Gohel D, Skintzos P (2023). _ggiraph: Make 'ggplot2' Graphics Interactive_. R package version 0.8.8, ", a(href = "https://CRAN.R-project.org/package=ggiraph", "https://CRAN.R-project.org/package=ggiraph", target="_blank"))
                                     ),
                           radioButtons("regression_choice",
                                        label = tags$b("Regression Results:"),
                                        choices = c("Full Regression", "Statistically Significant Predictors"),
                                        selected = "Full Regression",
                                        inline = TRUE), 
                           conditionalPanel("input.regression_choice == 'Full Regression'",
                                                      uiOutput("full_regression")),
                           conditionalPanel("input.regression_choice == 'Statistically Significant Predictors'",
                                                   uiOutput("significant_regression")),
                           br("")
                           )
                  )
)
