
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

# Server
server <- function(input, output, session) {
  
  output$noenroll_predictor_facet <- renderGirafe({
    
    color_palette = c("#A6A6D6", "#F9844D", "#658E78")
    
    snap_predictor <- as.character(indicators[input$predictor])
    
    if (snap_predictor != "svi" & snap_predictor != "walkability" & snap_predictor != "roads_rail_airport_proximity_index" & snap_predictor != "firearm_homicide") {
      chicago_data <- chicago_data %>% 
        mutate(tooltip = c(paste0(neighborhood, "\n", as.character(predictor_labels[snap_predictor]), ": ", round(.data[[snap_predictor]], digits = 2), "%\nPercent SNAP-Eligible Not Enrolled: ", round(percent_eligible_no_snap, digits = 2), "%")))
    } else {
      chicago_data <- chicago_data %>% 
        mutate(tooltip = c(paste0(neighborhood, "\n", as.character(predictor_labels[snap_predictor]), ": ", round(.data[[snap_predictor]], digits = 2), "\nPercent SNAP-Eligible Not Enrolled: ", round(percent_eligible_no_snap, digits = 2), "%")))}
    
    facet_chicago_scatter <- chicago_data %>% 
      filter(!is.na(facet_area)) %>% 
      ggplot(aes(x = .data[[snap_predictor]], y = percent_eligible_no_snap)) +
      geom_point_interactive(aes(color = facet_area, tooltip = tooltip)) +
      geom_smooth(method = lm, color = "black") +
      facet_wrap(vars(facet_area)) +
      scale_x_continuous(name = c(paste0(as.character(predictor_labels[snap_predictor]), ifelse(snap_predictor != "svi" & snap_predictor != "walkability" & snap_predictor != "roads_rail_airport_proximity_index" & snap_predictor != "firearm_homicide", " (%)", "")))) + 
      # source: https://stackoverflow.com/questions/27433798/how-can-i-change-the-y-axis-figures-into-percentages-in-a-barplot
      scale_y_continuous(name = "SNAP-Eligible, Not Enrolled (%)") +
      scale_color_manual(values = color_palette) +
      labs(title = c(paste0("Relationship Between ", input$predictor, " and SNAP Enrollment"))) +
      theme_bw() +
      theme(axis.title.x = element_text(size = 15),
            axis.title.y = element_text(size = 15),
            axis.text = element_text(size = 13),
            plot.title = element_text(size = 17, face = "bold"),
            legend.position = 'none',
            strip.text.x = element_text(size = 12),
            strip.background = element_rect(fill="#E8EFE8"))
    
    final_facet <- girafe(ggobj = facet_chicago_scatter,
                          options = opts_sizing(rescale = FALSE),
                          height_svg = 6,
                          width_svg = 12)
    return(final_facet)
  })
  
  output$noenroll_predictor_full <- renderGirafe({
    
    color_palette = c("#CC6967", "#A6A6D6", "#F9844D", "#658E78")
    
    snap_predictor <- as.character(indicators[input$predictor])
    
    if (snap_predictor != "svi" & snap_predictor != "walkability" & snap_predictor != "roads_rail_airport_proximity_index") {
      chicago_data <- chicago_data %>% 
        mutate(tooltip = c(paste0(neighborhood, "\n", as.character(predictor_labels[snap_predictor]), ": ", round(.data[[snap_predictor]], digits = 2), "%\nPercent SNAP-Eligible Not Enrolled: ", round(percent_eligible_no_snap, digits = 2), "%")))
    } else {
      chicago_data <- chicago_data %>% 
        mutate(tooltip = c(paste0(neighborhood, "\n", as.character(predictor_labels[snap_predictor]), ": ", round(.data[[snap_predictor]], digits = 2), "\nPercent SNAP-Eligible Not Enrolled: ", round(percent_eligible_no_snap, digits = 2), "%")))}
    
    full_chicago_scatter <- ggplot(chicago_data, aes(x = .data[[snap_predictor]], y = percent_eligible_no_snap)) +
      geom_point_interactive(aes(color = chicago_area, shape = chicago_area, tooltip = tooltip), size = 2) +
      geom_smooth(method = lm, color = "black", show.legend = FALSE) +
      scale_x_continuous(name = as.character(predictor_labels[snap_predictor]),
                         labels = if (snap_predictor != "svi" & snap_predictor != "walkability" & snap_predictor != "roads_rail_airport_proximity_index"){
                           function(x) paste0(x, "%")
                         } else {
                           labels = function(x) paste0(x)
                         }) + 
      scale_y_continuous(name = "Percent of SNAP-Eligible in\nNeighborhood That Are Not Enrolled",
                         labels = function(x) paste0(x, "%")) +
      scale_color_manual(values = color_palette, labels = c("The Loop", "The North Side", 
                                                            "The South Side", "The West Side"), 
                         name = "Area of Chicago", na.translate = F) +
      scale_shape_discrete(name = "Area of Chicago", labels = c("The Loop", "The North Side", 
                                                                "The South Side", "The West Side"),
                           na.translate = F) +
      labs(title = c(paste0("Relationship Between ", input$predictor, " and SNAP Enrollment"))) +
      theme_bw() +
      theme(axis.title.x = element_text(size = 15),
            axis.title.y = element_text(size = 15),
            axis.text = element_text(size = 13),
            plot.title = element_text(size = 17, face = "bold"),
            legend.text = element_text(size = 13),
            legend.title = element_text(size = 15))
    
    final_plot <- girafe(ggobj = full_chicago_scatter,
                         options = opts_sizing(rescale = FALSE),
                         height_svg = 6,
                         width_svg = 12)
    return(final_plot)
  })
  
  output$chicago_map_full <- renderGirafe({
    
    community_factor <- as.character(indicators[input$indicator])
    
    if (community_factor != "svi" & community_factor != "walkability" & community_factor != "roads_rail_airport_proximity_index" & community_factor != "firearm_homicide") {
      chicago_cp <- chicago_cp %>% 
        mutate(tooltip = c(paste0(neighborhood, ", ", round(.data[[community_factor]], digits = 2), "%")))
    } else {
      chicago_cp <- chicago_cp %>% 
        mutate(tooltip = c(paste0(neighborhood, ", ", round(.data[[community_factor]], digits = 2))))}
    
    chicago_map <- ggplot(data = chicago_cp) +
      geom_sf_interactive(aes(fill = .data[[community_factor]], tooltip = tooltip)) +
      scale_fill_continuous_sequential(palette = "Greens", 
                                       name = "", 
                                       labels = if (community_factor != "svi" & community_factor != "walkability" & community_factor != "roads_rail_airport_proximity_index" & community_factor != "firearm_homicide"){
                                         function(x) paste0(x, "%")
                                       } else {
                                         labels = function(x) paste0(x)
                                       },
                                       guide = guide_colorbar(barwidth = 1),
                                       limits = if (community_factor != "svi" & community_factor != "walkability" & community_factor != "roads_rail_airport_proximity_index" & community_factor != "firearm_homicide") {
                                         c(0, 100)} else {NULL}) +
      labs(title = input$indicator) +
      theme_void() +
      theme(plot.title = element_text(size = 9, face = "bold", hjust = 0.5),
            legend.text = element_text(size = 6))
    
    final_map <- girafe(ggobj = chicago_map,
                        options = opts_sizing(rescale = FALSE))
    return(final_map)
  })
  
  output$community_area_bar <- renderGirafe({
    
    compute_height_svg <- function(community_area_length) {
      base_height <- 6
      extra_height <- ifelse(community_area_length > 2, ceiling((community_area_length - 2) / 2), 0)
      height_svg <- base_height + 3 * extra_height
      return(height_svg)
    }
    
    color_palette = c("loop" = "#CC6967", 
                      "north" = "#A6A6D6", 
                      "south" = "#F9844D", 
                      "west" = "#658E78")
    
    selected_predictors <- as.character(indicators[input$community_predictor])
    
    barplot_data <- pivot_data %>% 
      filter(neighborhood %in% input$community_area & predictor %in% selected_predictors) %>%
      mutate(tooltip_ca = c(paste0(neighborhood, ", ", as.character(predictor_labels[predictor]), ": ", round(percent, digits = 2), "%")),
             tooltip_av = c(paste0("Chicago Average, ", as.character(predictor_labels[predictor]), ": ", round(avg_percent, digits = 2), "%")))
    
    community_area_barplot <- ggplot() + 
      geom_bar_interactive(data = barplot_data, aes(x = predictor, y = avg_percent, tooltip = tooltip_av), 
                           stat = "identity", fill = "#484F48", width = 0.9, alpha = 0.3) + 
      geom_col_interactive(data = barplot_data, aes(x = predictor, y = percent, fill = chicago_area, tooltip = tooltip_ca), position = "dodge", width = 0.7) +
      scale_x_discrete(name = "Predictor of\nSNAP Enrollment",
                       labels = function (x) stringr::str_wrap(as.character(predictor_labels[x]), width = 15)) + 
      scale_y_continuous(name = "Predictor Rate (%)",
                         limits = c(0,100),
                         expand = c(0,1)) +
      facet_wrap(vars(neighborhood), ncol = 2) +
      scale_fill_manual(values = color_palette, name = "Area of Chicago",
                        labels = c("loop" = "The Loop", 
                                   "north" = "The North Side", 
                                   "south" = "The South Side", 
                                   "west" = "The West Side")) +
      labs(title = "Barriers to SNAP Enrollment",
           subtitle = "Chicago average shown in gray.") +
      theme_bw() +
      theme(axis.title.x = element_text(size = 13),
            axis.title.y = element_blank(),
            axis.text.x = element_text(size = 11),
            axis.text.y = element_text(size = 11),
            plot.title = element_text(size = 17, face = "bold"),
            legend.text = element_text(size = 11),
            legend.title = element_blank(),
            legend.margin = margin(0,0,0,0),
            legend.position = "top",
            panel.spacing.x = unit(4, "mm"),
            strip.text.x = element_text(size = 13),
            strip.background = element_rect(fill="#E8EFE8"),
            plot.subtitle = element_text(size = 13)) +
      coord_flip() +
      scale_color_manual(name = NULL, values = c("Average", "gray"))
    
    final_barplot <- girafe(ggobj = community_area_barplot,
                            options = list(opts_sizing(rescale = FALSE)),
                            height_svg = compute_height_svg(length(input$community_area)),
                            width_svg = 12)
    return(final_barplot)
  })
  
  output$indicator_description <- renderText({
    description <- as.character(variable_descriptions[input$indicator])
    return(description)
  })
  
  output$predictor_description <- renderUI({
    indicator_description <- as.character(variable_descriptions[input$predictor])
    predictor_description <- as.character(predictor_descriptions[input$predictor])
    
    full_description <- HTML(indicator_description, "<br/><br/>", predictor_description)
    return(full_description)
  })
  
  output$community_snapenroll <- renderUI({
    percent_no_snap <- chicago_data %>% 
      filter(neighborhood %in% input$community_area) %>% 
      select(c(neighborhood, percent_eligible_no_snap))
    
    snap_enrollment <- HTML(c(paste0("The percent of SNAP-eligible residents in ", "<b>",input$community_area,"</b>", 
                                     " that are enrolled in SNAP is ",  
                                     "<b>",round(100 - percent_no_snap$percent_eligible_no_snap[match(input$community_area, percent_no_snap$neighborhood)], 
                                                 digits = 2), "%.</b><br/>")))
    return(snap_enrollment)
  })
  
  output$chicago_national <- renderGirafe({
    
    color_palette = c("#CC6967", "#A6A6D6", "#F9844D", "#658E78")

    fills <- c(
      alpha(color_palette[1:4], .75)
      )
    
    selected_predictor <- as.character(indicators[input$chicago_national_predictor])
    
    us_value <- pivot_data %>% 
      filter(predictor == selected_predictor & neighborhood == "United States") %>% 
      pull(percent)
    
    chicago_average <- pivot_data %>% 
      filter(predictor == selected_predictor & neighborhood == "United States") %>% 
      pull(mean(avg_percent))
    
    chicago_barplot <- pivot_data %>% 
      filter(predictor == selected_predictor & neighborhood != "United States") %>% 
      mutate(tooltip = c(paste0(neighborhood, ", ", round(percent, digits = 2), "%"))) %>% 
      ggplot(aes(x = reorder(neighborhood, desc(percent)), y = percent, fill = chicago_area)) +
      geom_col_interactive(aes(tooltip = tooltip)) +
      scale_fill_manual(values = fills, name = "Area of Chicago", 
                        labels = c("The Loop", "The North Side", 
                                   "The South Side", "The West Side")) +
      theme_bw() +
      geom_hline(aes(yintercept = us_value, 
                 linetype = "National Rate"), color = "black") +
      geom_hline(aes(yintercept = chicago_average, 
                 linetype = "Chicago Average"), color = "black") + 
      theme(axis.text.x = element_text(angle = 70,
                                       hjust=1)) +
      scale_linetype_manual(name = NULL, values = c("dashed", "solid"), 
                            guide = guide_legend(override.aes = list(color = c("black", "black")))) +
      labs(title = c(paste0("Distribution of ", input$chicago_national_predictor, " Across Chicago")),
           subtitle = "Chicago average and national rate shown as horizontal lines.") +
      scale_x_discrete(name = "Chicago Neighborhood") +
      scale_y_continuous(name = "Predictor Rate", 
                         labels = function(x) paste0(x, "%"),
                         expand = c(0,0)) +
      theme(axis.title.x = element_blank(),
            axis.title.y = element_text(size = 13),
            axis.text.x = element_text(size = 9),
            axis.text.y = element_text(size = 11),
            plot.title = element_text(size = 17, face = "bold"),
            legend.text = element_text(size = 10),
            legend.title = element_text(size = 13),
            plot.subtitle = element_text(size = 12))
    
    final_barplot <- girafe(ggobj = chicago_barplot,
                            options = opts_sizing(rescale = FALSE),
                            height_svg = 6,
                            width_svg = 12)
    
    return(final_barplot)
  })
  
  output$chicago_national_facet <- renderGirafe({
    
    color_palette = c("#A6A6D6", "#F9844D", "#658E78")
    
    fills <- c(
      alpha(color_palette[1:4], .75)
    )
    
    selected_predictor <- as.character(indicators[input$chicago_national_predictor])
    
    facet_labs <- c("North Side", "South Side", "West Side")
    names(facet_labs) <- c("north", "south", "west")
    
    us_value <- pivot_data %>% 
      filter(predictor == selected_predictor & neighborhood == "United States") %>% 
      pull(percent)
    
    chicago_average <- pivot_data %>% 
      filter(predictor == selected_predictor & neighborhood == "United States") %>% 
      pull(mean(avg_percent))
    
    chicago_barplot <- pivot_data %>% 
      filter(predictor == selected_predictor & neighborhood != "United States" & neighborhood != "Loop") %>% 
      mutate(tooltip = c(paste0(neighborhood, ", ", round(percent, digits = 2), "%"))) %>% 
      ggplot(aes(x = reorder(neighborhood, desc(percent)), y = percent, fill = chicago_area)) +
      geom_col_interactive(aes(tooltip = tooltip)) +
      facet_wrap(vars(chicago_area), scales = "free_x", ncol = 3, labeller = labeller(chicago_area = facet_labs)) +
      scale_fill_manual(values = fills, guide = "none") +
      theme_bw() +
      geom_hline(aes(yintercept = us_value, 
                     linetype = "National Rate"), color = "black") +
      geom_hline(aes(yintercept = chicago_average, 
                     linetype = "Chicago Average"), color = "black") + 
      theme(axis.text.x = element_text(angle = 75, hjust=1)) +
      scale_linetype_manual(name = "Comparisons", values = c("dashed", "solid"), 
                            guide = guide_legend(override.aes = list(color = c("black", "black")))) +
      labs(title = c(paste0("Distribution of ", input$chicago_national_predictor, " Across Chicago")),
           subtitle = "Chicago average and national rate shown as horizontal lines.") +
      scale_x_discrete(name = "Chicago Neighborhood") +
      scale_y_continuous(name = "Predictor Rate", 
                         labels = function(x) paste0(x, "%"),
                         expand = c(0,0)) +
      theme(axis.title.x = element_blank(),
            axis.title.y = element_text(size = 13),
            axis.text.x = element_text(size = 8),
            axis.text.y = element_text(size = 11),
            plot.title = element_text(size = 17, face = "bold"),
            legend.text = element_text(size = 10),
            legend.title = element_blank(),
            legend.margin = margin(0,0,0,0),
            legend.position = "top",
            plot.subtitle = element_text(size = 12),
            strip.text.x = element_text(size = 12),
            strip.background = element_rect(fill="#E8EFE8"))
    
    final_barplot_facet <- girafe(ggobj = chicago_barplot,
                            options = opts_sizing(rescale = FALSE),
                            height_svg = 6,
                            width_svg = 12)
    
    return(final_barplot_facet)
  })
  
  output$full_regression <- renderUI({
    
    all_predictors <- lm(data = chicago_data, "percent_eligible_no_snap ~ food_insecurity + poverty + unemployment + public_assistance + disability + single_parent + medicaid + grade9_ed + eviction_rate + community_belonging + nonhispanic_white + college_grad + limited_english_proficiency + noncitizen + foreign_born")
    
    HTML(stargazer(all_predictors, type="html", dep.var.labels = c("SNAP-Eligible, Not Enrolled"), 
                   covariate.labels = c("Food Insecurity Rate", "Poverty Rate", "Unemployment Rate", "Public Assistance", "Disability", 
                                        "Single Parent Households", "Medicaid", "Ninth Grade Education Rate", "Eviction Rate", 
                                        "Community Belonging", "White (Non-Hispanic)", "College Graduation Rate", "Limited English Proficiency", 
                                        "Non-Citizen", "Foreign Born")))
    })
  
  output$significant_regression <- renderUI({
    
    significant_predictors <- lm(data = chicago_data, "percent_eligible_no_snap ~ noncitizen + college_grad + nonhispanic_white + medicaid + public_assistance + poverty")
    
    HTML(stargazer(significant_predictors, type="html", dep.var.labels = c("SNAP-Eligible, Not Enrolled"), covariate.labels = c("Non-Citizen", "College Graduation Rate", "White (Non-Hispanic)", "Medicaid", "Public Assistance", "Poverty Rate")))
    })
  
  observeEvent(input$link_to_map, {
    updateTabsetPanel(session, "snap_enrollment", selected = "Get to Know Chicago")
})
  
  observeEvent(input$link_to_nation, {
    updateTabsetPanel(session, "snap_enrollment", selected = "National Enrollment Predictors")
  })
  
  observeEvent(input$link_to_regression, {
    updateTabsetPanel(session, "snap_enrollment", selected = "Enrollment Predictors in Chicago")
  })
  
  observeEvent(input$link_to_barplots, {
    updateTabsetPanel(session, "snap_enrollment", selected = "Putting it Together")
  })
  
  output$predictor_description_national <- renderUI({
    indicator_description <- as.character(variable_descriptions[input$chicago_national_predictor])
    predictor_description <- as.character(predictor_descriptions[input$chicago_national_predictor])
    
    full_description <- HTML(indicator_description, "<br/><br/>", predictor_description)
    return(full_description)
  })
  
  output$regression_results <- renderUI({
    predictor_description <- as.character(predictor_descriptions[input$predictor])
    regression_result <- as.character(regression_results[input$predictor])
    
    full_results <- HTML(predictor_description, "<br/><br/>", regression_result)
    return(full_results)
  })
  
}