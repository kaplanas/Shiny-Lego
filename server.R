shinyServer(function(input, output) {
  
  #############################################################################
  #############################################################################
  ## Demographics                                                            ##
  #############################################################################
  #############################################################################
  
  #############################################################################
  # Gender and ethnicity                                                      #
  #############################################################################
  
  # Create the demographics circle graph.
  demographics.circle.graph = reactive({
    # Store the themes and genders chosen by the user in variables with
    # shorter names.
    selected.themes = input$demographicsCircleThemePicker
    selected.genders = input$demographicsCircleGenderPicker
    # Determine what we're going to facet by: theme, gender, both, or neither.
    # Filter if necessary.
    temp.heads.df = heads.df %>%
      mutate(facet.name = "",
             facet.theme = "",
             facet.other = "")
    if(length(selected.themes) > 0) {
      temp.heads.df = temp.heads.df %>%
        filter(theme.name %in% gsub(" \\([0-9]+\\)$", "", selected.themes)) %>%
        mutate(facet.name = theme.name,
               facet.theme = theme.name)
    }
    if(length(selected.genders) > 0) {
      temp.heads.df = temp.heads.df %>%
        filter(gender %in% selected.genders) %>%
        mutate(facet.name = paste(facet.name,
                                  ifelse(length(selected.themes) > 0, ", ", ""),
                                  gender,
                                  sep = ""),
               facet.other = gender)
    }
    circle.graph(temp.heads.df,
                 facet.by.theme = length(selected.themes) > 0,
                 facet.by.other = length(selected.genders) > 0)
  })
  
  # The actual demographics graph.
  output$demographicsCirclePlot = renderHighchart({
    demographics.circle.graph()
  })
  output$demographicsCirclePlotUI = renderUI({
    highchartOutput("demographicsCirclePlot",
                    height = circle.plot.height(length(input$demographicsCircleThemePicker),
                                                length(input$demographicsCircleGenderPicker)),
                    width = circle.plot.width(length(input$demographicsCircleThemePicker),
                                              length(input$demographicsCircleGenderPicker)))
  })
  
  #############################################################################
  # Ethnic diversity and gender parity                                        #
  #############################################################################
  
  output$demographicsDiversity = renderHighchart({
    # Get one row per theme, with the relevant columns.
    temp.heads.df = heads.df %>%
      dplyr::select(theme.name, theme.num.parts, theme.ethnic.diversity,
                    theme.pct.female) %>%
      filter(theme.num.parts > 1) %>%
      distinct()
    # Add a "measure" column with the measure specified by the user.
    if(input$demographicsMeasurePicker == "Ethnic diversity") {
      temp.heads.df = temp.heads.df %>%
        mutate(measure = theme.ethnic.diversity)
    } else if(input$demographicsMeasurePicker == "Percent female") {
      temp.heads.df = temp.heads.df %>%
        mutate(measure = theme.pct.female)
    }
    temp.heads.df = temp.heads.df %>%
      filter(!is.na(measure))
    # Sort by the column specified by the user.
    if(input$demographicsOrderPicker == "Measure") {
      temp.heads.df = temp.heads.df %>%
        arrange(desc(measure), theme.name)
    } else if(input$demographicsOrderPicker == "Number of pieces") {
      temp.heads.df  = temp.heads.df %>%
        arrange(desc(theme.num.parts), theme.name)
    } else if(input$demographicsOrderPicker == "Theme name") {
      temp.heads.df = temp.heads.df %>%
        arrange(theme.name)
    }
    temp.heads.df$theme.name = factor(temp.heads.df$theme.name,
                                      levels = temp.heads.df$theme.name)
    # Get log num parts, and the maximum over the dataset.
    temp.heads.df = temp.heads.df %>%
      mutate(theme.num.parts.col = log(theme.num.parts) / max(log(theme.num.parts)))
    # Set the format for the tooltip.
    point.format = paste(
      " ({point.num_pieces} pieces)</span><br/><span>",
      input$demographicsMeasurePicker,
      ":\u00A0{point.y}</span>",
      sep = ""
    )
    # Make the plot.
    highchart() %>%
      hc_chart(type = "bar") %>%
      hc_xAxis(categories = temp.heads.df$theme.name) %>%
      hc_add_series(pointPadding = 0,
                    data = temp.heads.df %>%
                      mutate(y = measure,
                             num_pieces = theme.num.parts),
                    colorByPoint = T,
                    colors = rgb(colorRamp(c("white", "black"))(temp.heads.df$theme.num.parts.col),
                                 maxColorValue = 255),
                    borderColor = "#000000") %>%
      hc_tooltip(headerFormat = "<span><b>{point.key}</b>",
                 pointFormat = point.format,
                 valueDecimals = 2) %>%
      hc_legend(enabled = F)
  })
  
  #############################################################################
  # Find sets with a specific ethnicity or gender                             #
  #############################################################################
  
  output$demographicsSets = renderDataTable({
    # Store the themes and genders chosen by the user in variables with
    # shorter names.
    selected.themes = input$demographicsSetThemePicker
    selected.ethnicities = input$demographicsSetEthnicityPicker
    selected.genders = input$demographicsSetGenderPicker
    # Get all the unique hexadecimal colors and, for each one, whether text
    # printed over that color should be black or white.
    unique.colors = sort(unique(heads.df$color.hex))
    text.color = data.frame(heads.df %>%
                              select(color.hex, text.color.hex) %>%
                              distinct() %>%
                              arrange(color.hex))$text.color.hex
    # Get the dataset to display.  Filter if necessary.
    temp.heads.df = heads.df %>%
      dplyr::select(theme.name, set.name, part.name, gender,
                    color.name, color.hex, num.parts)
    if(length(selected.themes) > 0) {
      temp.heads.df = temp.heads.df %>%
        filter(theme.name %in% gsub(" \\([0-9]+\\)$", "", selected.themes))
    }
    if(length(selected.ethnicities) > 0) {
      temp.heads.df = temp.heads.df %>%
        filter(color.name %in% selected.ethnicities)
    }
    if(length(selected.genders) > 0) {
      temp.heads.df = temp.heads.df %>%
        filter(gender %in% selected.genders)
    }
    # Display the dataset.
    datatable(temp.heads.df,
              options = list(pageLength = 100,
                             columnDefs = list(list(targets = 5,
                                                    visible = F))),
              rownames = F,
              colnames = c("Theme", "Set", "Part", "Gender", "Color",
                           "Color hex", "Number of pieces")) %>%
      formatStyle("color.hex", target = "row",
                  backgroundColor = styleEqual(unique.colors, unique.colors),
                  color = styleEqual(unique.colors, text.color))
  })
  
  #############################################################################
  #############################################################################
  ## Fashion                                                                 ##
  #############################################################################
  #############################################################################
  
  #############################################################################
  # Hair                                                                      #
  #############################################################################
  
  # Create the hair treemap.
  hair.treemap = reactive({
    # Store the themes chosen by the user in a variable.
    selected.themes = input$hairTreemapThemePicker
    # Filter on theme, if specified by the user.
    temp.hair.df = hair.style.df
    if(length(selected.themes) > 0) {
      temp.hair.df = temp.hair.df %>%
        filter(theme.name %in% gsub(" \\([0-9]+\\)$", "", selected.themes))
    } else {
      temp.hair.df = temp.hair.df %>%
        mutate(theme.name = "")
    }
    # Use the user selections to determine which levels to plot in which order.
    level.settings = list(
      list(level = 1,
           dataLabels = list(enabled = T)),
      list(level = 2,
           dataLabels = list(enabled = T)),
      list(level = 3,
           dataLabels = list(enabled = F))
    )
    if(input$hairTreemapOrderPicker == "style.first") {
      temp.hair.df = temp.hair.df %>%
        mutate(id.level.1 = tolower(style),
               name.level.1 = style,
               color.level.1 = NA,
               opacity.level.1 = 0,
               id.level.2 = tolower(paste(style, color.hex)),
               name.level.2 = color.name,
               color.level.2 = color.hex,
               opacity.level.2 = 1)
      level.settings[[2]][["dataLabels"]][["enabled"]] = F
    } else if(input$hairTreemapOrderPicker == "color.first") {
      temp.hair.df = temp.hair.df %>%
        mutate(id.level.1 = tolower(color.hex),
               name.level.1 = color.name,
               color.level.1 = color.hex,
               opacity.level.1 = 1,
               id.level.2 = tolower(paste(color.hex, style)),
               name.level.2 = style,
               color.level.2 = color.hex,
               opacity.level.2 = 0)
      level.settings[[1]][["dataLabels"]][["enabled"]] = F
    }
    # Add a third level for pieces.
    temp.hair.df = temp.hair.df %>%
      mutate(id.level.3 = tolower(paste(id.level.2, part.name)),
             name.level.3 = part.name,
             color.level.3 = color.level.2,
             opacity.level.3 = NA)
    # Create the plot.
    treemap.graph(temp.hair.df, level.settings)
  })
  
  # The actual hair graph.
  output$hairTreemapUI = renderUI({
    hair.treemap()
  })
  
  #############################################################################
  # Clothing                                                                  #
  #############################################################################
  
  # Create the clothing treemap.
  clothes.treemap = reactive({
    # Store the themes chosen by the user in a variable.
    selected.themes = input$clothesTreemapThemePicker
    # Filter on theme, if specified by the user.
    temp.clothes.df = clothes.type.df
    if(length(selected.themes) > 0) {
      temp.clothes.df = temp.clothes.df %>%
        filter(theme.name %in% gsub(" \\([0-9]+\\)$", "", selected.themes))
    } else {
      temp.clothes.df = temp.clothes.df %>%
        mutate(theme.name = "")
    }
    # Start with upper vs. lower as the top level.
    temp.clothes.df = temp.clothes.df %>%
      mutate(id.level.1 = tolower(upper.lower),
             name.level.1 = case_when(upper.lower == "upper" ~ "Upper",
                                      upper.lower == "lower" ~ "Lower",
                                      T ~ ""),
             color.level.1 = NA,
             opacity.level.1 = 0)
    # Use the user selections to determine which levels to plot in which order.
    level.settings = list(
      list(level = 1,
           dataLabels = list(enabled = F),
           borderWidth = 20,
           layoutStartingDirection = "horizontal"),
      list(level = 2,
           dataLabels = list(enabled = T)),
      list(level = 3,
           dataLabels = list(enabled = T)),
      list(level = 4,
           dataLabels = list(enabled = F))
    )
    if(input$clothesTreemapOrderPicker == "type.first") {
      temp.clothes.df = temp.clothes.df %>%
        mutate(id.level.2 = tolower(paste(upper.lower, type)),
               name.level.2 = type,
               color.level.2 = NA,
               opacity.level.2 = 0,
               id.level.3 = tolower(paste(upper.lower, type, color.hex)),
               name.level.3 = color.name,
               color.level.3 = color.hex,
               opacity.level.3 = 1)
      level.settings[[3]][["dataLabels"]][["enabled"]] = F
    } else if(input$clothesTreemapOrderPicker == "color.first") {
      temp.clothes.df = temp.clothes.df %>%
        mutate(id.level.2 = tolower(paste(upper.lower, color.hex)),
               name.level.2 = color.name,
               color.level.2 = color.hex,
               opacity.level.2 = 1,
               id.level.3 = tolower(paste(upper.lower, color.hex, type)),
               name.level.3 = type,
               color.level.3 = color.hex,
               opacity.level.3 = 0)
      level.settings[[2]][["dataLabels"]][["enabled"]] = F
    }
    # Add a fourth level for pieces.
    temp.clothes.df = temp.clothes.df %>%
      mutate(id.level.4 = tolower(paste(id.level.3, part.name)),
             name.level.4 = part.name,
             color.level.4 = color.level.2,
             opacity.level.4 = NA)
    # Create the plot.
    treemap.graph(temp.clothes.df, level.settings)
  })
  
  # The actual clothing graph.
  output$clothesTreemapUI = renderUI({
    clothes.treemap()
  })
  
})
