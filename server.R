shinyServer(function(input, output, session) {
  
  #############################################################################
  #############################################################################
  ## Data                                                                    ##
  #############################################################################
  #############################################################################
  
  source("munge_data.R", local = T)
  
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
    temp.heads.df = theme.counts.df %>%
      dplyr::select(theme.name, total.heads, ethnic.diversity, pct.female) %>%
      filter(total.heads > 1) %>%
      distinct()
    # Add a "measure" column with the measure specified by the user.
    if(input$demographicsMeasurePicker == "Ethnic diversity") {
      temp.heads.df = temp.heads.df %>%
        mutate(measure = ethnic.diversity)
    } else if(input$demographicsMeasurePicker == "Percent female") {
      temp.heads.df = temp.heads.df %>%
        mutate(measure = pct.female)
    }
    temp.heads.df = temp.heads.df %>%
      filter(!is.na(measure))
    # Sort by the column specified by the user.
    if(input$demographicsOrderPicker == "Measure") {
      temp.heads.df = temp.heads.df %>%
        arrange(desc(measure), theme.name)
    } else if(input$demographicsOrderPicker == "Number of pieces") {
      temp.heads.df  = temp.heads.df %>%
        arrange(desc(total.heads), theme.name)
    } else if(input$demographicsOrderPicker == "Theme name") {
      temp.heads.df = temp.heads.df %>%
        arrange(theme.name)
    }
    temp.heads.df$theme.name = factor(temp.heads.df$theme.name,
                                      levels = temp.heads.df$theme.name)
    # Get log num parts, and the maximum over the dataset.
    temp.heads.df = temp.heads.df %>%
      mutate(num.parts.col = log(total.heads) / max(log(total.heads)))
    # Set the format for the tooltip.
    point.format = paste(
      " ({point.num_pieces} pieces)</span><br/><span>",
      input$demographicsMeasurePicker,
      ":\u00A0{point.y}</span>",
      sep = ""
    )
    # Make the plot.
    hc = highchart() %>%
      hc_chart(type = "bar") %>%
      hc_xAxis(categories = temp.heads.df$theme.name) %>%
      hc_add_series(pointPadding = 0,
                    data = temp.heads.df %>%
                      mutate(y = measure,
                             num_pieces = total.heads),
                    colorByPoint = T,
                    colors = rgb(colorRamp(c("white", "black"))(temp.heads.df$num.parts.col),
                                 maxColorValue = 255),
                    borderColor = "#000000") %>%
      hc_tooltip(headerFormat = "<span><b>{point.key}</b>",
                 pointFormat = point.format,
                 valueDecimals = 2) %>%
      hc_legend(enabled = F)
    if(input$demographicsMeasurePicker == "Percent female") {
      hc = hc %>%
        hc_yAxis(max = 100)
    }
    hc
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
    # Get the dataset to display.  Filter if necessary.
    temp.heads.df = heads.df %>%
      dplyr::select(theme.name, set.name, part.name, gender,
                    color.name, color.hex, text.color.hex, num.parts)
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
    part.table(temp.heads.df,
               columns.to.hide = c(5, 6),
               column.names = c("Theme", "Set", "Part", "Gender", "Color",
                                "Color hex", "Text color hex",
                                "Number of pieces"))
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
    # Store the themes and types chosen by the user in variables.
    selected.themes = input$clothesTreemapThemePicker
    selected.types = input$clothesTreemapTypePicker
    # Filter on theme and/or type, if specified by the user.
    temp.clothes.df = clothes.type.df
    if(length(selected.themes) > 0) {
      temp.clothes.df = temp.clothes.df %>%
        filter(theme.name %in% gsub(" \\([0-9]+\\)$", "", selected.themes))
    } else {
      temp.clothes.df = temp.clothes.df %>%
        mutate(theme.name = "")
    }
    if(length(selected.types) > 0) {
      temp.clothes.df = temp.clothes.df %>%
        filter(type %in% gsub(" \\([0-9]+\\)$", "", selected.types))
    }
    # Remove "other" pieces, if so specified by the user.
    if(!input$clothesTreemapShowOther) {
      temp.clothes.df = temp.clothes.df %>%
        filter(!grepl("^Other", type))
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

  #############################################################################
  # Accessories                                                               #
  #############################################################################

  # Create the accessories treemap.
  accessories.treemap = reactive({
    # Store the themes chosen by the user in variables.
    selected.themes = input$accessoriesTreemapThemePicker
    # Filter on theme, if specified by the user.
    temp.accessories.df = accessory.parts.df
    if(length(selected.themes) > 0) {
      temp.accessories.df = temp.accessories.df %>%
        filter(theme.name %in% gsub(" \\([0-9]+\\)$", "", selected.themes))
    } else {
      temp.accessories.df = temp.accessories.df %>%
        mutate(theme.name = "")
    }
    # Two levels: accessory, then part.
    level.settings = list(
      list(level = 1,
           dataLabels = list(enabled = T),
           borderWidth = 3,
           borderColor = "black"),
      list(level = 2,
           dataLabels = list(enabled = F))
    )
    temp.accessories.df = temp.accessories.df %>%
      mutate(id.level.1 = tolower(accessory),
             name.level.1 = accessory,
             color.level.1 = "#FFFFFF",
             opacity.level.1 = 0,
             id.level.2 = tolower(paste(accessory, part.name)),
             name.level.2 = part.name,
             color.level.2 = "#FFFFFF",
             opacity.level.2 = 0)
    # Create the plot.
    treemap.graph(temp.accessories.df, level.settings)
  })

  # The actual accessory graph.
  output$accessoriesTreemapUI = renderUI({
    accessories.treemap()
  })

  #############################################################################
  # Find sets with a specific fashion item                                    #
  #############################################################################

  output$fashionSets = renderDataTable({
    # Store the themes, colors, and items chosen by the user in variables with
    # shorter names.
    selected.themes = input$fashionSetThemePicker
    selected.colors = input$fashionSetColorPicker
    selected.hair.styles = input$fashionSetHairStylePicker
    selected.clothing.types = input$fashionSetClothingTypePicker
    selected.accessories = input$fashionSetAccessoryPicker
    # Get the dataset to display.  Filter if necessary.
    temp.fashion.items.df = fashion.items.df %>%
      dplyr::select(theme.name, set.name, part.name, hair.styles,
                    clothing.types, accessories, color.name, color.hex,
                    text.color.hex, total.parts)
    if(length(selected.themes) > 0) {
      temp.fashion.items.df = temp.fashion.items.df %>%
        filter(theme.name %in% gsub(" \\([0-9]+\\)$", "", selected.themes))
    }
    if(length(selected.colors) > 0) {
      temp.fashion.items.df = temp.fashion.items.df %>%
        filter(color.name %in% selected.colors)
    }
    if(length(selected.hair.styles) > 0) {
      temp.fashion.items.df = temp.fashion.items.df %>%
        filter(grepl(paste("(",
                           paste0(selected.hair.styles, collapse = "|"),
                           ")\\b",
                           sep = ""), hair.styles))
    }
    if(length(selected.clothing.types) > 0) {
      temp.fashion.items.df = temp.fashion.items.df %>%
        filter(grepl(paste("(",
                           paste0(selected.clothing.types, collapse = "|"),
                           ")\\b",
                           sep = ""), clothing.types))
    }
    if(length(selected.accessories) > 0) {
      temp.fashion.items.df = temp.fashion.items.df %>%
        filter(grepl(paste("(",
                           paste0(selected.accessories, collapse = "|"),
                           ")\\b",
                           sep = ""), accessories))
    }
    # Display the dataset.
    part.table(temp.fashion.items.df,
               columns.to.hide = c(7, 8),
               column.names = c("Theme", "Set", "Part", "Hair styles",
                                "Clothing types", "Accessories", "Color",
                                "Color hex", "Text color hex",
                                "Number of pieces"))
  })
  
  #############################################################################
  #############################################################################
  ## Moods                                                                   ##
  #############################################################################
  #############################################################################

  #############################################################################
  # Mood counts                                                               #
  #############################################################################

  output$moodsPolarPlotUI = renderUI({
    temp.moods.df = moods.df %>%
      mutate(facet.name = "",
             facet.theme = "",
             facet.other = "")
    temp.moods.df$mood.and.color = factor(paste(temp.moods.df$mood, temp.moods.df$mood.color))
    # Store the themes and genders chosen by the user in variables with
    # shorter names.
    selected.themes = input$moodsPolarThemePicker
    selected.genders = input$moodsPolarGenderPicker
    # Determine what we're going to facet by: theme, gender, both, or neither.
    # Filter if necessary.
    if(length(selected.themes) > 0) {
      temp.moods.df = temp.moods.df %>%
        filter(theme.name %in% gsub(" \\([0-9]+\\)$", "", selected.themes)) %>%
        mutate(facet.name = theme.name,
               facet.theme = theme.name)
    }
    if(length(selected.genders) > 0) {
      temp.moods.df = temp.moods.df %>%
        filter(gender %in% selected.genders) %>%
        mutate(facet.name = paste(facet.name,
                                  ifelse(length(selected.themes) > 0, ", ", ""),
                                  gender,
                                  sep = ""),
               facet.other = gender)
    }
    # If we're faceting by both theme and gender, fix the number of columns at
    # 3.
    num.cols = NULL
    if(length(selected.themes) > 0 & length(selected.genders) > 0) {
      num.cols = 3
    }
    # Get the count for each mood.  Also get the maximum count for each theme,
    # so that we can scale all facets by gender within a theme appropriately.
    temp.moods.df = temp.moods.df %>%
      group_by(mood.and.color, facet.name, facet.theme) %>%
      summarize(total.parts = sum(num.parts)) %>%
      complete(facet.name, facet.theme, mood.and.color,
               fill = list(total.parts = 0)) %>%
      separate(mood.and.color, into = c("mood", "mood.color"), sep = " ")
    # Make the plot.
    lapply(
      sort(unique(as.character(temp.moods.df$facet.name))),
      function(x) {
        current.theme = unique(temp.moods.df$facet.theme[as.character(temp.moods.df$facet.name) == x])
        theme.max = max(temp.moods.df$total.parts[temp.moods.df$facet.theme == current.theme])
        hc = hchart(temp.moods.df %>%
                      filter(as.character(facet.name) == x),
                    "column",
                    hcaes(x = mood, y = total.parts, color = mood.color)) %>%
          hc_chart(polar = T) %>%
          hc_pane(startAngle = 90) %>%
          hc_tooltip(borderWidth = 3,
                     headerFormat = "<span><b>{point.key}:</b></span>\u00A0",
                     pointFormat = "{point.y}") %>%
          hc_plotOptions(column = list(pointPadding = 0,
                                       groupPadding = 0)) %>%
          hc_yAxis(max = theme.max) %>%
          hc_add_theme(hc_theme_null())
        if(x != "") {
          hc = hc %>%
            hc_title(text = x)
        }
        hc
      }
    ) %>%
      hw_grid(ncol = num.cols)
  })

  #############################################################################
  # Mood percents                                                             #
  #############################################################################

  output$moodsBarPlot = renderHighchart({
    # Get one row per theme, with the relevant columns.
    temp.moods.df = moods.df %>%
      inner_join(theme.counts.df, by = c("theme.name")) %>%
      group_by(theme.name, total.heads, mood, mood.color) %>%
      summarize(total.parts = sum(num.parts)) %>%
      ungroup() %>%
      mutate(pct.heads = (total.parts / total.heads) * 100,
             total.heads.col = log(total.heads) / max(log(total.heads))) %>%
      filter(total.heads > 1)
    # Plot the color of the mood, with themes with fewer pieces moved towards
    # white.
    temp.moods.df$color.to.plot = unlist(apply(
      temp.moods.df, 1,
      function(x) {
        rgb(colorRamp(c("#FFFFFF",
                        as.character(x[["mood.color"]])))(as.numeric(x[["total.heads.col"]])),
            maxColorValue = 255)
      }
    ))
    # Filter to the mood specified by the user.
    temp.moods.df = temp.moods.df %>%
      filter(mood == input$moodsMoodPicker) %>%
      filter(!is.na(pct.heads))
    # Sort by the column specified by the user.
    if(input$moodsOrderPicker == "Percent of pieces with mood") {
      temp.moods.df = temp.moods.df %>%
        arrange(desc(pct.heads), theme.name)
    } else if(input$moodsOrderPicker == "Total pieces") {
      temp.moods.df  = temp.moods.df %>%
        arrange(desc(total.heads), theme.name)
    } else if(input$moodsOrderPicker == "Theme name") {
      temp.moods.df = temp.moods.df %>%
        arrange(theme.name)
    }
    temp.moods.df$theme.name = factor(temp.moods.df$theme.name,
                                      levels = temp.moods.df$theme.name)
    # Set the format for the tooltip.
    point.format = paste(
      " ({point.num_pieces} pieces)</span><br/><span>",
      input$moodsMoodPicker,
      ":\u00A0{point.y}%</span>",
      sep = ""
    )
    # Make the plot.
    highchart() %>%
      hc_chart(type = "bar") %>%
      hc_xAxis(categories = temp.moods.df$theme.name) %>%
      hc_add_series(pointPadding = 0,
                    data = temp.moods.df %>%
                      mutate(y = pct.heads,
                             num_pieces = total.heads),
                    colorByPoint = T,
                    colors = temp.moods.df$color.to.plot,
                    borderColor = "#000000") %>%
      hc_tooltip(headerFormat = "<span><b>{point.key}</b>",
                 pointFormat = point.format,
                 valueDecimals = 2) %>%
      hc_legend(enabled = F) %>%
      hc_yAxis(max = 100)
  })

  #############################################################################
  # Find sets with a specific mood                                            #
  #############################################################################

  output$moodsSets = renderDataTable({
    # Store the themes, genders, and moods chosen by the user in variables with
    # shorter names.
    selected.themes = input$moodsSetThemePicker
    selected.genders = input$moodsSetGenderPicker
    selected.moods = input$moodsSetMoodPicker
    # Get the dataset to display.  Filter if necessary.
    temp.moods.df = moods.df %>%
      dplyr::select(theme.name, set.name, part.name, gender, color.name,
                    color.hex, text.color.hex, num.parts, mood) %>%
      distinct() %>%
      group_by(theme.name, set.name, part.name, gender, color.name, color.hex,
               text.color.hex, num.parts) %>%
      summarize(moods = paste0(mood, collapse = ", ")) %>%
      ungroup()
    if(length(selected.themes) > 0) {
      temp.moods.df = temp.moods.df %>%
        filter(theme.name %in% gsub(" \\([0-9]+\\)$", "", selected.themes))
    }
    if(length(selected.genders) > 0) {
      temp.moods.df = temp.moods.df %>%
        filter(gender %in% selected.genders)
    }
    if(length(selected.moods) > 0) {
      temp.moods.df = temp.moods.df %>%
        filter(grepl(paste(paste0(selected.moods, collapse = "|"),
                           "\\b", sep = ""),
                     moods))
    }
    # Display the dataset.
    part.table(temp.moods.df %>%
                 select(theme.name, set.name, part.name, moods, gender,
                        color.name, color.hex, text.color.hex, num.parts),
               columns.to.hide = c(6, 7),
               column.names = c("Theme", "Set", "Part", "Moods", "Gender",
                                "Color", "Color hex", "Text color hex",
                                "Number of pieces"))
  })
  
  #############################################################################
  #############################################################################
  ## Ecology                                                                 ##
  #############################################################################
  #############################################################################

  #############################################################################
  # Plants                                                                    #
  #############################################################################

  # Get the dimensions of the facets needed for the plant dendrograms, and
  # create the facets.
  plant.facet.info = reactive({
    dendrogram.facet.dims(max(length(input$plantsDendrogramThemePicker), 1))
  })
  output$plantsDendrogram = renderUI({
    dendrogram.facet.uis(plant.facet.info(),
                         "plant")
  })

  # Plot the actual dendrograms.
  observe({
    plant.facets = dendrogram.facets(ecology.vertices.vis.df %>%
                                       filter(type == "plant"),
                                     ecology.edges.vis.df %>%
                                       filter(type == "plant"),
                                     input$plantsDendrogramThemePicker,
                                     plant.facet.info(),
                                     "plant")
    for(dendrogram in plant.facets) {
      local({
        my.dendrogram = dendrogram
        output[[my.dendrogram[["id"]]]] = renderVisNetwork({my.dendrogram[["dendrogram"]]})
      })
    }
  })

  #############################################################################
  # Animals                                                                   #
  #############################################################################

  # Get the dimensions of the facets needed for the animal dendrograms, and
  # create the facets.
  animal.facet.info = reactive({
    dendrogram.facet.dims(max(length(input$animalsDendrogramThemePicker), 1))
  })
  output$animalsDendrogram = renderUI({
    dendrogram.facet.uis(animal.facet.info(),
                         "animal")
  })

  # Plot the actual dendrograms.
  observe({
    animal.facets = dendrogram.facets(ecology.vertices.vis.df %>%
                                        filter(type == "animal"),
                                      ecology.edges.vis.df %>%
                                        filter(type == "animal"),
                                      input$animalsDendrogramThemePicker,
                                      animal.facet.info(),
                                      "animal")
    for(dendrogram in animal.facets) {
      local({
        my.dendrogram = dendrogram
        output[[my.dendrogram[["id"]]]] = renderVisNetwork({my.dendrogram[["dendrogram"]]})
      })
    }
  })

  #############################################################################
  # Species diversity                                                         #
  #############################################################################

  output$ecologyDiversity = renderHighchart({
    # Get one row per theme, with the relevant columns.
    temp.ecology.df = theme.counts.df %>%
      dplyr::select(theme.name, total.plants, plant.species.diversity,
                    total.animals, animal.species.diversity) %>%
      filter(total.plants > 1 | total.animals > 1) %>%
      distinct()
    # Add "measure" and "total" columns with the measure specified by the user
    # and the corresponding total.
    if(input$ecologyMeasurePicker == "Species diversity of plants") {
      temp.ecology.df = temp.ecology.df %>%
        mutate(measure = plant.species.diversity,
               total = total.plants)
    } else if(input$ecologyMeasurePicker == "Species diversity of animals") {
      temp.ecology.df = temp.ecology.df %>%
        mutate(measure = animal.species.diversity,
               total = total.animals)
    }
    temp.ecology.df = temp.ecology.df %>%
      filter(!is.na(measure))
    # Sort by the column specified by the user.
    if(input$ecologyOrderPicker == "Measure") {
      temp.ecology.df = temp.ecology.df %>%
        arrange(desc(measure), theme.name)
    } else if(input$ecologyOrderPicker == "Number of pieces") {
      temp.ecology.df  = temp.ecology.df %>%
        arrange(desc(total), theme.name)
    } else if(input$ecologyOrderPicker == "Theme name") {
      temp.ecology.df = temp.ecology.df %>%
        arrange(theme.name)
    }
    temp.ecology.df$theme.name = factor(temp.ecology.df$theme.name,
                                        levels = temp.ecology.df$theme.name)
    # Get log num parts, and the maximum over the dataset.
    temp.ecology.df = temp.ecology.df %>%
      mutate(num.parts.col = log(total) / max(log(total)))
    # Set the format for the tooltip.
    point.format = paste(
      " ({point.num_pieces} ",
      ifelse(input$ecologyMeasurePicker == "Species diversity of plants",
             "plant", "animal"),
      " pieces)</span><br/><span>",
      input$ecologyMeasurePicker,
      ":\u00A0{point.y}</span>",
      sep = ""
    )
    # Make the plot.
    hc = highchart() %>%
      hc_chart(type = "bar") %>%
      hc_xAxis(categories = temp.ecology.df$theme.name) %>%
      hc_add_series(pointPadding = 0,
                    data = temp.ecology.df %>%
                      mutate(y = measure,
                             num_pieces = total),
                    colorByPoint = T,
                    colors = rgb(colorRamp(c("white", "black"))(temp.ecology.df$num.parts.col),
                                 maxColorValue = 255),
                    borderColor = "#000000") %>%
      hc_tooltip(headerFormat = "<span><b>{point.key}</b>",
                 pointFormat = point.format,
                 valueDecimals = 2) %>%
      hc_legend(enabled = F)
    hc
  })

  #############################################################################
  # Find sets with a specific plant or animal                                 #
  #############################################################################

  output$ecologySets = renderDataTable({
    # Store the themes and colors chosen by the user in variables with
    # shorter names.
    selected.themes = input$ecologySetThemePicker
    selected.colors = input$ecologySetColorPicker
    # Get the dataset to display.  Filter if necessary.
    temp.species.df = ecology.df %>%
      dplyr::select(theme.name, set.name, part.name, color.name, color.hex,
                    text.color.hex, total.parts) %>%
      distinct()
    if(length(selected.themes) > 0) {
      temp.species.df = temp.species.df %>%
        filter(theme.name %in% gsub(" \\([0-9]+\\)$", "", selected.themes))
    }
    if(length(selected.colors) > 0) {
      temp.species.df = temp.species.df %>%
        filter(color.name %in% selected.colors)
    }
    # Display the dataset.
    part.table(temp.species.df,
               columns.to.hide = c(4, 5),
               column.names = c("Theme", "Set", "Part", "Color", "Color hex",
                                "Text color hex", "Number of pieces"))
  })
  
})
