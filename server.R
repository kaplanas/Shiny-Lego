shinyServer(function(input, output) {
  
  #############################################################################
  #############################################################################
  ## Demographics                                                            ##
  #############################################################################
  #############################################################################
  
  #############################################################################
  # Gender and ethnicity                                                      #
  #############################################################################
  
  # Create the demographics graph.
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
             facet.gender = "")
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
               facet.gender = gender)
    }
    demographics.circle.edges = bind_rows(
      # Create one edge from each color/facet to each relevant part.
      temp.heads.df %>%
        mutate(from = paste(color.hex, facet.name),
               to = paste(part.id, color.hex, facet.name)) %>%
        select(from, to) %>%
        distinct(),
      # Add one edge from each facet to each color/facet.
      temp.heads.df %>%
        mutate(from = facet.name,
               to = paste(color.hex, facet.name)) %>%
        select(from, to) %>%
        distinct(),
      # Add one edge from the root to each facet.
      temp.heads.df %>%
        mutate(from = "root",
               to = facet.name) %>%
        select(from, to) %>%
        distinct()
    )
    # Add vertices.
    demographics.circle.vertices = bind_rows(
      # Add one vertex for each part.
      temp.heads.df %>%
        group_by(part.id, part.name, color.hex, facet.name, facet.theme,
                 facet.gender) %>%
        summarize(total.parts = sum(num.parts)) %>%
        ungroup() %>%
        mutate(name = paste(part.id, color.hex, facet.name),
               fill.to.plot = color.hex,
               color.to.plot = "#000000") %>%
        select(name, part.name, fill.to.plot, color.to.plot, total.parts,
               facet.name, facet.theme, facet.gender) %>%
        distinct(),
      # Add one vertex for each color/facet.
      temp.heads.df %>%
        mutate(name = paste(color.hex, facet.name),
               fill.to.plot = "#FFFFFF",
               color.to.plot = "#000000",
               total.parts = 1) %>%
        select(name, fill.to.plot, color.to.plot, total.parts, facet.name,
               facet.theme, facet.gender) %>%
        distinct(),
      # Add one vertex for each facet.
      temp.heads.df %>%
        mutate(name = facet.name,
               fill.to.plot = "#FFFFFF",
               color.to.plot = "#000000",
               total.parts = 1) %>%
        select(name, fill.to.plot, color.to.plot, total.parts, facet.name,
               facet.theme, facet.gender) %>%
        distinct(),
      # Add a root vertex.
      data.frame(name = "root",
                 fill.to.plot = "#FFFFFF",
                 color.to.plot = "#000000",
                 total.parts = 1,
                 stringsAsFactors = F)
    )
    # Use ggraph to create the circlepack plot.
    demographics.circle.igraph = graph_from_data_frame(demographics.circle.edges,
                                                       vertices = demographics.circle.vertices)
    demographics.circle.ggraph = ggraph(demographics.circle.igraph,
                                 layout = "circlepack", weight = "total.parts") +
      geom_node_circle()
    # Pull out x, y, and r for each category.
    demographics.circle.facet.centers = demographics.circle.ggraph$data %>%
      filter(as.character(name) == as.character(facet.name)) %>%
      group_by(facet.theme) %>%
      mutate(x.center = x, y.center = y,
             r.center = max(r)) %>%
      ungroup() %>%
      dplyr::select(x.center, y.center, r.center, facet.name)
    # Rescale x, y, and r for each non-root so that each theme (facet) is
    # centered at (0, 0) and the same size.  Within a theme, sub-facets by
    # gender are the same scale; therefore, facets with smaller counts will
    # appear smaller.
    demographics.circle.faceted.data = demographics.circle.ggraph$data %>%
      rownames_to_column("rowname") %>%
      inner_join(demographics.circle.facet.centers, by = c("facet.name")) %>%
      mutate(x.faceted = (x - x.center) / r.center,
             y.faceted = (y - y.center) / r.center,
             r.faceted = r / r.center)
    # Feed the rescaled dataset into geom_circle.
    demographics.circle.facet.graph = ggplot(demographics.circle.faceted.data,
                                             aes(x0 = x.faceted,
                                                 y0 = y.faceted,
                                                 r = r.faceted,
                                                 fill = fill.to.plot,
                                                 color = color.to.plot)) +
      geom_circle() +
      scale_fill_manual(values = sort(unique(as.character(demographics.circle.faceted.data$fill.to.plot)))) +
      scale_color_manual(values = sort(unique(as.character(demographics.circle.faceted.data$color.to.plot)))) +
      coord_equal() +
      guides(fill = F, color = F, size = F) +
      theme_void()
    if(length(input$demographicsCircleThemePicker) > 0) {
      if(length(input$demographicsCircleGenderPicker) > 0) {
        demographics.circle.facet.graph = demographics.circle.facet.graph +
          facet_grid(facet.theme ~ facet.gender) +
          theme(strip.text.y = element_text(angle = -90))
      } else {
        demographics.circle.facet.graph = demographics.circle.facet.graph +
          facet_wrap(~ facet.theme)
      }
    } else if(length(input$demographicsCircleGenderPicker) > 0) {
      demographics.circle.facet.graph = demographics.circle.facet.graph +
        facet_wrap(~ facet.gender)
    }
    if(length(input$demographicsCircleThemePicker) > 0 |
       length(input$demographicsCircleGenderPicker) > 0) {
      demographics.circle.facet.graph = demographics.circle.facet.graph +
        theme(strip.text = element_text(size = 20, face = "bold"))
    }
    demographics.circle.facet.graph
  })
  
  # The actual demographics graph.
  output$demographicsCirclePlot <- renderPlot({
    demographics.circle.graph()
  })
  demographics.circle.plot.width = reactive({
    numeric.width = 700
    if(length(input$demographicsCircleGenderPicker) > 0) {
      numeric.width = length(input$demographicsCircleGenderPicker) * 300
    } else if(length(input$demographicsCircleThemePicker) > 0) {
      numeric.width = wrap_dims(length(input$demographicsCircleThemePicker))[2] * 300
    }
    return(paste(numeric.width, "px", sep = ""))
  })
  demographics.circle.plot.height = reactive({
    numeric.height = 700
    if(length(input$demographicsCircleGenderPicker > 0)) {
      if(length(input$demographicsCircleThemePicker > 0)) {
        numeric.height = length(input$demographicsCircleThemePicker) * 300
      } else {
        numeric.height = 300
      }
    } else if(length(input$demographicsCircleThemePicker) > 0) {
      numeric.height = wrap_dims(length(input$demographicsCircleThemePicker))[1] * 300
    }
    return(paste(numeric.height, "px", sep = ""))
  })
  output$demographicsCirclePlotUI <- renderUI({
    plotOutput("demographicsCirclePlot",
               width = demographics.circle.plot.width(),
               height = demographics.circle.plot.height(),
               hover = hoverOpts("demographicsCirclePlotHover",
                                   delay = 20, delayType = "debounce"))
  })
  
  # Tooltip for the demographics graph.
  # https://gitlab.com/snippets/16220
  output$demographicsCircleHover = renderUI({
    # Get the hover options.
    hover = input$demographicsCirclePlotHover
    # Find the data point that corresponds to the circle the mouse is hovering
    # over.
    if(!is.null(hover)) {
      point = demographics.circle.graph()$data %>%
        filter(leaf) %>%
        filter(r.faceted >= (((x.faceted - hover$x) ^ 2) + ((y.faceted - hover$y) ^ 2)) ^ .5)
      if(length(input$demographicsCircleGenderPicker) > 0) {
        point = point %>%
          filter(as.character(facet.gender) ==  hover$panelvar1)
        if(length(input$demographicsCircleThemePicker) > 0) {
          point = point %>%
            filter(as.character(facet.theme) == hover$panelvar2)
        }
      } else if(length(input$demographicsCircleThemePicker) > 0) {
        point = point %>%
          filter(as.character(facet.theme) == hover$panelvar1)
      }
    } else {
      return(NULL)
    }
    if(nrow(point) != 1) {
      return(NULL)
    }
    # Calculate how far from the left and top the center of the circle is, as a
    # percent of the total graph size.
    left_pct = (point$x.faceted - hover$domain$left) / (hover$domain$right - hover$domain$left)
    top_pct <- (hover$domain$top - point$y.faceted) / (hover$domain$top - hover$domain$bottom)
    # Convert the percents into pixels.
    left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
    top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
    # Set the style of the tooltip.
    style = paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
                   "left:", left_px, "px; top:", top_px, "px;")
    # Create the actual tooltip as a wellPanel.
    wellPanel(
      style = style,
      p(HTML(paste(paste("<b>", point$total.parts,
                         " piece",
                         ifelse(point$total.parts == 1, "", "s"),
                         "</b>",
                         sep = ""),
                   as.character(point$part.name),
                   sep = "<br/>")))
    )
  })
  
  #############################################################################
  # Ethnic diversity and gender parity                                        #
  #############################################################################
  
  output$demographicsDiversity = renderPlot({
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
    # Sort by the column specified by the user.
    if(input$demographicsOrderPicker == "Measure") {
      temp.heads.df = temp.heads.df %>%
        arrange(measure, desc(theme.name))
    } else if(input$demographicsOrderPicker == "Number of pieces") {
      temp.heads.df  = temp.heads.df %>%
        arrange(theme.num.parts, desc(theme.name))
    } else if(input$demographicsOrderPicker == "Theme name") {
      temp.heads.df = temp.heads.df %>%
        arrange(desc(theme.name))
    }
    temp.heads.df$theme.name = factor(temp.heads.df$theme.name,
                                      levels = temp.heads.df$theme.name)
    # Make the plot.
    temp.heads.df %>%
      filter(!is.na(measure)) %>%
      ggplot(aes(x = theme.name, y = measure, fill = theme.num.parts)) +
      geom_bar(stat = "identity", color = "black", size = 0.4) +
      scale_x_discrete("") +
      scale_y_continuous(input$demographicsMeasurePicker,
                         sec.axis = dup_axis()) +
      scale_fill_continuous("Number of pieces", trans = "log",
                            breaks = c(2, 20, 200, 2000),
                            high = "black", low = "white") +
      coord_flip()
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
  
})
