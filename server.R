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
  output$demographicsCirclePlot <- renderPlot({
    demographics.circle.graph()
  })
  output$demographicsCirclePlotUI <- renderUI({
    plotOutput("demographicsCirclePlot",
               width = circle.plot.width(length(input$demographicsCircleThemePicker),
                                         length(input$demographicsCircleGenderPicker)),
               height = circle.plot.height(length(input$demographicsCircleThemePicker),
                                           length(input$demographicsCircleGenderPicker)),
               hover = hoverOpts("demographicsCirclePlotHover",
                                   delay = 20, delayType = "debounce"))
  })
  
  # Tooltip for the demographics graph.
  output$demographicsCircleHover = renderUI({
    circle.tooltip(input$demographicsCirclePlotHover,
                   demographics.circle.graph()$data,
                   length(input$demographicsCircleThemePicker) > 0,
                   length(input$demographicsCircleGenderPicker) > 0)
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
  
  #############################################################################
  #############################################################################
  ## Fashion                                                                 ##
  #############################################################################
  #############################################################################
  
  #############################################################################
  # Hair                                                                      #
  #############################################################################
  
  # Create the hair circle graph.
  hair.circle.graph = reactive({
    # Store the themes and styles chosen by the user in variables with shorter
    # names.
    selected.themes = input$hairCircleThemePicker
    selected.styles = input$hairCircleStylePicker
    # Determine what we're going to facet by: theme, style, both, or neither.
    # Filter if necessary.
    if(length(selected.styles) > 0) {
      temp.hair.df = hair.style.df %>%
        filter(style %in% selected.styles) %>%
        mutate(facet.name = style,
               facet.theme = "",
               facet.other = style)
    } else {
      temp.hair.df = hair.df %>%
        mutate(facet.name = "",
               facet.theme = "",
               facet.other = "")
    }
    if(length(selected.themes) > 0) {
      temp.hair.df = temp.hair.df %>%
        filter(theme.name %in% gsub(" \\([0-9]+\\)$", "", selected.themes)) %>%
        mutate(facet.name = ifelse(facet.name != "",
                                   paste(facet.name, theme.name),
                                   theme.name),
               facet.theme = theme.name)
    }
    circle.graph(temp.hair.df,
                 facet.by.theme = length(selected.themes) > 0,
                 facet.by.other = length(selected.styles) > 0)
  })
  
  # The actual hair graph.
  output$hairCirclePlot <- renderPlot({
    hair.circle.graph()
  })
  output$hairCirclePlotUI <- renderUI({
    plotOutput("hairCirclePlot",
               width = circle.plot.width(length(input$hairCircleThemePicker),
                                         length(input$hairCircleStylePicker)),
               height = circle.plot.height(length(input$hairCircleThemePicker),
                                           length(input$hairCircleStylePicker)),
               hover = hoverOpts("hairCirclePlotHover",
                                 delay = 20, delayType = "debounce"))
  })
  
  # Tooltip for the hair graph.
  output$hairCircleHover = renderUI({
    circle.tooltip(input$hairCirclePlotHover,
                   hair.circle.graph()$data,
                   length(input$hairCircleThemePicker) > 0,
                   length(input$hairCircleStylePicker) > 0)
  })
  
})
