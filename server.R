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
  
  # # Create the hair sunburst graph.
  # hair.sunburst.graph = reactive({
  #   # Use the user selections to determine which levels to plot in which order.
  #   style.row = list(
  #     "level.column" = quo(style),
  #     "level.fill" = quo("#FFFFFF"),
  #     "level.color" = quo("#000000"),
  #     "level.label" = quo(style)
  #   )
  #   color.row = list(
  #     "level.column" = quo(color.hex),
  #     "level.fill" = quo(color.hex),
  #     "level.color" = quo("#000000"),
  #     "level.label" = quo("")
  #   )
  #   if(input$hairSunburstOrderPicker == "style.first") {
  #     levels.list = list(style.row, color.row)
  #     levels.list[[1]]$columns.in.order = quos(style)
  #     levels.list[[2]]$columns.in.order = quos(style, color.hex)
  #   } else if(input$hairSunburstOrderPicker == "color.first") {
  #     levels.list = list(color.row, style.row)
  #     levels.list[[1]]$columns.in.order = quos(color.hex)
  #     levels.list[[2]]$columns.in.order = quos(color.hex, style)
  #   }
  #   levels.list[[1]]$level.xmin = 0
  #   levels.list[[1]]$level.xmax = 1
  #   levels.list[[2]]$level.xmin = 1
  #   levels.list[[2]]$level.xmax = 2
  #   # Create the plot.
  #   sunburst.plot(hair.style.df,
  #                 levels.list,
  #                 input$hairSunburstThemePicker)
  # })
  
  # # The actual hair graph.
  # output$hairSunburstPlot <- renderD3tree({
  #   # hair.sunburst.graph()
  #   # library
  #   
  #   # dataset
  #   group=c(rep("group-1",4),rep("group-2",2),rep("group-3",3))
  #   subgroup=paste("subgroup" , c(1,2,3,4,1,2,1,2,3), sep="-")
  #   value=c(13,5,22,12,11,7,3,1,23)
  #   data=data.frame(group,subgroup,value)
  #   
  #   # basic treemap
  #   p=treemap(data,
  #             index=c("group","subgroup"),
  #             vSize="value",
  #             type="index"
  #   )            
  #   
  #   # make it interactive ("rootname" becomes the title of the plot):
  #   inter=d3tree( p ,  rootname = "General" )
  #   inter
  # })
  # output$hairSunburstPlotUI <- renderUI({
  #   plotOutput("hairSunburstPlot",
  #              width = circle.plot.width(length(input$hairSunburstThemePicker),
  #                                        length(input$hairSunburstStylePicker)),
  #              height = circle.plot.height(length(input$hairSunburstThemePicker),
  #                                          length(input$hairSunburstStylePicker)),
  #              hover = hoverOpts("hairSunburstPlotHover",
  #                                delay = 20, delayType = "debounce"))
  # })
  
  # Tooltip for the hair graph.
  # output$hairCircleHover = renderUI({
  #   circle.tooltip(input$hairCirclePlotHover,
  #                  hair.circle.graph()$data,
  #                  length(input$hairCircleThemePicker) > 0,
  #                  length(input$hairCircleStylePicker) > 0)
  # })
  
  #############################################################################
  # Clothing                                                                  #
  #############################################################################
  
  # # Create the clothing circle graph.
  # clothes.circle.graph = reactive({
  #   # Store the themes and types chosen by the user in variables with shorter
  #   # names.
  #   selected.themes = input$clothesCircleThemePicker
  #   selected.types = input$clothesCircleTypePicker
  #   # Determine what we're going to facet by: theme, type, both, or neither.
  #   # Filter if necessary.
  #   if(length(selected.types) > 0) {
  #     temp.clothes.df = clothes.type.df %>%
  #       filter(type %in% selected.types) %>%
  #       mutate(facet.name = type,
  #              facet.theme = "",
  #              facet.other = type)
  #   } else {
  #     temp.clothes.df = clothes.df %>%
  #       mutate(facet.name = "",
  #              facet.theme = "",
  #              facet.other = "")
  #   }
  #   if(length(selected.themes) > 0) {
  #     temp.clothes.df = temp.clothes.df %>%
  #       filter(theme.name %in% gsub(" \\([0-9]+\\)$", "", selected.themes)) %>%
  #       mutate(facet.name = ifelse(facet.name != "",
  #                                  paste(facet.name, theme.name),
  #                                  theme.name),
  #              facet.theme = theme.name)
  #   }
  #   circle.graph(temp.clothes.df,
  #                facet.by.theme = length(selected.themes) > 0,
  #                facet.by.other = length(selected.types) > 0)
  # })
  # 
  # # The actual clothing graph.
  # output$clothesCirclePlot <- renderPlot({
  #   clothes.circle.graph()
  # })
  # output$clothesCirclePlotUI <- renderUI({
  #   plotOutput("clothesCirclePlot",
  #              width = circle.plot.width(length(input$clothesCircleThemePicker),
  #                                        length(input$clothesCircleTypePicker)),
  #              height = circle.plot.height(length(input$clothesCircleThemePicker),
  #                                          length(input$clothesCircleTypePicker)),
  #              hover = hoverOpts("clothesCirclePlotHover",
  #                                delay = 20, delayType = "debounce"))
  # })
  # 
  # # Tooltip for the clothing graph.
  # output$clothesCircleHover = renderUI({
  #   circle.tooltip(input$clothesCirclePlotHover,
  #                  clothes.circle.graph()$data,
  #                  length(input$clothesCircleThemePicker) > 0,
  #                  length(input$clothesCircleTypePicker) > 0)
  # })
  
})
