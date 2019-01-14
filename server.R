shinyServer(function(input, output) {
  
  #############################################################################
  # Demographics                                                              #
  #############################################################################
  
  # Create the demographics graph.
  ethnicity.graph = reactive({
    # Store the themes chosen by the user in a variable with a shorter name.
    selected.themes = input$demographicsThemePicker
    # If the user has selected any specific themes, filter on those themes.
    # Otherwise, set the theme name to an empty string so that the code below
    # doesn't facet by theme.
    temp.heads.df = heads.df
    if(length(selected.themes) > 0) {
      temp.heads.df = temp.heads.df %>%
        filter(theme.name %in% gsub(" \\([0-9]+\\)$", "", selected.themes))
    } else {
      temp.heads.df = temp.heads.df %>%
        mutate(theme.name = "")
    }
    ethnicity.edges = bind_rows(
      # Create one edge from each color/theme to each relevant part.
      temp.heads.df %>%
        mutate(from = paste(color.hex, theme.name, sep = ""),
               to = paste(part.id, color.hex, theme.name, sep = "")) %>%
        select(from, to) %>%
        distinct(),
      # Add one edge from each theme (or the root, if no themes are selected)
      # to each color/theme.
      temp.heads.df %>%
        mutate(from = paste("root",  theme.name, sep = ""),
               to = paste(color.hex, theme.name, sep = "")) %>%
        select(from, to) %>%
        distinct()
    )
    # If the user didn't select any themes, we've already added a root node.
    # Otherwise, add one edge from the root to each theme.
    if(length(selected.themes > 0)) {
      ethnicity.edges = bind_rows(
        ethnicity.edges,
        temp.heads.df %>%
          mutate(from = "root",
                 to = paste("root", theme.name, sep = ""))
      )
    }
    # Add vertices.
    ethnicity.vertices = bind_rows(
      # Add one vertex for each part.
      temp.heads.df %>%
        group_by(part.id, part.name, color.hex, theme.name, sep = "") %>%
        summarize(total.parts = sum(num.parts)) %>%
        ungroup() %>%
        mutate(name = paste(part.id, color.hex, theme.name, sep = ""),
               fill.to.plot = color.hex,
               color.to.plot = "#000000") %>%
        select(name, part.name, fill.to.plot, color.to.plot, total.parts,
               theme.name) %>%
        distinct(),
      # Add one vertex for each color/theme.
      temp.heads.df %>%
        mutate(name = paste(color.hex, theme.name, sep = ""),
               fill.to.plot = "#FFFFFF",
               color.to.plot = "#000000",
               total.parts = 1) %>%
        select(name, fill.to.plot, color.to.plot, total.parts, theme.name) %>%
        distinct(),
      # Add a root vertex.
      data.frame(name = "root",
                 fill.to.plot = "#FFFFFF",
                 color.to.plot = "#000000",
                 total.parts = 1,
                 stringsAsFactors = F)
    )
    # If any themes are selected, add one vertex for each theme.
    if(length(selected.themes) > 0) {
      ethnicity.vertices = bind_rows(
        ethnicity.vertices,
        temp.heads.df %>%
          mutate(name = paste("root", theme.name, sep = ""),
                 fill.to.plot = "#FFFFFF",
                 color.to.plot = "#000000",
                 total.parts = 1) %>%
          select(name, fill.to.plot, color.to.plot, total.parts, theme.name) %>%
          distinct()
      )
    }
    # Use ggraph to create the circlepack plot.
    ethnicity.igraph = graph_from_data_frame(ethnicity.edges, vertices = ethnicity.vertices)
    ethnicity.ggraph = ggraph(ethnicity.igraph,
                              layout = "circlepack", weight = "total.parts") +
      geom_node_circle()
    # Pull out x, y, and r for each category.
    facet.centers = ethnicity.ggraph$data
    if(length(selected.themes) > 0) {
      facet.centers = facet.centers %>%
        filter(as.character(name) != "root" &
                 gsub("^root", "", as.character(name)) == as.character(theme.name))
    } else {
      facet.centers = facet.centers %>%
        filter(as.character(name) == "root") %>%
        mutate(theme.name = "")
    }
    facet.centers = facet.centers %>%
      mutate(x.center = x, y.center = y, r.center = r) %>%
      dplyr::select(x.center, y.center, r.center, theme.name)
    # Rescale x, y, and r for each non-root so that each theme (facet) is
    # centered at (0, 0) and on the same scale.
    ethnicity.faceted.data = ethnicity.ggraph$data
    if(length(selected.themes) > 0) {
      ethnicity.faceted.data = ethnicity.faceted.data %>%
        filter(!is.na(theme.name))
    } else {
      ethnicity.faceted.data = ethnicity.faceted.data %>%
        mutate(theme.name = "")
    }
    ethnicity.faceted.data = ethnicity.faceted.data %>%
      left_join(facet.centers, by = c("theme.name")) %>%
      mutate(x.faceted = (x - x.center) / r.center,
             y.faceted = (y - y.center) / r.center,
             r.faceted = r / r.center)
    # Feed the rescaled dataset into geom_circle.
    # print(head(ethnicity.faceted.data[,c("x.faceted", "y.faceted", "r.faceted", "fill.to.plot", "color.to.plot", "theme.name")]))
    ethnicity.facet.graph = ggplot(ethnicity.faceted.data,
                                   aes(x0 = x.faceted,
                                       y0 = y.faceted,
                                       r = r.faceted,
                                       fill = fill.to.plot,
                                       color = color.to.plot)) +
      geom_circle() +
      scale_fill_manual(values = sort(unique(as.character(ethnicity.faceted.data$fill.to.plot)))) +
      scale_color_manual(values = sort(unique(as.character(ethnicity.faceted.data$color.to.plot)))) +
      coord_equal() +
      guides(fill = F, color = F, size = F) +
      theme_void()
    if(length(input$demographicsThemePicker) > 0) {
      ethnicity.facet.graph = ethnicity.facet.graph +
        facet_wrap(~ theme.name) +
        theme(strip.text = element_text(size = 20, face = "bold"))
    }
    ethnicity.facet.graph
  })
  
  # The actual demographics graph.
  output$demographicsPlot <- renderPlot({
    ethnicity.graph()
  })
  
  # Tooltip for the demographics graph.
  # https://gitlab.com/snippets/16220
  output$demographicsHover = renderUI({
    # Get the hover options.
    hover = input$demographics_plot_hover
    # Find the data point that corresponds to the circle the mouse is hovering
    # over.
    if(!is.null(hover)) {
      point = ethnicity.graph()$data %>%
        filter(leaf) %>%
        filter(r.faceted >= (((x.faceted - hover$x) ^ 2) + ((y.faceted - hover$y) ^ 2)) ^ .5)
      if(length(input$demographicsThemePicker) > 0) {
        point = point %>%
          filter(as.character(theme.name) ==  hover$panelvar1)
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
      p(HTML(as.character(point$part.name)))
    )
  })
  
})
