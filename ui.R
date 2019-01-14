shinyUI(navbarPage(
  
  # Application title.
  "Lego Database",
  
  # Demographics.
  tabPanel(
    
    "Demographics",
    
    # Make sure the cursor has the default shape, even when using tooltips
    tags$head(tags$style(HTML("#demographicsPlot { cursor: default; }"))),
    
    # Sidebar panel for controls.
    sidebarPanel(
      pickerInput(
        "demographicsThemePicker",
        choices = data.frame(
          theme.counts.df %>%
            filter(total.heads > 0) %>%
            mutate(label = paste(theme.name, " (", total.heads, ")", sep = "")) %>%
            arrange(desc(total.heads), theme.name)
        )$label,
        options = list(`actions-box` = T),
        multiple = T
      )
    ),
    
    # Main panel for plot.
    mainPanel(
      # Circle-packing plot.
      div(
        style = "position:relative",
        plotOutput(
          "demographicsPlot",
          width = "700px",
          height = "700px",
          hover = hoverOpts("demographics_plot_hover", delay = 20, delayType = "debounce")
        ),
        uiOutput("demographicsHover")
      )
    )
    
  )
  
))
