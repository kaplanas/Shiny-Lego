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
        "demographicsThemePicker", "Filter to specific themes:",
        choices = data.frame(
          theme.counts.df %>%
            filter(total.heads > 0) %>%
            mutate(label = paste(theme.name, " (", total.heads, ")", sep = "")) %>%
            arrange(desc(total.heads), theme.name)
        )$label,
        options = list(`actions-box` = T),
        multiple = T
      ),
      pickerInput(
        "demographicsGenderPicker", "Filter to specific genders:",
        choices = sort(unique(heads.df$gender)),
        options = list(`actions-box` = T),
        multiple = T
      )
    ),
    
    # Main panel, with one tab for the plot and one for text.
    mainPanel(tabsetPanel(
      type = "tabs",
      # Circle-packing plot.
      tabPanel(
        "Plot",
        div(
          style = "position:relative",
          uiOutput(
            "demographicsPlotUI"#,
            # width = "auto",
            # height = "auto",
            # hover = hoverOpts("demographics_plot_hover", delay = 20, delayType = "debounce")
          ),
          uiOutput("demographicsHover")
        )
      ),
      tabPanel("Guide", textOutput("demographicsGuide"))
    )
    
  ))
  
))
