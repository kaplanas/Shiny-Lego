shinyUI(navbarPage(
  
  # Application title.
  "Lego Database",
  
  # Demographics.
  tabPanel(
    
    "Demographics",
    
    # Make sure the cursor has the default shape, even when using tooltips
    tags$head(tags$style(HTML("#demographicsPlot { cursor: default; }"))),
    
    # One tab for each plot/table.
    tabsetPanel(
      
      type = "tabs",
      
      # Circle-packing plot of ethnicity and gender.
      tabPanel(
        
        "Ethnicity and gender",
        
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
          ),
          tags$p(span("Large graphs (e.g., of the full dataset) may take a few seconds to render.", style = "color:red")),
          tags$p("Each circle represents a unique minifigure or minidoll head.  The area of the circle is proportional to the number of pieces across all sets.  Use the filters to facet by theme and/or gender.  Hover over a circle to see the part name."),
          tags$p("\"Ethnicity\" is the color of the head.  Yes, it's silly."),
          tags$p("Gender is inferred from keywords in the part name."),
          tags$ul(
            tags$li("Male: \"Male\", \"Beard\", \"Moustache\", \"Stubble\", \"Goatee\", \"Sideburn\""),
            tags$li("Female: \"Female\", \"Girl\", \"Woman\"")
          ),
          tags$p("Some heads are not labeled male/female but contain the name of a character of known gender (e.g., \"Han Solo\").  Incorporating this information would require a hand-maintained list of character names and their genders; I haven't done this.")
        ),
            
        # Main panel with plot.
        mainPanel(div(
          style = "position:relative",
          uiOutput("demographicsPlotUI"),
          uiOutput("demographicsHover")
        ))
      
      ),
      
      # Bar plot of ethnic diversity and % female.
      tabPanel(
        
        "Ethnic diversity and gender parity by theme",
        
        # Sidebar panel for controls.
        sidebarPanel(
          pickerInput(
            "demographicsMeasurePicker", "Choose measure to plot:",
            choices = c("Ethnic diversity", "Percent female"),
            selected = "Ethnic diversity",
            multiple = F
          ),
          pickerInput(
            "demographicsOrderPicker", "Order by:",
            choices = c("Measure", "Number of pieces", "Theme name"),
            selected = "Measure",
            multiple = F
          ),
          tags$p("Ethnic diversity is the Shannon entropy (base 2) of color over all pieces."),
          tags$p("Percent female is the percent of female pieces out of all pieces, excluding pieces of unknown gender.")
        ),
        
        # Main panel with plot.
        mainPanel(
          plotOutput("demographicsDiversity",
                     width = "700px", height = "1500px")
        )
        
      ),
      
      # Table for finding sets with pieces of particular ethnicity/gender.
      tabPanel(
        
        "Find sets with a specific ethnicity or gender",
        
        # Sidebar panel for controls.
        sidebarPanel(
          pickerInput(
            "demographicsSetThemePicker", "Filter to specific themes:",
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
            "demographicsSetGenderPicker", "Filter to specific genders:",
            choices = sort(unique(heads.df$gender)),
            options = list(`actions-box` = T),
            multiple = T
          )
        ),
        
        # Main panel with table.
        mainPanel(
          dataTableOutput("demographicsSets")
        )
        
      )
    
    )
    
  )
  
))
