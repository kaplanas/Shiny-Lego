shinyUI(navbarPage(
  
  # Application title.
  "Life in the Lego World",
  
  # Demographics.
  tabPanel(
    
    "Demographics",
    
    # Make sure the cursor has the default shape, even when using tooltips
    tags$head(tags$style(HTML("#demographicsCirclePlot { cursor: default; }"))),
    
    # One tab for each plot/table.
    tabsetPanel(
      
      type = "tabs",
      
      # Circle-packing plot of ethnicity and gender.
      tabPanel(
        
        "Ethnicity and gender",
        
        # Sidebar panel for controls.
        sidebarPanel(
          theme.picker.input("demographicsCircleThemePicker", "total.heads"),
          pickerInput(
            "demographicsCircleGenderPicker", "Filter to one or more genders:",
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
          uiOutput("demographicsCirclePlotUI")
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
                     width = "1000px", height = "2000px")
        )
        
      ),
      
      # Table for finding sets with pieces of particular ethnicity/gender.
      tabPanel(
        
        "Find sets with a specific ethnicity or gender",
        
        # Sidebar panel for controls.
        sidebarPanel(
          theme.picker.input("demographicsSetThemePicker", "total.heads"),
          pickerInput(
            "demographicsSetEthnicityPicker", "Filter to one or more ethnicities:",
            choices = sort(unique(heads.df$color.name)),
            choicesOpt = list(content =
                                sapply(sort(unique(heads.df$color.name)),
                                       function(x) {
                                         background.color = paste("background-color: ",
                                                                  unique(colors.df$color.hex[colors.df$name == x]),
                                                                  ";",
                                                                  sep = "")
                                         text.color = paste("color: ",
                                                            unique(colors.df$text.color.hex[colors.df$name == x]),
                                                            ";",
                                                            sep = "")
                                         num.heads = nrow(heads.df[heads.df$color.name == x,])
                                         HTML(paste("<span style=\"padding: 2px; ",
                                                    background.color, " ",
                                                    text.color, "\">",
                                                    x, " (", num.heads, ")",
                                                    "</span>",
                                                    sep = ""))
                                               })),
            multiple = T
          ),
          pickerInput(
            "demographicsSetGenderPicker", "Filter to one or more genders:",
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
    
  )#,
  
  # # Fashion
  # tabPanel(
  #   
  #   "Fashion",
  #   
  #   # Make sure the cursor has the default shape, even when using tooltips
  #   tags$head(tags$style(HTML("#hairCirclePlot { cursor: default; }"))),
  #   tags$head(tags$style(HTML("#clothesCirclePlot { cursor: default; }"))),
  #   
  #   # One tab for each plot/table.
  #   tabsetPanel(
  #     
  #     type = "tabs",
  #     
  #     # Circle-packing plot of hair style and color.
  #     tabPanel(
  # 
  #       "Hair",
  # 
  #       # Sidebar panel for controls.
  #       sidebarPanel(
  #         theme.picker.input("hairSunburstThemePicker", "total.hair"),
  #         selectInput(
  #           "hairSunburstOrderPicker", "Adjust order of levels:",
  #           choices = list("Style, then color" = "style.first",
  #                          "Color, then style" = "color.first")
  #         ),
  #         checkboxInput(
  #           "hairSunburstPiecePicker", "Show individual pieces"
  #         ),
  #         tags$p(HTML("Style is inferred from keywords in the part name.  A single part may have multiple styles; when you facet by style, a part will appearh in <i>all</i> relevant facets."))
  #       ),
  #       
  #       # Main panel with plot.
  #       mainPanel(div(
  #         style = "position:relative",
  #         uiOutput("hairCirclePlotUI")
  #       ))
  # 
  #     ),
  #     
  #     # Circle-packing plot of clothing type and color.
  #     tabPanel(
  #       
  #       "Clothing",
  #       
  #       # Sidebar panel for controls.
  #       sidebarPanel(
  #         theme.picker.input("clothesCircleThemePicker", "total.clothes"),
  #         pickerInput(
  #           "clothesCircleTypePicker", "Filter to specific types:",
  #           choices = sort(unique(clothes.type.df$type)),
  #           multiple = T
  #         )
  #       ),
  #       
  #       # Main panel with plot.
  #       mainPanel(div(
  #         style = "position:relative",
  #         uiOutput("clothesCirclePlotUI"),
  #         uiOutput("clothesCircleHover")
  #       ))
  #       
  #     )
  #     
  #   )
  #   
  # )
  
))
