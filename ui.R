shinyUI(navbarPage(
  
  # Application title.
  "Life in the Lego World",
  
  # Demographics.
  tabPanel(
    
    "Demographics",
    
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
          tags$p(HTML("Each circle represents a <b>unique minifigure or minidoll head</b>.")),
          tags$p(HTML("<b>Hover</b> to see the part name.")),
          tags$p(HTML("Area is proportional to the <b>number of pieces</b> across all sets.")),
          tags$p(HTML("<b>\"Ethnicity\"</b> is the color of the piece.")),
          tags$p(HTML("<b>Gender</b> is inferred from keywords in the part name.")),
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
          tags$p(HTML("<b>Ethnic diversity</b> is the Shannon entropy (base 2) of color over all pieces.")),
          tags$p(HTML("<b>Percent female</b> is the percent of female pieces out of all pieces, excluding pieces of unknown gender."))
        ),
        
        # Main panel with plot.
        mainPanel(
          highchartOutput("demographicsDiversity",
                     width = "700px", height = "2000px")
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
    
  ),
  
  # Fashion
  tabPanel(

    "Fashion",

    # One tab for each plot/table.
    tabsetPanel(

      type = "tabs",

      # Treemap of hair style and color.
      tabPanel(

        "Hair",

        # Sidebar panel for controls.
        sidebarPanel(
          theme.picker.input("hairTreemapThemePicker", "total.hair"),
          selectInput(
            "hairTreemapOrderPicker", "Adjust order of levels:",
            choices = list("Color, then style" = "color.first",
                           "Style, then color" = "style.first")
          ),
          tags$p(span("Large graphs (e.g., of the full dataset) may take a few seconds to render.", style = "color:red")),
          tags$p(HTML("<b>Style</b> is inferred from keywords in the part name.")),
          tags$p(HTML("A single part may have <b>multiple styles</b>; in that case, it's counted in <i>all</i> relevant styles."))
        ),

        # Main panel with plot.
        mainPanel(div(
          style = "position:relative",
          htmlOutput("hairTreemapUI")
        ))

      ),

      # Treemap of clothing type and color.
      tabPanel(

        "Clothing",

        # Sidebar panel for controls.
        sidebarPanel(
          theme.picker.input("clothesTreemapThemePicker", "total.clothes"),
          selectInput(
            "clothesTreemapOrderPicker", "Adjust order of levels:",
            choices = list("Color, then type" = "color.first",
                           "Type, then color" = "type.first")
          ),
          tags$p(span("Large graphs (e.g., of the full dataset) may take a few seconds to render.", style = "color:red")),
          tags$p(HTML("Clothes for the <b>upper</b> and <b>lower</b> body are shown separately.")),
          tags$p(HTML("<b>Type</b> is inferred from keywords in the part name.")),
          tags$p(HTML("A single part may have <b>multiple types</b>; in that case, it's counted in <i>all</i> relevant types."))
        ),
        
        # Main panel with plot.
        mainPanel(div(
          style = "position:relative",
          htmlOutput("clothesTreemapUI")
        ))

     )

    )

  )
  
))
