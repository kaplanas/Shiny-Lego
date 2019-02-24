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
          tags$p(HTML("<b>Gender</b> is inferred from keywords in the part name (\"Male\", \"Female\", etc., plus references to facial hair).")),
          tags$p("Some heads are not labeled male/female but contain the name of a character of known gender (e.g., \"Han Solo\").  Incorporating this information would require a hand-maintained list of character names and their genders; I haven't done this.")
        ),
            
        # Main panel with plot.
        mainPanel(
          uiOutput("demographicsCirclePlotUI")
        )
      
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
          color.picker.input("demographicsSetEthnicityPicker",
                             heads.df,
                             "Filter to one or more ethnicities"),
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
          tags$p(HTML("A part with no keywords is classified as <b>\"Other\"</b>.")),
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
          checkboxInput(
            "clothesTreemapShowOther", tags$b("Show \"other\" pieces (with no type)"),
            value = T
          ),
          pickerInput(
            "clothesTreemapTypePicker",
            "Filter to one or more clothing types:",
            choices = data.frame(
              clothes.type.df %>%
                group_by(type) %>%
                summarize(total.parts = sum(num.parts)) %>%
                mutate(label = paste(type, " (", total.parts, ")", sep = "")) %>%
                arrange(desc(total.parts))
            )$label,
            multiple = T
          ),
          tags$p(span("Large graphs (e.g., of the full dataset) may take a few seconds to render.", style = "color:red")),
          tags$p(HTML("Clothes for the <b>upper</b> and <b>lower</b> body are shown separately.")),
          tags$p(HTML("<b>Type</b> is inferred from keywords in the part name.")),
          tags$p(HTML("A part with no keywords is classified as <b>\"Other\"</b>.")),
          tags$p(HTML("A single part may have <b>multiple types</b>; in that case, it's counted in <i>all</i> relevant types.")),
          tags$p(HTML("<b>Color</b> is inferred from the color word(s) immediately before the type keyword, if any (e.g., \"<u>White</u> Shirt\").  Otherwise, the color of the piece is used.  This method is not 100&percnt; accurate; it misses cases like \"<u>White</u> Button-Down Shirt\"."))
        ),
        
        # Main panel with plot.
        mainPanel(div(
          style = "position:relative",
          htmlOutput("clothesTreemapUI")
        ))

     ),
     
     # Treemap of accessories.
     tabPanel(
       
       "Accessories",
       
       # Sidebar panel for controls.
       sidebarPanel(
         theme.picker.input("accessoriesTreemapThemePicker", "total.accessories"),
         tags$p(span("Large graphs (e.g., of the full dataset) may take a few seconds to render.", style = "color:red")),
         tags$p(HTML("<b>Accessories</b> are inferred from keywords in the part name.")),
         tags$p(HTML("A single part may have <b>multiple accessories</b>; in that case, it's counted in <i>all</i> relevant accessories.")),
         tags$p(HTML("<b>Color</b> is not included because it can't consistently be inferred from the dataset.  The color of the accessory may be the color of the piece, a color noted in the name of the part, or not indicated at all."))
       ),
       
       # Main panel with plot.
       mainPanel(div(
         style = "position:relative",
         htmlOutput("accessoriesTreemapUI")
       ))
       
     ),
     
     # Table for finding sets with pieces with particular hair styles,
     # clothing types, or accessories.
     tabPanel(
       
       "Find sets with a specific item",
       
       # Sidebar panel for controls.
       sidebarPanel(
         theme.picker.input("fashionSetThemePicker", "total.fashion.items"),
         color.picker.input("fashionSetColorPicker",
                            fashion.items.df,
                            "Filter to one or more colors"),
         pickerInput("fashionSetHairStylePicker",
                     "Filter to one or more hair styles",
                     choices = sort(unique(hair.style.df$style)),
                     multiple = T),
         pickerInput("fashionSetClothingTypePicker",
                     "Filter to one or more clothing types",
                     choices = sort(unique(clothes.type.df$type)),
                     multiple = T),
         pickerInput("fashionSetAccessoryPicker",
                     "Filter to one or more accessories",
                     choices = sort(unique(accessory.parts.df$accessory)),
                     multiple = T),
         tags$p(HTML("<b>Color</b> is the color of the piece, which may be different from the color of various items printed on the piece."))
       ),
       
       # Main panel with table.
       mainPanel(
         dataTableOutput("fashionSets")
       )
       
     )

    )

  ),
  
  # Moods.
  tabPanel(
    
    "Moods",
    
    # One tab for each plot/table.
    tabsetPanel(
      
      type = "tabs",
      
      # Polar plot of moods.
      tabPanel(
        
        "Mood counts",
        
        # Sidebar panel for controls.
        sidebarPanel(
          theme.picker.input("moodsPolarThemePicker", "total.moods"),
          tags$p(span("Large graphs (e.g., of the full dataset) may take a few seconds to render.", style = "color:red")),
          tags$p(HTML("<b>Mood</b> is inferred from keywords in the part name."))
        ),
        
        # Main panel with plot.
        mainPanel(
          uiOutput("moodsPolarPlotUI")
        )
        
      ),
      
      # Bar plot of moods.
      tabPanel(
        
        "Mood percents",
        
        # Sidebar panel for controls.
        sidebarPanel(
          pickerInput(
            "moodsMoodPicker", "Choose mood to plot:",
            choices = c("Happy", "Sad", "Angry", "Afraid"),
            selected = "Happy",
            multiple = F
          ),
          pickerInput(
            "moodsOrderPicker", "Order by:",
            choices = c("Percent of pieces with mood", "Total pieces", "Theme name"),
            selected = "Percent of pieces with mood",
            multiple = F
          ),
          tags$p(span("Large graphs (e.g., of the full dataset) may take a few seconds to render.", style = "color:red"))
        ),
        
        # Main panel with plot.
        mainPanel(
          highchartOutput("moodsBarPlot",
                          width = "700px", height = "2000px")
        )
        
      ),
      
      # Table for finding sets with pieces with a particular mood.
      tabPanel(
        
        "Find sets with a specific mood",
        
        # Sidebar panel for controls.
        sidebarPanel(
          theme.picker.input("moodsSetThemePicker", "total.heads"),
          pickerInput(
            "moodsSetMoodPicker", "Filter to one or more moods:",
            choices = c("Happy", "Sad", "Angry", "Afraid"),
            options = list(`actions-box` = T),
            multiple = T
          )
        ),
        
        # Main panel with table.
        mainPanel(
          dataTableOutput("moodsSets")
        )
        
      )
      
    )
    
  ),
  
  # Ecology.
  tabPanel(
    
    "Ecology",
    
    # One tab for each plot/table.
    tabsetPanel(
      
      type = "tabs",
      
      # Dendrogram of plants.
      tabPanel(
        
        "Plants",
        
        # Sidebar panel for controls.
        sidebarPanel(
          theme.picker.input("plantsDendrogramThemePicker", "total.plants"),
          tags$p(span("Large graphs (e.g., of the full dataset) may take a few seconds to render.", style = "color:red")),
          tags$p(span(HTML("<b>Hover</b> over a node to see a list of pieces.  Filled nodes have no pieces."))),
          tags$p(span(HTML("<b>Double-click</b> on a node to collapse its daughters."))),
          tags$p(span(HTML("<b>Scroll</b> to zoom in or out.  <b>Click and drag</b> to move the tree."))),
          tags$p(span(HTML("<b>\"Species\"</b> (very roughly speaking) is inferred from keywords in the part name.  <b>Phylogenetic trees</b> (even rougher) are constructed from hypernyms in WordNet.")))
        ),
        
        # Main panel with dendrogram.
        mainPanel(
          uiOutput("plantsDendrogram")
        )
        
      ),
      
      # Dendrogram of animals.
      tabPanel(
        
        "Animals",
        
        # Sidebar panel for controls.
        sidebarPanel(
          theme.picker.input("animalsDendrogramThemePicker", "total.animals"),
          tags$p(span("Large graphs (e.g., of the full dataset) may take a few seconds to render.", style = "color:red")),
          tags$p(span(HTML("<b>Hover</b> over a node to see a list of pieces.  Filled nodes have no pieces."))),
          tags$p(span(HTML("<b>Double-click</b> on a node to collapse its daughters."))),
          tags$p(span(HTML("<b>\"Species\"</b> (very roughly speaking) is inferred from keywords in the part name.  <b>Phylogenetic trees</b> (even rougher) are constructed from hypernyms in WordNet.")))
        ),
        
        # Main panel with dendrogram.
        mainPanel(
          uiOutput("animalsDendrogram")
        )
        
      ),
      
      # Table for finding sets with pieces for a particular plant/animal.
      tabPanel(
        
        "Find sets with a specific plant or animal",
        
        # Sidebar panel for controls.
        sidebarPanel(
          theme.picker.input("ecologySetThemePicker", "total.plants.animals"),
          color.picker.input("ecologySetEthnicityPicker",
                             heads.df,
                             "Filter to one or more colors")
        ),
        
        # Main panel with table.
        mainPanel(
          dataTableOutput("ecologySets")
        )
        
      )
      
    )
    
  )
  
))
