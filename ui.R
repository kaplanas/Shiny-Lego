shinyUI(fluidPage(theme = "style.css",
                  div(style = "padding: 1px 0px; width: '100%'",
                      titlePanel(
                        title = "",
                        windowTitle = "Living in the Lego World"
                      )
                  ),
                  navbarPage(
  
  # Application title.
  title = div(span(img(src = "lego_head_small.png"),
                   "Living in the Lego World",
                   style = "position: relative; top: 50%; transform: translateY(-50%);")),
  
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
          pickerInput(
            "demographicsCircleThemePicker", "Filter to one or more themes:",
            choices = c(), multiple = T
          ),
          pickerInput(
            "demographicsCircleGenderPicker", "Filter to one or more genders:",
            choices = c(),
            options = list(`actions-box` = T),
            multiple = T
          ),
          tags$p(span("Large graphs (e.g., of the full dataset) may take a few seconds to render.  The first graph may take up to two minutes if the app is retrieving new data from Rebrickable.", style = "color:red")),
          tags$p(HTML("<b>Hover</b> to see the part name.")),
          tags$p(HTML("Each circle represents a <b>unique minifigure or minidoll head</b>.")),
          tags$p(HTML("Area is proportional to the <b>number of pieces</b> across all sets.")),
          tags$p(HTML("<b>\"Ethnicity\"</b> is the color of the piece.  Yes, it's silly.")),
          tags$p(HTML("<b>Gender</b> is inferred from keywords in the part name (\"Male\", \"Female\", etc., plus references to facial hair).")),
          tags$p("Some heads are not labeled male/female but contain the name of a character of known gender (e.g., \"Han Solo\").  Incorporating this information would require a hand-maintained list of character names and their genders; I haven't done this.")
        ),

        # Main panel with plot.
        mainPanel(
          uiOutput("demographicsCirclePlotUI") %>%
            withSpinner()
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
          tags$p(HTML("<b>Hover</b> to see the number of pieces and measure value.")),
          tags$p(HTML("<b>Ethnic diversity</b> is the Shannon entropy (base 2) of color over all pieces.")),
          tags$p(HTML("<b>Percent female</b> is the percent of female pieces out of all pieces, excluding pieces of unknown gender.")),
          tags$p(HTML("<b>Saturation</b> represents the number of pieces in the theme."))
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
          pickerInput(
            "demographicsSetThemePicker", "Filter to one or more themes:",
            choices = c(), multiple = T
          ),
          pickerInput(
            "demographicsSetEthnicityPicker", "Filter to one or more ethnicities:",
            choices = c(), multiple = T
          ),
          pickerInput(
            "demographicsSetGenderPicker", "Filter to one or more genders:",
            choices = c(),
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
          pickerInput(
            "hairTreemapThemePicker", "Filter to one or more themes:",
            choices = c(), multiple = T
          ),
          selectInput(
            "hairTreemapOrderPicker", "Adjust order of levels:",
            choices = list("Color, then style" = "color.first",
                           "Style, then color" = "style.first")
          ),
          tags$p(span("Large graphs (e.g., of the full dataset) may take a few seconds to render.", style = "color:red")),
          tags$p(HTML("<b>Click</b> to drill down.  At the lowest level, <b>hover</b> to see the part name.")),
          tags$p(HTML("<b>Style</b> is inferred from keywords in the part name.")),
          tags$p(HTML("A part with no keywords is classified as <b>\"Other\"</b>.")),
          tags$p(HTML("A single part may have <b>multiple styles</b>; in that case, it's counted in <i>all</i> relevant styles."))
        ),

        # Main panel with plot.
        mainPanel(div(
          style = "position:relative",
          htmlOutput("hairTreemapUI") %>%
            withSpinner()
        ))

      ),

      # Treemap of clothing type and color.
      tabPanel(

        "Clothing",

        # Sidebar panel for controls.
        sidebarPanel(
          pickerInput(
            "clothesTreemapThemePicker", "Filter to one or more themes:",
            choices = c(), multiple = T
          ),
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
            "clothesTreemapTypePicker", "Filter to one or more clothing types:",
            choices = c(), multiple = T
          ),
          tags$p(span("Large graphs (e.g., of the full dataset) may take a few seconds to render.", style = "color:red")),
          tags$p(HTML("<b>Click</b> to drill down.  At the lowest level, <b>hover</b> to see the part name.")),
          tags$p(HTML("Clothes for the <b>upper</b> and <b>lower</b> body are shown separately.")),
          tags$p(HTML("<b>Type</b> is inferred from keywords in the part name.")),
          tags$p(HTML("A part with no keywords is classified as <b>\"Other\"</b>.")),
          tags$p(HTML("A single part may have <b>multiple types</b>; in that case, it's counted in <i>all</i> relevant types.")),
          tags$p(HTML("<b>Color</b> is inferred from the color word(s) immediately before the type keyword, if any (e.g., \"<u>White</u> Shirt\").  Otherwise, the color of the piece is used.  This method is not 100&percnt; accurate; it misses cases like \"<u>White</u> Button-Down Shirt\"."))
        ),

        # Main panel with plot.
        mainPanel(div(
          style = "position:relative",
          htmlOutput("clothesTreemapUI") %>%
            withSpinner()
        ))

     ),

     # Treemap of accessories.
     tabPanel(

       "Accessories",

       # Sidebar panel for controls.
       sidebarPanel(
         pickerInput(
           "accessoriesTreemapThemePicker", "Filter to one or more themes:",
           choices = c(), multiple = T
         ),
         tags$p(span("Large graphs (e.g., of the full dataset) may take a few seconds to render.", style = "color:red")),
         tags$p(HTML("<b>Click</b> to drill down.  At the lowest level, <b>hover</b> to see the part name.")),
         tags$p(HTML("<b>Accessories</b> are inferred from keywords in the part name.")),
         tags$p(HTML("A single part may have <b>multiple accessories</b>; in that case, it's counted in <i>all</i> relevant accessories.")),
         tags$p(HTML("<b>Color</b> is not included because it can't consistently be inferred from the dataset.  The color of the accessory may be the color of the piece, a color noted in the name of the part, or not indicated at all."))
       ),

       # Main panel with plot.
       mainPanel(div(
         style = "position:relative",
         htmlOutput("accessoriesTreemapUI") %>%
           withSpinner()
       ))

     ),

     # Table for finding sets with pieces with particular hair styles,
     # clothing types, or accessories.
     tabPanel(

       "Find sets with a specific item",

       # Sidebar panel for controls.
       sidebarPanel(
         pickerInput(
           "fashionSetThemePicker", "Filter to one or more themes:",
           choices = c(), multiple = T
         ),
         pickerInput(
           "fashionSetColorPicker", "Filter to one or more colors:",
           choices = c(), multiple = T
         ),
         pickerInput(
           "fashionSetHairStylePicker", "Filter to one or more hair styles",
           choices = c(), multiple = T
         ),
         pickerInput(
           "fashionSetClothingTypePicker", "Filter to one or more clothing types",
           choices = c(), multiple = T
         ),
         pickerInput(
           "fashionSetAccessoryPicker", "Filter to one or more accessories",
           choices = c(), multiple = T
         ),
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
          pickerInput(
            "moodsPolarThemePicker", "Filter to one or more themes:",
            choices = c(), multiple = T
          ),
          pickerInput(
            "moodsPolarGenderPicker", "Filter to one or more genders:",
            choices = c(),
            options = list(`actions-box` = T),
            multiple = T
          ),
          tags$p(span("Large graphs (e.g., of the full dataset) may take a few seconds to render.", style = "color:red")),
          tags$p(HTML("<b>Hover</b> to see the number of pieces.")),
          tags$p(HTML("<b>Mood</b> is inferred from keywords in the part name (heads only).")),
          tags$p(HTML("A single head may have <b>multiple moods</b>; in that case, it's counted in <i>all</i> relevant moods."))
        ),

        # Main panel with plot.
        mainPanel(
          uiOutput("moodsPolarPlotUI") %>%
            withSpinner()
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
          tags$p(span("Large graphs (e.g., of the full dataset) may take a few seconds to render.", style = "color:red")),
          tags$p(HTML("<b>Saturation</b> represents the number of pieces in the theme.")),
          tags$p(HTML("<b>Hover</b> to see the number of pieces and percent with the chosen mood."))
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
          pickerInput(
            "moodsSetThemePicker", "Filter to one or more themes:",
            choices = c(), multiple = T
          ),
          pickerInput(
            "moodsSetGenderPicker", "Filter to one or more genders:",
            choices = c(),
            options = list(`actions-box` = T),
            multiple = T
          ),
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
          pickerInput(
            "plantsDendrogramThemePicker", "Filter to one or more themes:",
            choices = c(), multiple = T
          ),
          tags$p(span("Large graphs (e.g., of the full dataset) may take a few seconds to render.", style = "color:red")),
          tags$p(span(HTML("<b>Hover</b> over a node to see a list of pieces.  Filled nodes have no pieces."))),
          tags$p(span(HTML("<b>Double-click</b> on a node to collapse its daughters."))),
          tags$p(span(HTML("<b>Scroll</b> to zoom in or out.  <b>Click and drag</b> to move the tree."))),
          tags$p(span(HTML("<b>\"Species\"</b> (very roughly speaking) is inferred from keywords in the part name.  <b>Phylogenetic trees</b> (even rougher) are constructed from hypernyms in WordNet.")))
        ),

        # Main panel with dendrogram.
        mainPanel(
          uiOutput("plantsDendrogram") %>%
            withSpinner()
        )

      ),

      # Dendrogram of animals.
      tabPanel(

        "Animals",

        # Sidebar panel for controls.
        sidebarPanel(
          pickerInput(
            "animalsDendrogramThemePicker", "Filter to one or more themes:",
            choices = c(), multiple = T
          ),
          tags$p(span("Large graphs (e.g., of the full dataset) may take a few seconds to render.", style = "color:red")),
          tags$p(span(HTML("<b>Hover</b> over a node to see a list of pieces.  Filled nodes have no pieces."))),
          tags$p(span(HTML("<b>Double-click</b> on a node to collapse its daughters."))),
          tags$p(span(HTML("<b>\"Species\"</b> (very roughly speaking) is inferred from keywords in the part name.  <b>Phylogenetic trees</b> (even rougher) are constructed from hypernyms in WordNet.")))
        ),

        # Main panel with dendrogram.
        mainPanel(
          uiOutput("animalsDendrogram") %>%
            withSpinner()
        )

      ),

      # Bar plot of species diversity.
      tabPanel(

        "Species diversity by theme",

        # Sidebar panel for controls.
        sidebarPanel(
          pickerInput(
            "ecologyMeasurePicker", "Choose measure to plot:",
            choices = c("Species diversity of plants", "Species diversity of animals"),
            selected = "Species diversity of plants",
            multiple = F
          ),
          pickerInput(
            "ecologyOrderPicker", "Order by:",
            choices = c("Measure", "Number of pieces", "Theme name"),
            selected = "Measure",
            multiple = F
          ),
          tags$p(HTML("<b>Hover</b> to see the number of pieces and measure value.")),
          tags$p(HTML("<b>Species diversity</b> is the Shannon entropy (base 2) of \"species\" (deduced from keywords in the part name) over all pieces.")),
          tags$p(HTML("<b>Saturation</b> represents the number of pieces in the theme."))
        ),

        # Main panel with plot.
        mainPanel(
          highchartOutput("ecologyDiversity",
                          width = "700px", height = "2000px")
        )

      ),

      # Table for finding sets with pieces for a particular plant/animal.
      tabPanel(

        "Find sets with a specific plant or animal",

        # Sidebar panel for controls.
        sidebarPanel(
          pickerInput(
            "ecologySetThemePicker", "Filter to one or more themes:",
            choices = c(), multiple = T
          ),
          pickerInput(
            "ecologySetColorPicker", "Filter to one or more colors:",
            choices = c(), multiple = T
          )
        ),

        # Main panel with table.
        mainPanel(
          dataTableOutput("ecologySets")
        )

      )

    )

  ),
  
  # About and credits.
  tabPanel(
    
    "About",
    
    # Various tabs.
    tabsetPanel(
      
      # General info.
      tabPanel(
        "Overview",
        tags$h1("Scope"),
        tags$p(HTML("This collection of visualizations addresses the question, \"What is it like to live in the Lego world?\"  In other words, if you're Wyldstyle, what kinds of people do you meet?  How are they feeling?  What plants and animals do you find around you?")),
        tags$p(HTML("Think of each theme as an island on the Lego planet.  Each visualization can be faceted by theme, so you can compare fashion, flora and fauna, etc. across themes.")),
        tags$h1("Approach"),
        tags$p(HTML("Parts are labeled and categorized using three main sources of information:")),
        tags$ul(
          tags$li(HTML("The part category (e.g., \"Minifig Heads\" or \"Plants and Animals\") specified in the database")),
          tags$li(HTML("The hexadecimal part color specified in the database")),
          tags$li(HTML("Keywords in the part name"))
        ),
        tags$p(HTML("The keywords that map part names to categories involve more-or-less hand-curated lists and some <i>very</i> basic text processing (mostly regular expressions).  The process is <b>not 100% accurate</b>; there are plenty of false positives and false negatives.  But it's good enough for a first pass.")),
        tags$h1("GitHub"),
        tags$p(HTML("Source code is available at <a href=\"https://github.com/kaplanas/Shiny-Lego\">https://github.com/kaplanas/Shiny-Lego</a>."))
      ),
      
      # Credits.
      tabPanel(
        "Credits",
        tags$h1("Datasets"),
        tags$p(HTML("All Lego data comes from the files made available by <a href=\"https://rebrickable.com/downloads/\">Rebrickable</a>.  The app checks for new data about once a week.")),
        tags$p(HTML("Phylogenetic trees are inferred from hypernym relations in the <a href=\"https://wordnet.princeton.edu/\">WordNet</a> database, using the files provided by <a href=\"http://wordnetport.sourceforge.net/\">WordNetPort</a>.")),
        tags$p(HTML("Basic lemmatization uses the list in the <a href=\"https://github.com/trinker/lexicon\">lexicon</a> package.")),
        tags$h1("R Packages"),
        tags$p(HTML("<a href=\"http://shiny.rstudio.com/\">Shiny</a> and the <a href=\"https://www.tidyverse.org/\">tidyverse</a>, of course.")),
        tags$p(HTML("Position and size of the circles in the demographics circle-packing graphs are calculated using <a href=\"https://github.com/thomasp85/ggraph\">ggraph</a>.")),
        tags$p(HTML("<a href=\"https://igraph.org/r/\">igraph</a> is used to model the hierarchical relationships in the circle-packing graphs, and to construct phylogenetic trees from the graph of WordNet hypernym relationships.")),
        tags$p(HTML("Treemaps, polar charts, and bar charts are rendered with <a href=\"https://www.highcharts.com/\">Highcharts</a>, via <a href=\"http://jkunst.com/highcharter/\">Highcharter</a>.")),
        tags$p(HTML("Phylogenetic trees are rendered with <a href=\"https://datastorm-open.github.io/visNetwork/\">visNetwork</a>.")),
        tags$p(HTML("Tables are rendered with <a href=\"https://datatables.net/\">DataTables</a>, using the <a href=\"https://rstudio.github.io/DT/\">DT</a> package.")),
        tags$p(HTML("Joins with regular expressions are facilitated by <a href=\"https://github.com/dgrtwo/fuzzyjoin\">fuzzyjoin</a>.")),
        tags$p(HTML("<a href=\"https://github.com/statsmaths/cleanNLP\">cleanNLP</a> and <a href=\"https://cran.r-project.org/web/packages/wordnet/index.html\">wordnet</a> aren't used in the app, but were helpful in exploratory data analysis</a>."))
      ),
      
      # Other visualizations.
      tabPanel(
        "Other visualizations",
        tags$p(HTML("")),
        tags$p(HTML("<a href=\"https://shiny.rstudio.com/gallery/lego-set.html\">LEGO set visualizer</a> (Shiny app)")),
        tags$p(HTML("<a href=\"https://www.kaggle.com/devisangeetha/lego-let-s-play\">LEGO - Let's play</a> (parts, colors, sets, and themes)")),
        tags$p(HTML("<a href=\"https://mode.com/blog/lego-data-analysis\">67 years of Lego sets</a>")),
        tags$p(HTML("<a href=\"https://flowingdata.com/2015/10/19/evolving-lego-color-palette/\">Evolving LEGO color palette</a>")),
        tags$p(HTML("<a href=\"https://nateaff.com/2017/09/11/lego-topic-models/\">LEGO color themes as topic models</a>")),
        tags$p(HTML("<a href=\"https://www.wired.com/2012/01/the-mathematics-of-lego/\">The mathematics of Lego</a> (set size and the power law)")),
        tags$p(HTML("<a href=\"http://www.bartneck.de/2010/12/17/taxonomy-for-lego-minifigures/\">Taxonomy for LEGO Minifigures</a>")),
        tags$p(HTML("<a href=\"https://www.kaggle.com/martinellis/prominence-of-special-parts-over-time-visualised\">Prominence of special parts over time, visualised</a>")),
        tags$p(HTML("<a href=\"https://therealityprose.wordpress.com/2013/01/17/what_happened_with_lego/\">What happened with LEGO</a> (Lego set price over time)")),
        tags$p(HTML("Various other projects on <a href=\"https://www.kaggle.com/rtatman/lego-database/kernels\">Kaggle</a>"))
      )
      
    )
    
  )
  
)))
