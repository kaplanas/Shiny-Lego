library(dplyr)
library(tidyr)
library(tibble)
library(ggplot2)
library(shiny)
library(shinyWidgets)
library(shinycssloaders)
library(igraph)
library(ggraph)
library(scales)
library(DT)
library(treemap)
library(highcharter)
library(purrr)
library(stringr)
library(fuzzyjoin)
library(lexicon)
library(visNetwork)
library(httr)
library(rdrop2)
library(lubridate)

# Spinner options.
options(spinner.type = 7,
        spinner.color = "#F2CD37")

# Tables we're going to get from Rebrickable.
table.names = c("themes", "colors", "part_categories", "parts", "inventories",
                "sets", "inventory_parts", "inventory_sets",
                "part_relationships")

# Dataframes we're going to create.
data.frames = c("heads", "clothes.type", "fashion.items", "hair.style",
                "accessory.parts", "moods", "theme.counts",
                "ecology.vertices.vis", "ecology.edges.vis", "ecology",
                "colors")

# Paths to various places where we're storing files.
local.path.to.tables = "data_files/"
web.path.to.tables = "http://m.rebrickable.com/media/downloads/"
dropbox.path.to.raw.tables = "raw_lego_tables/"
dropbox.path.to.munged.tables = "munged_lego_tables/"

# Number of milliseconds in one week.  This is how often we check for new data
# at Rebrickable.
milliseconds.in.one.week = 1000 * 60 * 60 * 24 * 7

# Authenticate Dropbox.
drop_auth(rdstoken = "droptoken.rds")

# Function to create a color picker input.  We will need several of these.
color.picker.input = function(input.id, data.df, label.text) {
  pickerInput(
    input.id, label.text,
    choices = sort(unique(data.df$color.name)),
    choicesOpt = list(content =
                        sapply(sort(unique(data.df$color.name)),
                               function(x) {
                                 background.color = paste("background-color: ",
                                                          unique(colors.df$color.hex[colors.df$name == x]),
                                                          ";",
                                                          sep = "")
                                 text.color = paste("color: ",
                                                    unique(colors.df$text.color.hex[colors.df$name == x]),
                                                    ";",
                                                    sep = "")
                                 num.rows = nrow(data.df[data.df$color.name == x,])
                                 HTML(paste("<span style=\"padding: 2px; ",
                                            background.color, " ",
                                            text.color, "\">",
                                            x, " (", num.rows, ")",
                                            "</span>",
                                            sep = ""))
                               })),
    multiple = T
  )
}

# Height and width of (1) an unfaceted circle plot, or (2) each facet of a
# faceted circle plot.
circle.plot.dims = list(height = 700, width = 700)
circle.plot.facet.dims = list(height = 300, width = 300)

# Function to create a (possibly faceted) circle graph.  General strategy: use
# ggraph to pack the circles and determine their position/size, and then feed
# the coordinates and size of each circle into Highcharts to display the graph.
circle.graph = function(data.df, facet.by.theme, facet.by.other) {
  # Create a dataframe of edges.
  circle.edges = bind_rows(
    # Create one edge from each color/facet to each relevant part.
    data.df %>%
      mutate(from = paste(color.hex, facet.name),
             to = paste(part.id, color.hex, facet.name)) %>%
      select(from, to) %>%
      distinct(),
    # Add one edge from each facet to each color/facet.
    data.df %>%
      mutate(from = facet.name,
             to = paste(color.hex, facet.name)) %>%
      select(from, to) %>%
      distinct(),
    # Add one edge from the root to each facet.
    data.df %>%
      mutate(from = "root",
             to = facet.name) %>%
      select(from, to) %>%
      distinct()
  )
  # Create a dataframe of vertices.
  circle.vertices = bind_rows(
    # Add one vertex for each part.
    data.df %>%
      group_by(part.id, part.name, color.hex, facet.name, facet.theme,
               facet.other) %>%
      summarize(total.parts = sum(num.parts)) %>%
      ungroup() %>%
      mutate(name = paste(part.id, color.hex, facet.name),
             fill.to.plot = color.hex,
             color.to.plot = "#000000") %>%
      select(name, part.name, fill.to.plot, color.to.plot, total.parts,
             facet.name, facet.theme, facet.other) %>%
      distinct(),
    # Add one vertex for each color/facet.
    data.df %>%
      mutate(name = paste(color.hex, facet.name),
             fill.to.plot = "#FFFFFF",
             color.to.plot = "#000000",
             total.parts = 1) %>%
      select(name, fill.to.plot, color.to.plot, total.parts, facet.name,
             facet.theme, facet.other) %>%
      distinct(),
    # Add one vertex for each facet.
    data.df %>%
      mutate(name = facet.name,
             fill.to.plot = "#FFFFFF",
             color.to.plot = "#000000",
             total.parts = 1) %>%
      select(name, fill.to.plot, color.to.plot, total.parts, facet.name,
             facet.theme, facet.other) %>%
      distinct(),
    # Add a root vertex.
    data.frame(name = "root",
               fill.to.plot = "#FFFFFF",
               color.to.plot = "#000000",
               total.parts = 1,
               stringsAsFactors = F)
  )
  # Use ggraph to create the circlepack plot.
  circle.igraph = graph_from_data_frame(circle.edges,
                                        vertices = circle.vertices)
  circle.ggraph = ggraph(circle.igraph,
                         layout = "circlepack", weight = "total.parts") +
    geom_node_circle()
  # Pull out x, y, and r for each category, so that we can position and resize
  # the facets appropriately.
  circle.facet.centers = circle.ggraph$data %>%
    filter(as.character(name) == as.character(facet.name)) %>%
    group_by(facet.theme) %>%
    mutate(x.center = x, y.center = y,
           r.center = max(r)) %>%
    ungroup()
  # Determine what the x and y of the category "should" be in order to simulate
  # facets.  Also get the range of each axis.
  # We have to simulate the facets; we can't use hw_grid from the highcharter
  # package because, as far as I can tell, that function is incompatible with
  # setting the width and height of the plot in highchartOutput, which is the
  # only way I can actually get explicit width and height to work in Shiny.
  # And we have to set the width and height explicitly in order to "square up"
  # the plot, because Highcharts doesn't have the equivalent of ggplot's
  # coord_equal.
  wrap.dims = wrap_dims(length(unique(circle.facet.centers$facet.name)))
  if(facet.by.theme & facet.by.other) {
    circle.facet.centers = circle.facet.centers %>%
      mutate(facet.index = NA,
             facet.index.x = dense_rank(facet.other),
             facet.index.y = -1 * dense_rank(facet.theme))
    circle.xmax = length(unique(circle.ggraph$data$facet.other[!is.na(circle.ggraph$data$facet.other)])) + 0.5
    circle.xmin = 0.5
    circle.ymax = -0.5
    circle.ymin = -1 * (length(unique(circle.ggraph$data$facet.theme[!is.na(circle.ggraph$data$facet.theme)])) + 0.5)
  } else {
    circle.facet.centers = circle.facet.centers %>%
      mutate(facet.index = row_number(facet.name)) %>%
      mutate(facet.index.y = ceiling(facet.index / wrap.dims[2]),
             facet.index.x = ((facet.index - 1) %% wrap.dims[2]) + 1) %>%
      mutate(facet.index.y = -1 * facet.index.y)
    circle.xmax = wrap.dims[2] + 0.5
    circle.xmin = 0.5
    circle.ymax = -0.5
    circle.ymin = -1 * (wrap.dims[1] + 0.5)
  }
  circle.facet.centers = circle.facet.centers %>%
    dplyr::select(x.center, y.center, r.center, facet.name, facet.index,
                  facet.index.x, facet.index.y)
  # If we're faceting by anything, create an annotation for each facet.
  circle.facet.labels = list()
  if(facet.by.theme | facet.by.other) {
    circle.facet.labels = apply(
      circle.facet.centers, 1,
      function(x) {
        list(
          labels = list(
            list(
              point = list(
                x = x[["facet.index.x"]],
                y = as.numeric(x[["facet.index.y"]]) + 0.4,
                xAxis = 0, yAxis = 0
              ),
              text = x[["facet.name"]]
            )
          )
        )
      }
    )
  }
  # Rescale x, y, and r for each non-root so that each theme (facet) is
  # centered at the correct (faceted) location and the same size.  Within a
  # theme, sub-facets (e.g., by gender) are the same scale; therefore,
  # sub-facets with smaller counts will have smaller circles.
  circle.faceted.data = circle.ggraph$data %>%
    rownames_to_column("rowname") %>%
    inner_join(circle.facet.centers, by = c("facet.name")) %>%
    mutate(x.faceted = (((x - x.center) / r.center) / 2) + facet.index.x,
           y.faceted = (-1 * (((y - y.center) / r.center) / 2)) + facet.index.y,
           r.faceted = (r / r.center) / 2) %>%
    mutate(total.parts.scaled = pi * (r.faceted ^ 2)) %>%
    filter(leaf)
  # Determine what the diameter of the largest circle should be, in pixels.
  max.diameter = 2 * max(circle.faceted.data$r.faceted)
  if(facet.by.theme | facet.by.other) {
    max.diameter = max.diameter * min(circle.plot.facet.dims$height,
                                      circle.plot.facet.dims$width)
  } else {
    max.diameter = max.diameter * min(circle.plot.dims$height,
                                      circle.plot.dims$width)
  }
  # Feed the data into Highcharts.
  circle.faceted.data = circle.faceted.data %>%
    mutate(part_name = part.name,
           total_parts = total.parts)
  circle.facet.graph = hchart(circle.faceted.data,
                              "bubble",
                              hcaes(x.faceted,
                                    y.faceted,
                                    z = total.parts.scaled,
                                    color = fill.to.plot),
                              minSize = 0,
                              maxSize = max.diameter,
                              zMin = 0,
                              zMax = max(circle.faceted.data$total.parts.scaled) * 1.2,
                              marker = list(lineColor = "#000000",
                                            lineWidth = 0.5,
                                            fillOpacity = 1)) %>%
    hc_chart(margin = 0) %>%
    hc_xAxis(max = circle.xmax,
             maxPadding = 0,
             min = circle.xmin,
             minPadding = 0) %>%
    hc_yAxis(max = circle.ymax,
             maxPadding = 0,
             min = circle.ymin,
             minPadding = 0) %>%
    hc_tooltip(borderWidth = 4,
               headerFormat = "",
               pointFormat = "<span style=\"color:{point.color}\">\u25CF</span>\u00A0<span><b>Total pieces: {point.total_parts}</b></span><br/>{point.part_name}") %>%
    hc_add_theme(hc_theme_null()) %>%
    hc_add_annotations(circle.facet.labels)
  circle.facet.graph
}

# Functions to determine what the height and width of a circle plot should be,
# given whether and how it's faceted.
circle.plot.width = function(num.themes, num.other) {
  numeric.width = circle.plot.dims$width
  if(num.other > 0) {
    if(num.themes > 0) {
      numeric.width = num.other * circle.plot.facet.dims$width
    } else {
      numeric.width = wrap_dims(num.other)[2] * circle.plot.facet.dims$width
    }
  } else if(num.themes > 0) {
    numeric.width = wrap_dims(num.themes)[2] * circle.plot.facet.dims$width
  }
  return(paste(numeric.width, "px", sep = ""))
}
circle.plot.height = function(num.themes, num.other) {
  numeric.height = circle.plot.dims$height
  if(num.other > 0) {
    if(num.themes > 0) {
      numeric.height = num.themes * circle.plot.facet.dims$height
    } else {
      numeric.height = wrap_dims(num.other)[1] * circle.plot.facet.dims$height
    }
  } else if(num.themes > 0) {
    numeric.height = wrap_dims(num.themes)[1] * circle.plot.facet.dims$height
  }
  return(paste(numeric.height, "px", sep = ""))
}

# Function to create a treemap.
treemap.graph = function(data.df, level.settings) {
  # Determine the number of levels in the data.  Use column names to infer the
  # number of levels and their order.
  level.ids = colnames(data.df)[grepl("^id\\.level", colnames(data.df))]
  level.nums = gsub("id\\.level\\.", "", level.ids)
  # Create a dataframe that encodes the tree structure for Highcharts.
  treemap.df = data.frame()
  for(i in 1:max(level.nums)) {
    grouping.levels = c(
      "theme.name",
      paste("id.level.", 1:i, sep = ""),
      colnames(data.df)[grepl(paste("\\.", i, "$", sep = ""), colnames(data.df))]
    )
    cols.to.select = c("theme.name", "name", "id", "level", "value",
                       "valuecolor", "color", "parent", "opacity",
                       paste("index.", 1:i, sep = ""))
    temp.df = data.df %>%
      group_by(.dots = grouping.levels) %>%
      summarize(total.parts = sum(num.parts)) %>%
      ungroup() %>%
      mutate(level = i,
             value = total.parts,
             valuecolor = total.parts) %>%
      mutate_(.dots = setNames(paste0("name.level.", i), "name")) %>%
      mutate_(.dots = setNames(paste0("id.level.", i), "id")) %>%
      mutate_(.dots = setNames(paste0("color.level.", i), "color")) %>%
      mutate_(.dots = setNames(paste0("opacity.level.", i), "opacity"))
    if(i == 1) {
      temp.df = temp.df %>%
        mutate(parent = NA)
    } else {
      temp.df = temp.df %>%
        mutate_(.dots = setNames(paste0("id.level.", i - 1), "parent"))
    }
    for(j in 1:i) {
      if(j == i) {
        temp.df = temp.df %>%
          mutate_(.dots = setNames(NA, paste0("index.", j)))
      } else {
        temp.df = temp.df %>%
          mutate_(.dots = setNames(paste0("id.level.", j + 1), paste0("index.", j)))
      }
    }
    temp.df = temp.df %>%
      select_(.dots = c(cols.to.select))
    treemap.df = bind_rows(treemap.df, temp.df)
  }
  # Make the treemap.
  lapply(
    sort(unique(as.character(data.df$theme.name))),
    function(x) {
      hc = highchart() %>%
        hc_add_series(data = list_parse(treemap.df %>%
                                          filter(theme.name == x)),
                      type = "treemap",
                      layoutAlgorithm = "squarified",
                      levelIsConstant = T,
                      allowDrillToNode = T,
                      levels = level.settings)
      if(x != "") {
        hc = hc %>%
          hc_title(text = x)
      }
      hc
    }
  ) %>%
    hw_grid()
}

# Function to create a DataTable of parts.
part.table = function(data.df, column.names, columns.to.hide) {
  # We don't do much here, but we might want to in the future.
  temp.df = data.df
  # Get all the unique hexadecimal colors and, for each one, whether text
  # printed over that color should be black or white.
  unique.colors = sort(unique(temp.df$color.hex))
  text.color = data.frame(temp.df %>%
                            select(color.hex, text.color.hex) %>%
                            distinct() %>%
                            arrange(color.hex))$text.color.hex
  # Create the DataTable.
  datatable(temp.df,
            options = list(pageLength = 100,
                           columnDefs = list(list(targets = columns.to.hide,
                                                  visible = F))),
            rownames = F,
            colnames = column.names) %>%
    formatStyle("color.hex", target = "row",
                backgroundColor = styleEqual(unique.colors, unique.colors),
                color = styleEqual(unique.colors, text.color))
}

# Function to determine the dimensions we need for faceting a dendrogram.
# We're using bootstrap, so the number of columns has to be a divisor of 12.
dendrogram.facet.dims = function(num.themes) {
  num.cols = wrap_dims(num.themes)[2]
  while(!is.element(num.cols, c(1, 2, 3, 4, 6, 12))) {
    num.cols = num.cols - 1
  }
  num.rows = ceiling(num.themes / num.cols)
  col.width = 12 / num.cols
  list(num.cols = num.cols,
       num.rows = num.rows,
       col.width = col.width)
}

# Function to create the widgets for a faceted dendrogram.
dendrogram.facet.uis = function(facet.info, output.string) {
  do.call(
    tagList,
    lapply(1:facet.info[["num.rows"]],
           function(x) {
             fluidRow(
               do.call(
                 tagList,
                 lapply(1:facet.info[["num.cols"]],
                        function(y) {
                          column(facet.info[["col.width"]],
                                 visNetworkOutput({paste(output.string,
                                                         "dendrogram",
                                                         x, y,
                                                         sep = "")}))
                        })
               )
             )
           })
  )
}

# Function to create a list with one element for each facet of a dendrogram we
# want to plot.  Each element of the list contains the dendrogram itself, plus
# the ID of the widget that holds the facet.
dendrogram.facets = function(vertices.df,
                             edges.df,
                             themes.for.faceting,
                             facet.info,
                             output.string) {
  # Get the theme associated with each facet.
  if(length(themes.for.faceting) == 0) {
    vertices.df = vertices.df %>%
      mutate(theme.name = "")
    themes.to.facet = c("")
  } else {
    themes.to.facet = sort(gsub(" \\([0-9]+\\)$", "", themes.for.faceting))
  }
  # Create the facet for each theme.
  current.theme.index = 1
  facets = list()
  for(i in 1:facet.info[["num.rows"]]) {
    for(j in 1:facet.info[["num.cols"]]) {
      if(current.theme.index <= length(themes.to.facet)) {
        # Note the row and column index of the current facet, and the name of
        # its widget.
        current.facet = list(row.index = i,
                             col.index = j,
                             id = paste(output.string, "dendrogram", i, j,
                                        sep = ""))
        # Start with all and only the vertices associated with this theme, and
        # the edges that lead to them.
        temp.vertices.df = vertices.df %>%
          filter(is.na(theme.name) |
                   theme.name == themes.to.facet[current.theme.index])
        temp.edges.df = edges.df %>%
          semi_join(temp.vertices.df, by = c("to" = "id"))
        # For all the edges in the facet, add any nodes that those edges lead
        # from, and any edges that lead to those nodes in turn, until we've
        # found all the connected nodes/edges.
        found.all.vertices = F
        while(!found.all.vertices) {
          edges.to.add = edges.df %>%
            semi_join(temp.vertices.df, by = c("to" = "id"))
          vertices.to.add = vertices.df %>%
            semi_join(edges.to.add, by = c("id" = "from")) %>%
            anti_join(temp.vertices.df, by = c("id"))
          temp.vertices.df = bind_rows(
            temp.vertices.df,
            vertices.to.add
          ) %>%
            distinct()
          temp.edges.df = bind_rows(
            temp.edges.df,
            edges.to.add
          ) %>%
            distinct()
          if(nrow(vertices.to.add) == 0) {
            found.all.vertices = T
          }
        }
        # Group by node so we can get the number of parts associated with each
        # node (= the size of the node).  Add a tooltip with a list of all the
        # part names associated with the node, where the background color of
        # each row is the color of the part.
        temp.vertices.df = temp.vertices.df %>%
          arrange(id, desc(total.parts)) %>%
          group_by(id, label, type, node.regex) %>%
          summarize(value = coalesce(sum(total.parts) ^ 0.5, 2),
                    any.pieces = sum(ifelse(is.na(total.parts), 0, 1)) > 0,
                    title = paste("<div style=\"background-color: white; overflow-y: auto; max-height: 300px\">",
                                  paste0(paste("<span style=\"color:",
                                               text.color.hex,
                                               "; background-color:",
                                               color.hex,
                                               "; display: inline-block; margin: 3px; padding: 3px\">",
                                               part.name,
                                               " (",
                                               total.parts,
                                               ")</span>",
                                               sep = ""),
                                         collapse = "</br>"),
                                  "</div>",
                                  sep = "")) %>%
          ungroup() %>%
          mutate(title = ifelse(any.pieces, title, NA),
                 color.background = ifelse(any.pieces, "white", "black"),
                 color.border = "black")
        # Make the dendrogram and add it to the current facet.
        current.facet[["dendrogram"]] = visNetwork(temp.vertices.df,
                                                   temp.edges.df,
                                                   main = themes.to.facet[current.theme.index]) %>%
          visNodes(scaling = list(min = 1, max = 100)) %>%
          visHierarchicalLayout(direction = "UD",
                                sortMethod = "directed") %>%
          visOptions(collapse = list(enabled = T,
                                     clusterOptions = list(color = "red")))
        facets[[current.theme.index]] = current.facet
        current.theme.index = current.theme.index + 1
      }
    }
  }
  facets
}



