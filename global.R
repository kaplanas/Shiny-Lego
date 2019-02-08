library(dplyr)
library(tibble)
library(ggplot2)
library(ggforce)
library(shiny)
library(shinyWidgets)
library(igraph)
library(ggraph)
library(scales)
library(DT)
library(treemap)
library(highcharter)
library(purrr)

theme_set(theme_bw())

# Download all the tables from rebrickable.com; name each one "tablename.df".
table.names = c("themes", "colors", "part_categories", "parts", "inventories",
                "sets", "inventory_parts", "inventory_sets",
                "part_relationships")
path.to.tables = "data_files/"
# path.to.tables = "http://m.rebrickable.com/media/downloads/"
for(table.name in table.names) {
  assign(paste(gsub("_", "\\.", table.name), "df", sep = "."),
         read.csv(paste(path.to.tables, table.name, ".csv", sep = ""),
                  stringsAsFactors = F))
}
rm(table.name)

# Add useful columns to the table of colors: hexadecimal (with prefixed
# octothorpe) and the color text should be when printed on that background
# (black or white).
# https://stackoverflow.com/questions/3942878/how-to-decide-font-color-in-white-or-black-depending-on-background-color
colors.df = colors.df %>%
  mutate(color.hex = paste("#", rgb, sep = ""),
         text.color.hex = ifelse(((strtoi(paste("0X",
                                                substr(rgb, 1, 2),
                                                sep = "")) * 0.299) +
                                    (strtoi(paste("0X",
                                                  substr(rgb, 3, 4),
                                                  sep = "")) * 0.587) +
                                    (strtoi(paste("0X",
                                                  substr(rgb, 6, 7),
                                                  sep = "")) * 0.114)) > 186,
                                 "#000000",
                                 "#FFFFFF"))

# Create a table of themes that flattens out the theme tree structure.
theme.tree.df = themes.df %>%
  mutate(theme.id = id,
         theme.name = name,
         parent.id = parent_id) %>%
  select(theme.id, theme.name, parent.id) %>%
  left_join(themes.df, by = c("parent.id" = "id")) %>%
  mutate(parent.name = name,
         grandparent.id = parent_id) %>%
  select(theme.id, theme.name, parent.id, parent.name, grandparent.id) %>%
  left_join(themes.df, by = c("grandparent.id" = "id")) %>%
  mutate(grandparent.name = name) %>%
  select(theme.id, theme.name, parent.id, parent.name, grandparent.id,
         grandparent.name) %>%
  mutate(sub.sub.theme.name = case_when(!is.na(grandparent.name) ~ theme.name),
         sub.theme.name = case_when(!is.na(grandparent.name) ~ parent.name,
                                    !is.na(parent.name) ~ theme.name),
         theme.name = case_when(!is.na(grandparent.name) ~ grandparent.name,
                                !is.na(parent.name) ~ parent.name,
                                T ~ theme.name)) %>%
  select(theme.id, theme.name, sub.theme.name, sub.sub.theme.name)

# Create a single table that combines the data in a useful way.
lego.df = sets.df %>%
  mutate(set.id = set_num,
         set.name = name,
         set.num.parts = num_parts,
         theme.id = theme_id) %>%
  left_join(theme.tree.df, by = c("theme.id")) %>%
  left_join(inventory.sets.df, by = c("set.id" = "set_num")) %>%
  left_join(inventories.df, by = c("set.id" = "set_num")) %>%
  mutate(inventory.id = ifelse(is.na(inventory_id), id, inventory_id),
         inv.num.sets = quantity) %>%
  left_join(inventory.parts.df, by = c("inventory.id" = "inventory_id")) %>%
  mutate(inv.num.parts = quantity.y,
         part.is.spare = case_when(is_spare == "f" ~ F,
                                   is_spare == "t" ~ T)) %>%
  left_join(parts.df, by = c("part_num")) %>%
  mutate(part.id = part_num,
         part.name = name.y,
         total.parts = ifelse(is.na(inv.num.sets), 1, inv.num.sets) *
           ifelse(is.na(inv.num.parts), 1, inv.num.parts)) %>%
  left_join(part.categories.df, by = c("part_cat_id" = "id")) %>%
  mutate(part.category.id = part_cat_id,
         part.category.name = name) %>%
  left_join(colors.df, by = c("color_id" = "id")) %>%
  mutate(color.id = color_id,
         color.name = name.y.y,
         color.is.trans = case_when(is_trans == "f" ~ F,
                                    is_trans == "t" ~ T)) %>%
  select(set.id, set.name, year, set.num.parts, theme.id, theme.name,
         sub.theme.name, sub.sub.theme.name, inventory.id, inv.num.sets,
         inv.num.parts, total.parts, part.id, part.name, part.category.id,
         part.category.name, part.is.spare, color.id, color.name, color.hex,
         color.is.trans, text.color.hex)

# Create a data frame with distinct top-level themes and various counts
# associated with each (for displaying in input widgets).
theme.counts.df = lego.df %>%
  group_by(theme.name) %>%
  summarize(total.parts = sum(total.parts, na.rm = T)) %>%
  ungroup()

# Create a table of minifigure heads (for demographics).
heads.df = lego.df %>%
  filter(grepl("Mini(fig|doll) Heads", part.category.name)) %>%
  mutate(gender = case_when(grepl("\\b[Mm]ale\\b", part.name) ~ "Male",
                            grepl("\\b[Ff]emale\\b", part.name) ~ "Female",
                            grepl("\\b([Bb]eard|[Mm]o?ustache|[Ss]tubble|[Gg]oatee|[Ss]ideburn)", part.name) ~ "Male",
                            grepl("[Gg]irl|[Ww]oman", part.name) ~ "Female",
                            T ~ "Unknown"),
         num.parts = ifelse(is.na(inv.num.sets), 1, inv.num.sets) *
           ifelse(is.na(inv.num.parts), 1, inv.num.parts)) %>%
  group_by(theme.name) %>%
  mutate(theme.num.parts = sum(num.parts),
         theme.pct.female = (sum(ifelse(gender == "Female", 1, 0)) /
           sum(ifelse(gender == "Male" | gender == "Female", 1, 0))) * 100) %>%
  ungroup()
heads.df = heads.df %>%
  left_join(heads.df %>%
              group_by(theme.name, color.hex) %>%
              summarize(theme.color.num.parts = sum(num.parts)) %>%
              ungroup() %>%
              group_by(theme.name) %>%
              summarize(theme.ethnic.diversity = -1 *
                          sum((theme.color.num.parts /
                                 sum(theme.color.num.parts)) *
                                log(theme.color.num.parts /
                                      sum(theme.color.num.parts),
                                    2))) %>%
              dplyr::select(theme.name, theme.ethnic.diversity),
            by = c("theme.name")) %>%
  select(part.id, part.name, color.name, color.hex, color.is.trans,
         text.color.hex, gender, set.name, theme.name, sub.theme.name,
         sub.sub.theme.name, num.parts, theme.num.parts, theme.pct.female,
         theme.ethnic.diversity)

# Create a table of minifigure headwear (for fashion).
hair.df = lego.df %>%
  filter(grepl("Headwear", part.category.name) &
           grepl("Hair", part.name)) %>%
  mutate(num.parts = ifelse(is.na(inv.num.sets), 1, inv.num.sets) *
           ifelse(is.na(inv.num.parts), 1, inv.num.parts)) %>%
  group_by(theme.name) %>%
  mutate(theme.num.parts = sum(num.parts)) %>%
  ungroup() %>%
  select(part.id, part.name, color.name, color.hex, color.is.trans,
         text.color.hex, set.name, theme.name, sub.theme.name,
         sub.sub.theme.name, num.parts, theme.num.parts)

# Create another table with style keywords; a piece gets multiple rows if it's
# associated with multiple styles.  Add a row with the style "Other" for pieces
# that don't have any other style keywords.
hair.style.words.df = data.frame(
  word = c("Long", "Bangs", "Ponytail", "Short", "Wavy", "Mid-length", "Bun",
           "Tousled", "Braid", "Straight", "Spiked", "Bushy",
           "Curled", "Widow's peak", "Bob", "Pigtails", "Tied", "Bald",
           "Coiled", "Dreadlocks", "Bowl", "Mohawk", "Afro",
           "Combover", "Mullet"),
  regex = c("long", "bangs", "ponytail", "short", "wavy", "midlength", "bun",
            "tousled", "braid", "straight", "spik(ed|y)", "bushy",
            "curl(ed|y|s)", "widows peak", "bob", "pigtails", "tie(d|s)",
            "bald", "coil", "dreadlocks", "bowl", "mohawk", "afro", "combover",
            "mullet")
)
hair.style.df = do.call(
  "bind_rows",
  apply(
    hair.style.words.df, 1,
    function(x) {
      hair.df %>%
        filter(grepl(x[2], gsub("[^A-Za-z0-9]", "", tolower(part.name)))) %>%
        mutate(style = x[1])
    }
  )
)
hair.style.df = bind_rows(
  hair.style.df,
  hair.df %>%
    anti_join(hair.style.df, by = c("part.id")) %>%
    mutate(style = "Other")
)

# Create another table with (garment) type keywords; a piece gets multiple rows
# if it's associated with multiple types.  Add a row with the type "Other" for
# pieces that don't have any other type keywords.
clothes.type.words.df = data.frame(
  word = c("Shirt", "Jacket", "Armor", "Vest", "Suit", "Robe", "Skirt",
           "Overalls", "Sweater", "Uniform", "Trousers", "Coat", "Loincloth",
           "Halter", "Shorts", "Breastplate", "Blouse", "Corset", "Sweatshirt",
           "T-shirt", "Apron", "Jumpsuit", "Bikini", "Spacesuit", "Wetsuit",
           "Leotard", "Swimsuit", "Blazer", "Waistcoat", "Tuxedo", "Jeans",
           "Kimono", "Nightgown", "Pajama"),
  regex = c("shirt", "Jacket", "armou?r", "vest", "\\bsuit", "robe", "skirt",
            "overalls|dungarees", "sweater", "uniform", "trousers",
            "coats?\\b", "loincloth", "halter", "shorts", "breastplate",
            "blouse", "corset", "sweatshirt", "tshirt", "apron", "jumpsuit",
            "bikini", "spacesuit", "wetsuit", "leotard", "swimsuit", "blazer",
            "waistcoat", "tuxedo", "jeans", "kimono", "nightgown", "pajama")
)
clothes.type.df = do.call(
  "bind_rows",
  apply(
    clothes.type.words.df, 1,
    function(x) {
      clothes.df %>%
        filter(grepl(x[2], gsub("[^A-Za-z0-9]", "", tolower(part.name)))) %>%
        mutate(type = x[1])
    }
  )
)
clothes.type.df = bind_rows(
  clothes.type.df,
  clothes.df %>%
    anti_join(clothes.type.df, by = c("part.id")) %>%
    mutate(type = paste("Other",
                        ifelse(upper.lower == "upper", "Upper", "Lower")))
)

# Update theme count table.
theme.counts.df = theme.counts.df %>%
  left_join(heads.df %>%
              mutate(total.heads = theme.num.parts) %>%
              select(theme.name, total.heads) %>%
              distinct(),
            by = c("theme.name")) %>%
  left_join(hair.df %>%
              mutate(total.hair = theme.num.parts) %>%
              select(theme.name, total.hair) %>%
              distinct(),
            by = c("theme.name")) %>%
  left_join(clothes.df %>%
              mutate(total.clothes = theme.num.parts) %>%
              select(theme.name, total.clothes) %>%
              distinct(),
            by = c("theme.name"))

# Function to create a theme picker input.  We will need several of these.
theme.picker.input = function(input.id, column.with.counts) {
  temp.theme.counts.df = theme.counts.df
  temp.theme.counts.df[,"count.to.display"] = theme.counts.df[,column.with.counts]
  pickerInput(
    input.id, "Filter to one or more themes:",
    choices = data.frame(
      temp.theme.counts.df %>%
        filter(count.to.display > 0) %>%
        mutate(label = paste(theme.name, " (", count.to.display, ")", sep = "")) %>%
        arrange(desc(count.to.display), theme.name)
    )$label,
    multiple = T
  )
}

# Height and width of (1) an unfaceted circle plot, or (2) each facet of a
# faceted circle plot.
circle.plot.dims = list(height = 700, width = 700)
circle.plot.facet.dims = list(height = 300, width = 300)

# Function to create a (possibly faceted) circle graph.  Assumes that the first
# input is a dataframe with columns "facet.name", "facet.theme", "facet.other",
# "color.hex", "part.id", "part.name", "num.parts".  General strategy: use
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
treemap.graph = function(data.df, label.rules) {
  # Create a dataframe that encodes the tree structure for Highcharts.
  treemap.df = bind_rows(
    # Add one row for each top-level category.
    data.df %>%
      group_by(theme.name, id.level.1, name.level.1, color.level.1,
               opacity.level.1) %>%
      summarize(total.parts = sum(num.parts)) %>%
      ungroup() %>%
      mutate(name = name.level.1,
             id = id.level.1,
             level = 1,
             value = total.parts,
             valuecolor = total.parts,
             color = color.level.1,
             opacity = opacity.level.1,
             parent = NA,
             index.1 = NA,
             index.2 = NA) %>%
      select(theme.name, name, id, level, value, valuecolor, color, parent,
             index.1, index.2, opacity),
    # Add one row for each second-level category.
    data.df %>%
      group_by(theme.name, id.level.1, id.level.2, name.level.2, color.level.2,
               opacity.level.2) %>%
      summarize(total.parts = sum(num.parts)) %>%
      ungroup() %>%
      mutate(name = name.level.2,
             id = id.level.2,
             level = 2,
             value = total.parts,
             valuecolor = total.parts,
             color = color.level.2,
             opacity = opacity.level.2,
             parent = id.level.1,
             index.1 = id.level.1,
             index.2 = NA) %>%
      select(theme.name, name, id, level, value, valuecolor, color, parent,
             index.1, index.2, opacity)
  )
  # Add one row for each third-level category, if any.
  if(is.element("id.level.3", colnames(data.df))) {
    treemap.df = bind_rows(
      treemap.df,
      data.df %>%
        group_by(theme.name, id.level.1, id.level.2, id.level.3,
                 name.level.3, color.level.3) %>%
        summarize(total.parts = sum(num.parts)) %>%
        ungroup() %>%
        mutate(name = name.level.3,
               id = id.level.3,
               level = 3,
               value = total.parts,
               valuecolor = total.parts,
               color = color.level.3,
               parent = id.level.2,
               index.1 = id.level.2,
               index.2 = id.level.3,
               index.3 = NA) %>%
        select(theme.name, name, id, level, value, valuecolor, color, parent,
               index.1, index.2, index.3)
    )
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
                      levels = list(
                        list(level = 1,
                             dataLabels = list(enabled = label.rules[["1"]][["enabled"]]),
                             borderWidth = 5),
                        list(level = 2,
                             dataLabels = list(enabled = label.rules[["2"]][["enabled"]])),
                        list(level = 3,
                             dataLabels = list(enabled = label.rules[["3"]][["enabled"]]))
                      ))
      if(x != "") {
        hc = hc %>%
          hc_title(text = x)
      }
      hc
    }
  ) %>%
    hw_grid()
}
