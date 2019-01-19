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
# associated with multiple styles.
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
            by = c("theme.name"))

# Function to create a theme picker input.  We will need several of these.
theme.picker.input = function(input.id, column.with.counts) {
  temp.theme.counts.df = theme.counts.df
  temp.theme.counts.df[,"count.to.display"] = theme.counts.df[,column.with.counts]
  pickerInput(
    input.id, "Filter to specific themes:",
    choices = data.frame(
      temp.theme.counts.df %>%
        filter(count.to.display > 0) %>%
        mutate(label = paste(theme.name, " (", count.to.display, ")", sep = "")) %>%
        arrange(desc(count.to.display), theme.name)
    )$label,
    multiple = T
  )
}

# Function to create a (possibly faceted) circle graph.  Assumes that the first
# input is a dataframe with columns "facet.name", "facet.theme", "facet.other",
# "color.hex", "part.id", "part.name", "num.parts".
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
  # Pull out x, y, and r for each category.
  circle.facet.centers = circle.ggraph$data %>%
    filter(as.character(name) == as.character(facet.name)) %>%
    group_by(facet.theme) %>%
    mutate(x.center = x, y.center = y,
           r.center = max(r)) %>%
    ungroup() %>%
    dplyr::select(x.center, y.center, r.center, facet.name)
  # Rescale x, y, and r for each non-root so that each theme (facet) is
  # centered at (0, 0) and the same size.  Within a theme, sub-facets are the
  # same scale; therefore, facets with smaller counts will appear smaller.
  circle.faceted.data = circle.ggraph$data %>%
    rownames_to_column("rowname") %>%
    inner_join(circle.facet.centers, by = c("facet.name")) %>%
    mutate(x.faceted = (x - x.center) / r.center,
           y.faceted = (y - y.center) / r.center,
           r.faceted = r / r.center)
  # Feed the rescaled dataset into geom_circle.
  circle.facet.graph = ggplot(circle.faceted.data,
                              aes(x0 = x.faceted,
                                  y0 = y.faceted,
                                  r = r.faceted,
                                  fill = fill.to.plot,
                                  color = color.to.plot)) +
    geom_circle() +
    scale_fill_manual(values = sort(unique(as.character(circle.faceted.data$fill.to.plot)))) +
    scale_color_manual(values = sort(unique(as.character(circle.faceted.data$color.to.plot)))) +
    coord_equal() +
    guides(fill = F, color = F, size = F) +
    theme_void()
  if(facet.by.theme) {
    if(facet.by.other) {
      circle.facet.graph = circle.facet.graph +
        facet_grid(facet.theme ~ facet.other) +
        theme(strip.text.y = element_text(angle = -90))
    } else {
      circle.facet.graph = circle.facet.graph +
        facet_wrap(~ facet.theme)
    }
  } else if(facet.by.other) {
    circle.facet.graph = circle.facet.graph +
      facet_wrap(~ facet.other)
  }
  if(facet.by.theme | facet.by.other) {
    circle.facet.graph = circle.facet.graph +
      theme(strip.text = element_text(size = 20, face = "bold"))
  }
  circle.facet.graph
}

# Functions to determine what the height and width of a circle plot should be,
# given whether and how it's faceted.
circle.plot.width = function(num.themes, num.other) {
  numeric.width = 700
  if(num.other > 0) {
    if(num.themes > 0) {
      numeric.width = num.other * 300
    } else {
      numeric.width = wrap_dims(num.other)[2] * 300
    }
  } else if(num.themes > 0) {
    numeric.width = wrap_dims(num.themes)[2] * 300
  }
  return(paste(numeric.width, "px", sep = ""))
}
circle.plot.height = function(num.themes, num.other) {
  numeric.height = 700
  if(num.other > 0) {
    if(num.themes > 0) {
      numeric.height = num.themes * 300
    } else {
      numeric.height = wrap_dims(num.other)[1] * 300
    }
  } else if(num.themes > 0) {
    numeric.height = wrap_dims(num.themes)[1] * 300
  }
  return(paste(numeric.height, "px", sep = ""))
}

# Function to create a tooltip for a circle graph.
# https://gitlab.com/snippets/16220
circle.tooltip = function(hover, circle.graph.data.df, facet.by.theme, facet.by.other) {
  # Find the data point that corresponds to the circle the mouse is hovering
  # over.
  if(!is.null(hover)) {
    point = circle.graph.data.df %>%
      filter(leaf) %>%
      filter(r.faceted >= (((x.faceted - hover$x) ^ 2) + ((y.faceted - hover$y) ^ 2)) ^ .5)
    if(facet.by.other) {
      point = point %>%
        filter(as.character(facet.other) ==  hover$panelvar1)
      if(facet.by.theme) {
        point = point %>%
          filter(as.character(facet.theme) == hover$panelvar2)
      }
    } else if(facet.by.theme) {
      point = point %>%
        filter(as.character(facet.theme) == hover$panelvar1)
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
    p(HTML(paste(paste("<b>", point$total.parts,
                       " piece",
                       ifelse(point$total.parts == 1, "", "s"),
                       "</b>",
                       sep = ""),
                 as.character(point$part.name),
                 sep = "<br/>")))
  )
}
