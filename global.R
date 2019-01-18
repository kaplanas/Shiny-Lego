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
  ungroup() %>%
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

# Update theme count table.
theme.counts.df = theme.counts.df %>%
  left_join(heads.df %>%
              mutate(total.heads = theme.num.parts) %>%
              select(theme.name, total.heads) %>%
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
    options = list(`actions-box` = T),
    multiple = T
  )
}
