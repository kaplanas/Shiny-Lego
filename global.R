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

# Spinner options.
options(spinner.type = 7,
        spinner.color = "#F2CD37")

# Download all the tables from rebrickable.com; name each one "tablename.df".
table.names = c("themes", "colors", "part_categories", "parts", "inventories",
                "sets", "inventory_parts", "inventory_sets",
                "part_relationships")
milliseconds.in.one.day = 1000 * 60 * 60 * 24
local.path.to.tables = "data_files/"
web.path.to.tables = "http://m.rebrickable.com/media/downloads/"
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
# Sub-theme nesting only goes two deep (I checked).
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
# associated with each (for displaying in input widgets).  We'll add the counts
# after creating the tables for each part type.
theme.counts.df = lego.df %>%
  group_by(theme.name) %>%
  summarize(total.parts = sum(total.parts, na.rm = T)) %>%
  ungroup()

# Create a table of minifigure heads (for demographics).  Infer gender from
# keywords in the part name, and the presence of facial hair.
heads.df = lego.df %>%
  filter(grepl("Mini(fig|doll) Heads", part.category.name)) %>%
  mutate(gender = case_when(grepl("\\b[Mm]ale\\b", part.name) ~ "Male",
                            grepl("\\b[Ff]emale\\b", part.name) ~ "Female",
                            grepl("\\b([Bb]eard|[Mm]o?ustache|[Ss]tubble|[Gg]oatee|[Ss]ideburn)", part.name) ~ "Male",
                            grepl("[Gg]irl|[Ww]oman", part.name) ~ "Female",
                            T ~ "Unknown"),
         num.parts = ifelse(is.na(inv.num.sets), 1, inv.num.sets) *
           ifelse(is.na(inv.num.parts), 1, inv.num.parts))

# Create a table of minifigure hair (for fashion).
hair.df = lego.df %>%
  filter(grepl("Headwear", part.category.name) &
           grepl("Hair", part.name)) %>%
  mutate(num.parts = ifelse(is.na(inv.num.sets), 1, inv.num.sets) *
           ifelse(is.na(inv.num.parts), 1, inv.num.parts))

# Create another table with hair styles and regular expressions for finding the
# keywords associated with each one.
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

# Associate each piece with its style(s); a piece gets multiple rows if it's
# associated with multiple styles.
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

# Add a row with the style "Other" for pieces that don't have any other style
# keywords.
hair.style.df = bind_rows(
  hair.style.df,
  hair.df %>%
    anti_join(hair.style.df, by = c("part.id")) %>%
    mutate(style = "Other")
)

# Create a table of minifigure clothes.
clothes.df = lego.df %>%
  filter(grepl("^Mini(fig|doll).*Body$", part.category.name)) %>%
  mutate(num.parts = ifelse(is.na(inv.num.sets), 1, inv.num.sets) *
           ifelse(is.na(inv.num.parts), 1, inv.num.parts),
         upper.lower = case_when(grepl("Upper", part.category.name) ~ "upper",
                                 grepl("Lower", part.category.name) ~ "lower"))

# Create another table with garment types and regular expressions for finding
# the keywords associated with each one.
clothes.type.words.df = data.frame(
  word = c("Shirt", "Jacket", "Armor", "Vest", "Suit", "Robe", "Skirt",
           "Overalls", "Sweater", "Uniform", "Trousers", "Coat", "Loincloth",
           "Halter", "Shorts", "Breastplate", "Blouse", "Corset", "Sweatshirt",
           "Apron", "Jumpsuit", "Bikini", "Spacesuit", "Wetsuit", "Leotard",
           "Swimsuit", "Blazer", "Waistcoat", "Tuxedo", "Jeans", "Kimono",
           "Nightgown", "Pajama"),
  regex = c("(t-)?shirt", "jacket", "armou?r", "vest", "\\bsuit", "robe",
            "skirt", "overalls|dungarees", "sweater", "uniform", "trousers",
            "coats?\\b", "loincloth", "halter", "shorts", "breastplate",
            "blouse", "corset", "sweatshirt", "apron", "jumpsuit", "bikini",
            "spacesuit", "wetsuit", "leotard", "swimsuit", "blazer",
            "waistcoat", "tuxedo", "jeans", "kimono", "nightgown", "pajama")
)

# Associate each piece with its type(s); a piece gets multiple rows if it's
# associated with multiple types.  If the keyword is immediately preceded by a
# color word, replace the color of the piece with the color word.  (If multiple
# color names match the string before the keyword, use the longest one; this is
# likely to be the most specific, and therefore correct, choice.)
clothes.type.df = do.call(
  "bind_rows",
  apply(
    clothes.type.words.df, 1,
    function(x) {
      temp.df = clothes.df %>%
        filter(grepl(x[2], gsub("[^A-Za-z0-9 ]", "", tolower(part.name)))) %>%
        mutate(type = x[1])
      temp.df$string.before.type = str_match(gsub("[^A-Za-z0-9 -]", "",
                                                  tolower(temp.df$part.name)),
                                             paste("^(.*[^ ]) [a-z]*",
                                                   x[2],
                                                   sep = ""))[,2]
      temp.colors.df = temp.df %>%
        select(part.id, string.before.type) %>%
        fuzzy_join(colors.df,
                   by = c("string.before.type" = "name"),
                   match_fun = function(x, y) { endsWith(x, tolower(y)) }) %>%
        group_by(part.id) %>%
        mutate(color.rank = row_number(name)) %>%
        filter(color.rank == 1) %>%
        select(part.id, name, is_trans, color.hex, text.color.hex)
      temp.df = temp.df %>%
        left_join(temp.colors.df, by = c("part.id")) %>%
        mutate(color.name = ifelse(is.na(name), color.name, name),
               color.hex = ifelse(is.na(name), color.hex.x, color.hex.y),
               color.is.trans = ifelse(is.na(name),
                                       color.is.trans,
                                       case_when(is_trans == "t" ~ T,
                                                 is_trans == "f" ~ F,
                                                 T ~ NA)),
               text.color.hex = ifelse(is.na(name),
                                       text.color.hex.x,
                                       text.color.hex.y)) %>%
        select(part.id, part.name, upper.lower, color.name, color.hex,
               color.is.trans, text.color.hex, set.name, theme.name,
               sub.theme.name, sub.sub.theme.name, num.parts, type)
      temp.df
    }
  )
)

# Add a row with the type "Other" for pieces that don't have any other type
# keywords.
clothes.type.df = bind_rows(
  clothes.type.df,
  clothes.df %>%
    anti_join(clothes.type.df, by = c("part.id")) %>%
    mutate(type = paste("Other",
                        ifelse(upper.lower == "upper", "Upper", "Lower")),
           string.before.type = NA)
)

# Create a table of minifigure accessories (for fashion).
accessories.df = lego.df %>%
  filter(part.category.name == "Minifig Accessories") %>%
  mutate(num.parts = ifelse(is.na(inv.num.sets), 1, inv.num.sets) *
           ifelse(is.na(inv.num.parts), 1, inv.num.parts))

# Create another table with accessories and regular expressions for finding the
# keywords associated with each one.
accessory.words.df = data.frame(
  word = c("Glasses", "Mask", "Headset", "Goggles", "Balaclava", "Headband",
           "Visor", "Tattoo", "Helmet", "Hood", "Crown", "Belt", "Cloak",
           "Bag", "Veil", "Bow", "Button", "Earrings", "Fan", "Jewelry",
           "Headgear", "Hat", "Headdress", "Badge", "Pocket", "Zipper", "Tie",
           "Sash", "Collar", "Necklace", "Boots", "Buckle", "Shoes", "Scarf",
           "Harness", "Sandals", "Medallion", "Suspenders", "Purse", "Amulet",
           "Ruffle", "Stethoscope", "Backpack", "Medal", "Ribbon", "Cape",
           "Tassel", "Gloves", "Watch", "Sneakers", "Socks", "Epaulettes",
           "Diaper", "Handcuffs", "Shawl", "Skates", "Life Preserver",
           "Poncho", "Breathing Apparatus", "Scuba Tank"),
  regex = c("(sun)?glass|monocle", "mask", "head(set|phone)", "goggle",
            "balaclava", "headband", "visor", "tattoo", "helmet", "hood|cowl",
            "crown|tiara", "belt|cummerbund", "cloak", "pouch|bag\\b", "veil",
            "bows?\\b", "button", "earring", "fan\\b",
            "jewel|gem|bead|br(o|a)ch", "headgear", "hats?\\b|caps?\\b|fez",
            "headdress", "badge", "pocket", "zipper", "\\b(neck)?tie\\b",
            "sash", "collar", "necklace|pend(a|e)nt|locket", "boot", "buckle",
            "\\bshoe", "scarf|neckerchief|bandann?a", "harness", "sandal",
            "medallion", "suspender", "purse", "amulet", "ruffle|frill",
            "stethoscope", "(back)?pack|knapsack", "medal", "ribbon", "cape",
            "tassel", "glove|gauntlet", "watch", "sneaker",
            "(sock|stocking)s?\\b", "epaulet", "diaper", "handcuff", "shawl",
            "skate", "preserver", "poncho", "breathing apparatus",
            "scuba tank")
)

# Associate each piece with its accessory(ies); a piece gets multiple rows if
# it's associated with multiple accessories.
accessory.parts.df = do.call(
  "bind_rows",
  apply(
    accessory.words.df, 1,
    function(x) {
      bind_rows(
        heads.df %>%
          filter(grepl(x[2], gsub("[^A-Za-z0-9 ]", "", tolower(part.name)))) %>%
          mutate(accessory = x[1]),
        hair.df %>%
          filter(grepl(x[2], gsub("[^A-Za-z0-9 ]", "", tolower(part.name)))) %>%
          mutate(accessory = x[1]),
        clothes.df %>%
          filter(grepl(x[2], gsub("[^A-Za-z0-9 ]", "", tolower(part.name)))) %>%
          mutate(accessory = x[1]),
        accessories.df %>%
          filter(grepl(x[2], gsub("[^A-Za-z0-9 ]", "", tolower(part.name)))) %>%
          mutate(accessory = x[1])
      )
    }
  )
)

# Create a table with all fashion items (hair, clothes, and accessories).
fashion.items.df = lego.df %>%
  left_join(hair.style.df %>%
              select(part.id, style) %>%
              distinct() %>%
              group_by(part.id) %>%
              summarize(hair.styles = paste0(style, collapse = ", ")),
            by = c("part.id")) %>%
  left_join(clothes.type.df %>%
              select(part.id, type) %>%
              distinct() %>%
              group_by(part.id) %>%
              summarize(clothing.types = paste0(type, collapse = ", ")),
            by = c("part.id")) %>%
  left_join(accessory.parts.df %>%
              select(part.id, accessory) %>%
              distinct() %>%
              group_by(part.id) %>%
              summarize(accessories = paste0(accessory, collapse = ", ")),
            by = c("part.id")) %>%
  filter(!is.na(hair.styles) |
           !is.na(clothing.types) |
           !is.na(accessories))

# Create another table with moods and regular expressions for finding the
# keywords associated with each one.
mood.words.df = data.frame(
  word = c("Happy", "Angry", "Afraid", "Sad"),
  regex = c("smil(e|ing)|grin|smirk|happy|laugh",
            "angry|stern|scowl|gritt(ed|ing)|grim\\b|rag(e|ing)|sneer|annoy|fierce|frustrated",
            "scared|worried|sur?prised|concern|alarm|anxious|shock|tense|troubled|upset",
            "sad|tear|crying|emb(a|e)rrassed|pout")
)

# Associate each piece with its mood(s); a piece gets multiple rows if it's
# associated with multiple moods.
moods.df = do.call(
  "bind_rows",
  apply(
    mood.words.df, 1,
    function(x) {
      temp.df = heads.df %>%
        filter(grepl(x[2], tolower(part.name))) %>%
        mutate(mood = x[1])
      temp.df
    }
  )
)

# If a piece has no other mood keywords, "frown" -> "sad".  (Otherwise, "frown"
# is ambiguous; it could be "sad" or "angry".)
moods.df = bind_rows(
  moods.df,
  heads.df %>%
    filter(grepl("frown", tolower(part.name)) &
             !grepl(mood.words.df$regex[mood.words.df$word == "Angry"],
                    tolower(part.name))) %>%
    mutate(mood = "Sad")
)

# Associate a color with each mood.
moods.df = moods.df %>%
  mutate(mood.color = case_when(mood == "Happy" ~ "#F2CD37",
                                mood == "Sad" ~ "#5A93DB",
                                mood == "Angry" ~ "#C91A09",
                                mood == "Afraid" ~ "#AC78BA"))

# Make mood an ordered factor so the graphs display in the correct, consistent
# order.
moods.df$mood = factor(moods.df$mood,
                       levels = c("Happy", "Sad", "Afraid", "Angry"))

# Read in a dump of WordNet hypernyms.  We could get hypernyms using the
# "wordnet" package, but we would have to look for hypernys one word at a time;
# this is faster, because we want to get the whole graph and then find paths
# from specific words to "plant" or "animal" (both to determine which words
# actually refer to plants/animals, and to find their place in the tree of
# life).
hypernyms.df = bind_rows(
  read.csv("data_files/direct_hypernyms.csv", header = T,
           stringsAsFactors = F),
  setNames(read.csv("data_files/direct_hypernyms_2.csv", header = F,
                    stringsAsFactors = F),
           names(read.csv("data_files/direct_hypernyms.csv", header = T,
                          nrows = 3))),
  setNames(read.csv("data_files/direct_hypernyms_3.csv", header = F,
                    stringsAsFactors = F),
           names(read.csv("data_files/direct_hypernyms.csv", header = T,
                          nrows = 3)))
)

# Get a quick mapping from wordforms to lemmas, used for mapping plant/animal
# keywords to lemmas in WordNet.
data(hash_lemmas)

# Get plant and animal pieces.
ecology.df = lego.df %>%
  filter(part.category.name == "Plants and Animals")

# Get all the words used in plant/animal pieces, and their frequencies.
ecology.words.df = data.frame(word = gsub("[^A-Za-z ]", "",
                                          tolower(unlist(strsplit(sort(unique(ecology.df$part.name)),
                                                                  " "))))) %>%
  group_by(word) %>%
  summarize(freq = n()) %>%
  arrange(desc(freq)) %>%
  ungroup()

# Create a directed graph whose edges go from words to their hyponyms.  Plants
# and animals only.
ecology.edges = hypernyms.df %>%
  filter(is.element(A_LEXDOMAINNAME, c("noun.animal", "noun.plant"))) %>%
  mutate(to = A_LEMMA,
         from.synset = C_SYNSETID,
         type = gsub("noun\\.", "", A_LEXDOMAINNAME)) %>%
  select(to, from.synset, type) %>%
  distinct() %>%
  left_join(hypernyms.df, by = c("from.synset" = "A_SYNSETID")) %>%
  mutate(from = A_LEMMA) %>%
  select(from, to, type) %>%
  distinct()

# Remove certain subtypes of animals (e.g., "predatory animal") to reduce the
# number of distinct ways a piece may be classified.  We want all the words to
# be classified according to the same system.
ecology.edges = ecology.edges %>%
  filter(!is.element(to, c("predatory animal", "domesticated animal",
                           "predator", "young", "male", "female",
                           "offspring", "domestic animal", "insectivore",
                           "herb"))) %>%
  filter(to != "chestnut" | !grepl("equus|callus|horse", from))

# Add edges for a few words that don't follow the desired classification system
# (i.e., words that are classified only as something like "young mammal").
ecology.edges = bind_rows(
  ecology.edges,
  data.frame(
    from = c("feline", "bovid", "equine"),
    to = c("kitten", "lamb", "foal"),
    type = rep("animal", 3),
    stringsAsFactors = F
  )
)

# Make the igraph.
ecology.igraph = graph_from_data_frame(ecology.edges)

# For each word in the plant/animal pieces, if that word is a plant/animal
# word, get the shortest path from that word to "plant"/"animal" and add that
# path to the filtered graph of edges.  Skip words that usually do not refer to
# a plant or animal when they appear in a part name.
ecology.edges.filtered = data.frame()
for(i in 1:nrow(ecology.words.df)) {
  current.word = as.character(ecology.words.df$word[i])
  current.lemma = hash_lemmas$lemma[hash_lemmas$token == current.word]
  if(length(current.lemma) == 0) {
    current.lemma = current.word
  }
  current.types = unique(ecology.edges$type[ecology.edges$to == current.lemma])
  if(!is.element(current.lemma, c("stud", "spike", "head", "gray", "shell",
                                  "ass", "fang", "plug", "royal", "grey",
                                  "ornamental", "olive")) &
     !is.element(current.lemma, tolower(colors.df$name))) {
    for(type in current.types) {
      path = suppressWarnings(shortest_paths(ecology.igraph,
                                             to = current.lemma, from = type)$vpath[[1]])
      if(length(path) > 0) {
        for(j in 1:(length(path) - 1)) {
          ecology.edges.filtered = bind_rows(
            ecology.edges.filtered,
            data.frame(
              from = path[j]$name,
              to = path[j + 1]$name,
              type = type,
              depth = j,
              stringsAsFactors = F
            )
          )
        }
      }
    }
  }
}
rm(i, current.word, current.lemma, current.types, type, path, j)
ecology.edges.filtered = ecology.edges.filtered %>%
  distinct()

# Create a data frame of edges.
ecology.edges.vis.df = ecology.edges.filtered %>%
  mutate(color = "black")

# Create a data frame of nodes.  Add a regular expression to each node that
# matches the name of the node and related wordforms, so that we can later
# match nodes to part names that contain the relevant keywords.
ecology.vertices.vis.df = bind_rows(
  ecology.edges.filtered %>%
    mutate(name = from) %>%
    select(name, type),
  ecology.edges.filtered %>%
    mutate(name = to) %>%
    select(name, type)
) %>%
  distinct() %>%
  left_join(ecology.edges.filtered, by = c("name" = "to", "type")) %>%
  mutate(depth = ifelse(is.na(depth), 0, depth)) %>%
  select(name, depth, type) %>%
  left_join(hash_lemmas, by = c("name" = "lemma")) %>%
  group_by(name, depth, type) %>%
  summarize(node.regex = paste0(token, collapse = "|")) %>%
  ungroup() %>%
  mutate(id = name,
         title = name,
         label = name,
         node.regex = paste("\\b(", name,
                            ifelse(node.regex == "NA",
                                   "", paste("|", node.regex, sep = "")),
                            ")\\b", sep = "")) %>%
  select(id, label, title, depth, node.regex, type)

# For each part, determine which node that part is associated with (by matching
# keywords in the part name to node names).  If a part matches multiple nodes,
# choose the one furthest down in the tree.
ecology.parts.nodes.df = ecology.df %>%
  group_by(part.id, part.name, color.hex, text.color.hex, theme.name) %>%
  summarize(total.parts = sum(total.parts)) %>%
  ungroup() %>%
  mutate(lower.part.name = tolower(part.name)) %>%
  regex_inner_join(ecology.vertices.vis.df,
                   by = c(lower.part.name = "node.regex"),
                   ignore_case = T) %>%
  group_by(part.id, part.name, type) %>%
  filter(depth == max(depth)) %>%
  ungroup() %>%
  select(node.name = id, part.id, part.name, color.hex, text.color.hex,
         total.parts, theme.name, type)

# Add the parts to each node.  Don't group here; that happens just before
# plotting, because we might or might not need to facet by theme, and that
# affects the size of each node.
ecology.vertices.vis.df = ecology.vertices.vis.df %>%
  left_join(ecology.parts.nodes.df, by = c("id" = "node.name", "type"))

# Update theme count table.
theme.counts.df = theme.counts.df %>%
  left_join(heads.df %>%
              group_by(theme.name, color.hex) %>%
              summarize(color.num.parts = sum(num.parts)) %>%
              ungroup() %>%
              group_by(theme.name) %>%
              summarize(ethnic.diversity = -1 *
                          sum((color.num.parts /
                                 sum(color.num.parts)) *
                                log(color.num.parts /
                                      sum(color.num.parts),
                                    2)),
                        total.heads = sum(color.num.parts)) %>%
              ungroup() %>%
              dplyr::select(theme.name, total.heads, ethnic.diversity),
            by = c("theme.name")) %>%
  left_join(heads.df %>%
              group_by(theme.name) %>%
              summarize(pct.female = (sum(ifelse(gender == "Female", 1, 0)) /
                                        sum(ifelse(gender == "Male" | gender == "Female", 1, 0))) * 100) %>%
              select(theme.name, pct.female),
            by = c("theme.name")) %>%
  left_join(hair.df %>%
              group_by(theme.name) %>%
              summarize(total.hair = sum(num.parts)) %>%
              select(theme.name, total.hair) %>%
              distinct(),
            by = c("theme.name")) %>%
  left_join(clothes.df %>%
              group_by(theme.name) %>%
              summarize(total.clothes = sum(num.parts)) %>%
              select(theme.name, total.clothes) %>%
              distinct(),
            by = c("theme.name")) %>%
  left_join(accessory.parts.df %>%
              group_by(theme.name) %>%
              summarize(total.accessories = sum(num.parts)),
            by = c("theme.name")) %>%
  left_join(fashion.items.df %>%
              group_by(theme.name) %>%
              summarize(total.fashion.items = sum(total.parts)),
            by = c("theme.name")) %>%
  left_join(moods.df %>%
              group_by(theme.name) %>%
              summarize(total.moods = sum(num.parts)),
            by = c("theme.name")) %>%
  left_join(ecology.parts.nodes.df %>%
              filter(type == "plant") %>%
              group_by(theme.name, color.hex) %>%
              summarize(color.num.parts = sum(total.parts)) %>%
              ungroup() %>%
              group_by(theme.name) %>%
              summarize(plant.species.diversity = -1 *
                          sum((color.num.parts /
                                 sum(color.num.parts)) *
                                log(color.num.parts /
                                      sum(color.num.parts),
                                    2)),
                        total.plants = sum(color.num.parts)) %>%
              ungroup() %>%
              dplyr::select(theme.name, total.plants, plant.species.diversity),
            by = c("theme.name")) %>%
  left_join(ecology.parts.nodes.df %>%
              filter(type == "animal") %>%
              group_by(theme.name, color.hex) %>%
              summarize(color.num.parts = sum(total.parts)) %>%
              ungroup() %>%
              group_by(theme.name) %>%
              summarize(animal.species.diversity = -1 *
                          sum((color.num.parts /
                                 sum(color.num.parts)) *
                                log(color.num.parts /
                                      sum(color.num.parts),
                                    2)),
                        total.animals = sum(color.num.parts)) %>%
              ungroup() %>%
              dplyr::select(theme.name, total.animals, animal.species.diversity),
            by = c("theme.name")) %>%
  left_join(ecology.parts.nodes.df %>%
              group_by(theme.name) %>%
              summarize(total.plants.animals = sum(total.parts)),
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
