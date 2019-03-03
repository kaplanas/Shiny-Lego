observe({
  
  # Check again after one week.
  invalidateLater(milliseconds.in.one.week)
  
  # Are the files in Dropbox more than a week old?
  dropbox.timestamp = ""
  while(dropbox.timestamp == "") {
    dropbox.timestamp = try(
      drop_get_metadata(paste(dropbox.path.to.munged.tables,
                              "theme_counts.csv", sep = ""))$server_modified,
      silent = T
    )
  }
  dropbox.is.old = interval(ymd_hms(dropbox.timestamp), Sys.time()) %/% days(1) > 7
  any.rebrickable.failures = F
  
  # If so, download new data from Rebrickable and re-create the crucial
  # dataframes.
  if(dropbox.is.old) {
    
    # Download the raw data.  If any of the downloads fail, make a note of this and quit.
    for(table.name in table.names) {
      df.name = paste(gsub("_", "\\.", table.name), "df", sep = ".")
      file.name = paste(local.path.to.tables, table.name, ".csv", sep = "")
      possible.error = tryCatch(
        assign(df.name,
               read.csv(paste(web.path.to.tables, table.name, ".csv", sep = ""),
                        stringsAsFactors = F)),
        warning = function(e) { e },
        error = function(e) { e }
      )
      if(inherits(possible.error, "error") | inherits(possible.error, "warning")) {
        any.rebrickable.failures = T
        break
      }
    }
    rm(table.name)
    
    # If we successfully downloaded all the tables, munge them.
    if(!any.rebrickable.failures) {
      
      # Add useful columns to the table of colors: hexadecimal (with prefixed
      # octothorpe) and the color text should be when printed on that background
      # (black or white).
      # https://stackoverflow.com/questions/3942878/how-to-decide-font-color-in-white-or-black-depending-on-background-color
      colors.df = colors.df %>%
        mutate(color.name = name,
               color.hex = paste("#", rgb, sep = ""),
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
      colors.df$color.name = as.character(colors.df$color.name)
      colors.df <<- colors.df
      
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
      # associated with each (for displaying in input widgets).  We'll add the
      # counts after creating the tables for each part type.
      theme.counts.df = lego.df %>%
        group_by(theme.name) %>%
        summarize(total.parts = sum(total.parts, na.rm = T)) %>%
        ungroup()
      
      # Create a table of minifigure heads (for demographics).  Infer gender
      # from keywords in the part name, and the presence of facial hair.
      heads.df <<- lego.df %>%
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
      
      # Create another table with hair styles and regular expressions for
      # finding the keywords associated with each one.
      hair.style.words.df = data.frame(
        word = c("Long", "Bangs", "Ponytail", "Short", "Wavy", "Mid-length",
                 "Bun", "Tousled", "Braid", "Straight", "Spiked", "Bushy",
                 "Curled", "Widow's peak", "Bob", "Pigtails", "Tied", "Bald",
                 "Coiled", "Dreadlocks", "Bowl", "Mohawk", "Afro", "Combover",
                 "Mullet"),
        regex = c("long", "bangs", "ponytail", "short", "wavy", "midlength",
                  "bun", "tousled", "braid", "straight", "spik(ed|y)", "bushy",
                  "curl(ed|y|s)", "widows peak", "bob", "pigtails", "tie(d|s)",
                  "bald", "coil", "dreadlocks", "bowl", "mohawk", "afro",
                  "combover", "mullet")
      )
      
      # Associate each piece with its style(s); a piece gets multiple rows if
      # it's associated with multiple styles.
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
      
      # Add a row with the style "Other" for pieces that don't have any other
      # style keywords.
      hair.style.df = bind_rows(
        hair.style.df,
        hair.df %>%
          anti_join(hair.style.df, by = c("part.id")) %>%
          mutate(style = "Other")
      )
      hair.style.df <<- hair.style.df
      
      # Create a table of minifigure clothes.
      clothes.df = lego.df %>%
        filter(grepl("^Mini(fig|doll).*Body$", part.category.name)) %>%
        mutate(num.parts = ifelse(is.na(inv.num.sets), 1, inv.num.sets) *
                 ifelse(is.na(inv.num.parts), 1, inv.num.parts),
               upper.lower = case_when(grepl("Upper", part.category.name) ~ "upper",
                                       grepl("Lower", part.category.name) ~ "lower"))
      
      # Create another table with garment types and regular expressions for
      # finding the keywords associated with each one.
      clothes.type.words.df = data.frame(
        word = c("Shirt", "Jacket", "Armor", "Vest", "Suit", "Robe", "Skirt",
                 "Overalls", "Sweater", "Uniform", "Trousers", "Coat",
                 "Loincloth", "Halter", "Shorts", "Breastplate", "Blouse",
                 "Corset", "Sweatshirt", "Apron", "Jumpsuit", "Bikini",
                 "Spacesuit", "Wetsuit", "Leotard", "Swimsuit", "Blazer",
                 "Waistcoat", "Tuxedo", "Jeans", "Kimono", "Nightgown",
                 "Pajama"),
        regex = c("(t-)?shirt", "jacket", "armou?r", "vest", "\\bsuit", "robe",
                  "skirt", "overalls|dungarees", "sweater", "uniform",
                  "trousers", "coats?\\b", "loincloth", "halter", "shorts",
                  "breastplate", "blouse", "corset", "sweatshirt", "apron",
                  "jumpsuit", "bikini", "spacesuit", "wetsuit", "leotard",
                  "swimsuit", "blazer", "waistcoat", "tuxedo", "jeans",
                  "kimono", "nightgown", "pajama")
      )
      
      # Associate each piece with its type(s); a piece gets multiple rows if
      # it's associated with multiple types.  If the keyword is immediately
      # preceded by a color word, replace the color of the piece with the color
      # word.  (If multiple color names match the string before the keyword,
      # use the longest one; this is likely to be the most specific, and
      # therefore correct, choice.)
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
      
      # Add a row with the type "Other" for pieces that don't have any other
      # type keywords.
      clothes.type.df = bind_rows(
        clothes.type.df,
        clothes.df %>%
          anti_join(clothes.type.df, by = c("part.id")) %>%
          mutate(type = paste("Other",
                              ifelse(upper.lower == "upper", "Upper", "Lower")),
                 string.before.type = NA)
      )
      clothes.type.df <<- clothes.type.df
      
      # Create a table of minifigure accessories (for fashion).
      accessories.df = lego.df %>%
        filter(part.category.name == "Minifig Accessories") %>%
        mutate(num.parts = ifelse(is.na(inv.num.sets), 1, inv.num.sets) *
                 ifelse(is.na(inv.num.parts), 1, inv.num.parts))
      
      # Create another table with accessories and regular expressions for
      # finding the keywords associated with each one.
      accessory.words.df = data.frame(
        word = c("Glasses", "Mask", "Headset", "Goggles", "Balaclava",
                 "Headband", "Visor", "Tattoo", "Helmet", "Hood", "Crown",
                 "Belt", "Cloak", "Bag", "Veil", "Bow", "Button", "Earrings",
                 "Fan", "Jewelry", "Headgear", "Hat", "Headdress", "Badge",
                 "Pocket", "Zipper", "Tie", "Sash", "Collar", "Necklace",
                 "Boots", "Buckle", "Shoes", "Scarf", "Harness", "Sandals",
                 "Medallion", "Suspenders", "Purse", "Amulet", "Ruffle",
                 "Stethoscope", "Backpack", "Medal", "Ribbon", "Cape",
                 "Tassel", "Gloves", "Watch", "Sneakers", "Socks",
                 "Epaulettes", "Diaper", "Handcuffs", "Shawl", "Skates",
                 "Life Preserver", "Poncho", "Breathing Apparatus",
                 "Scuba Tank"),
        regex = c("(sun)?glass|monocle", "mask", "head(set|phone)", "goggle",
                  "balaclava", "headband", "visor", "tattoo", "helmet",
                  "hood|cowl", "crown|tiara", "belt|cummerbund", "cloak",
                  "pouch|bag\\b", "veil", "bows?\\b", "button", "earring",
                  "fan\\b", "jewel|gem|bead|br(o|a)ch", "headgear",
                  "hats?\\b|caps?\\b|fez", "headdress", "badge", "pocket",
                  "zipper", "\\b(neck)?tie\\b", "sash", "collar",
                  "necklace|pend(a|e)nt|locket", "boot", "buckle", "\\bshoe",
                  "scarf|neckerchief|bandann?a", "harness", "sandal",
                  "medallion", "suspender", "purse", "amulet", "ruffle|frill",
                  "stethoscope", "(back)?pack|knapsack", "medal", "ribbon",
                  "cape", "tassel", "glove|gauntlet", "watch", "sneaker",
                  "(sock|stocking)s?\\b", "epaulet", "diaper", "handcuff",
                  "shawl", "skate", "preserver", "poncho",
                  "breathing apparatus", "scuba tank")
      )
      
      # Associate each piece with its accessory(ies); a piece gets multiple
      # rows if it's associated with multiple accessories.
      accessory.parts.df <<- do.call(
        "bind_rows",
        apply(
          accessory.words.df, 1,
          function(x) {
            bind_rows(
              heads.df %>%
                filter(grepl(x[2], gsub("[^A-Za-z0-9 ]", "",
                                        tolower(part.name)))) %>%
                mutate(accessory = x[1]),
              hair.df %>%
                filter(grepl(x[2], gsub("[^A-Za-z0-9 ]", "",
                                        tolower(part.name)))) %>%
                mutate(accessory = x[1]),
              clothes.df %>%
                filter(grepl(x[2], gsub("[^A-Za-z0-9 ]", "",
                                        tolower(part.name)))) %>%
                mutate(accessory = x[1]),
              accessories.df %>%
                filter(grepl(x[2], gsub("[^A-Za-z0-9 ]", "",
                                        tolower(part.name)))) %>%
                mutate(accessory = x[1])
            )
          }
        )
      )
      
      # Create a table with all fashion items (hair, clothes, and accessories).
      fashion.items.df <<- lego.df %>%
        left_join(hair.style.df %>%
                    select(part.id, style) %>%
                    distinct() %>%
                    group_by(part.id) %>%
                    summarize(hair.styles = paste0(style,
                                                   collapse = ", ")),
                  by = c("part.id")) %>%
        left_join(clothes.type.df %>%
                    select(part.id, type) %>%
                    distinct() %>%
                    group_by(part.id) %>%
                    summarize(clothing.types = paste0(type,
                                                      collapse = ", ")),
                  by = c("part.id")) %>%
        left_join(accessory.parts.df %>%
                    select(part.id, accessory) %>%
                    distinct() %>%
                    group_by(part.id) %>%
                    summarize(accessories = paste0(accessory,
                                                   collapse = ", ")),
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
      
      # Associate each piece with its mood(s); a piece gets multiple rows if
      # it's associated with multiple moods.
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
      
      # Make mood an ordered factor so the graphs display in the correct,
      # consistent order.
      moods.df$mood = factor(moods.df$mood,
                             levels = c("Happy", "Sad", "Afraid", "Angry"))
      
      # If a piece has no other mood keywords, "frown" -> "sad".  (Otherwise,
      # "frown" is ambiguous; it could be "sad" or "angry".)
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
      moods.df <<- moods.df
      
      # Read in a dump of WordNet hypernyms.  We could get hypernyms using the
      # "wordnet" package, but we would have to look for hypernys one word at a
      # time; this is faster, because we want to get the whole graph and then
      # find paths from specific words to "plant" or "animal" (both to
      # determine which words actually refer to plants/animals, and to find
      # their place in the tree of life).
      hypernyms.df = bind_rows(
        read.csv(paste(local.path.to.tables, "direct_hypernyms.csv",
                       sep = ""),
                 header = T, stringsAsFactors = F),
        setNames(read.csv(paste(local.path.to.tables, "direct_hypernyms_2.csv",
                                sep = ""),
                          header = F, stringsAsFactors = F),
                 names(read.csv(paste(local.path.to.tables, "direct_hypernyms.csv",
                                      sep = ""),
                                header = T, nrows = 3))),
        setNames(read.csv(paste(local.path.to.tables, "direct_hypernyms_3.csv",
                                sep = ""),
                          header = F, stringsAsFactors = F),
                 names(read.csv(paste(local.path.to.tables, "direct_hypernyms.csv",
                                      sep = ""),
                                header = T,
                                nrows = 3)))
      )
      
      # Get a quick mapping from wordforms to lemmas, used for mapping
      # plant/animal keywords to lemmas in WordNet.
      data(hash_lemmas)
      
      # Get plant and animal pieces.
      ecology.df <<- lego.df %>%
        filter(part.category.name == "Plants and Animals")
      
      # Get all the words used in plant/animal pieces, and their frequencies.
      ecology.words.df = data.frame(word = gsub("[^A-Za-z ]", "",
                                                tolower(unlist(strsplit(sort(unique(ecology.df$part.name)),
                                                                        " "))))) %>%
        group_by(word) %>%
        summarize(freq = n()) %>%
        arrange(desc(freq)) %>%
        ungroup()
      
      # Create a directed graph whose edges go from words to their hyponyms.
      # Plants and animals only.
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
      
      # Remove certain subtypes of animals (e.g., "predatory animal") to reduce
      # the number of distinct ways a piece may be classified.  We want all the
      # words to be classified according to the same system.
      ecology.edges = ecology.edges %>%
        filter(!is.element(to, c("predatory animal", "domesticated animal",
                                 "predator", "young", "male", "female",
                                 "offspring", "domestic animal", "insectivore",
                                 "herb"))) %>%
        filter(to != "chestnut" | !grepl("equus|callus|horse", from))
      
      # Add edges for a few words that don't follow the desired classification
      # system (i.e., words that are classified only as something like "young
      # mammal").
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
      
      # For each word in the plant/animal pieces, if that word is a
      # plant/animal word, get the shortest path from that word to
      # "plant"/"animal" and add that path to the filtered graph of edges.
      # Skip words that usually do not refer to a plant or animal when they
      # appear in a part name.
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
                                                   to = current.lemma,
                                                   from = type)$vpath[[1]])
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
      ecology.edges.vis.df <<- ecology.edges.filtered %>%
        mutate(color = "black")
      
      # Create a data frame of nodes.  Add a regular expression to each node
      # that matches the name of the node and related wordforms, so that we can
      # later match nodes to part names that contain the relevant keywords.
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
      
      # For each part, determine which node that part is associated with (by
      # matching keywords in the part name to node names).  If a part matches
      # multiple nodes, choose the one furthest down in the tree.
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
      # plotting, because we might or might not need to facet by theme, and
      # that affects the size of each node.
      ecology.vertices.vis.df = ecology.vertices.vis.df %>%
        left_join(ecology.parts.nodes.df, by = c("id" = "node.name", "type"))
      ecology.vertices.vis.df <<- ecology.vertices.vis.df
      
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
      theme.counts.df <<- theme.counts.df
      
      # Write the crucial dataframes to Dropbox.
      for(df in data.frames) {
        df.name = paste(df, "df", sep = ".")
        file.name = paste(local.path.to.tables, gsub("\\.", "_", df), ".csv",
                          sep = "")
        write.csv(get(df.name), file.name, row.names = F)
        if(drop_exists(paste(dropbox.path.to.munged.tables, gsub("\\.", "_", df),
                             ".csv",
                             sep = ""))) {
          drop_delete(paste(dropbox.path.to.munged.tables, gsub("\\.", "_", df), ".csv",
                            sep = ""))
        }
        drop_upload(file.name, path = dropbox.path.to.munged.tables,
                    mode = "overwrite", autorename = F)
      }
      
    }
    
  }
  
  # If the data in Dropbox isn't too old, or if we didn't successfully download
  # the raw files from Rebrickable, get the crucial dataframes from Dropbox.
  # For each one, if we fail, use an old local copy instead.
  if(!dropbox.is.old | any.rebrickable.failures) {
    for(df in data.frames) {
      df.name = paste(gsub("\\.", "_", df), ".csv", sep = "")
      possible.error = tryCatch(
        assign(paste(df, "df", sep = "."),
               drop_read_csv(paste(dropbox.path.to.munged.tables,
                                   df.name, sep = ""),
                             stringsAsFactors = F),
               envir = .GlobalEnv),
        warning = function(e) { e },
        error = function(e) { e }
      )
      if(inherits(possible.error, "error")) {
        assign(paste(df, "df", sep = "."),
               read.csv(paste(local.path.to.tables, df.name,
                              sep = ""),
                        stringsAsFactors = F),
               envir = .GlobalEnv)
      }
    }
  }
  
  # Re-populate the theme picker inputs (because they depend on these
  # dataframes).
  theme.picker.inputs.to.update = list(
    list(id = "demographicsCircleThemePicker", col = "total.heads"),
    list(id = "demographicsSetThemePicker", col = "total.heads"),
    list(id = "hairTreemapThemePicker", col = "total.hair"),
    list(id = "clothesTreemapThemePicker", col = "total.clothes"),
    list(id = "accessoriesTreemapThemePicker", col = "total.accessories"),
    list(id = "fashionSetThemePicker", col = "total.fashion.items"),
    list(id = "moodsPolarThemePicker", col = "total.moods"),
    list(id = "moodsSetThemePicker", col = "total.heads"),
    list(id = "plantsDendrogramThemePicker", col = "total.plants"),
    list(id = "animalsDendrogramThemePicker", col = "total.animals"),
    list(id = "ecologySetThemePicker", col = "total.plants.animals")
  )
  for(picker in theme.picker.inputs.to.update) {
    temp.theme.counts.df = theme.counts.df
    temp.theme.counts.df[,"count.to.display"] = temp.theme.counts.df[,picker[["col"]]]
    updatePickerInput(
      session = session,
      inputId = picker[["id"]],
      choices = data.frame(
        temp.theme.counts.df %>%
          filter(count.to.display > 0) %>%
          mutate(label = paste(theme.name, " (", count.to.display, ")", sep = "")) %>%
          arrange(desc(count.to.display), theme.name)
      )$label
    )
  }
  
  # Re-populate the color picker inputs (because they depend on these
  # dataframes).
  color.picker.inputs.to.update = list(
    list(id = "demographicsSetEthnicityPicker", data = heads.df),
    list(id = "fashionSetColorPicker", data = fashion.items.df),
    list(id = "ecologySetColorPicker", data = ecology.df)
  )
  for(picker in color.picker.inputs.to.update) {
    updatePickerInput(
      session = session,
      inputId = picker[["id"]],
      choices = sort(unique(picker[["data"]]$color.name)),
      choicesOpt = list(content =
                          sapply(sort(unique(picker[["data"]]$color.name)),
                                 function(x) {
                                   background.color = paste("background-color: ",
                                                            unique(colors.df$color.hex[colors.df$name == x]),
                                                            ";",
                                                            sep = "")
                                   text.color = paste("color: ",
                                                      unique(colors.df$text.color.hex[colors.df$name == x]),
                                                      ";",
                                                      sep = "")
                                   num.rows = nrow(picker[["data"]][picker[["data"]]$color.name == x,])
                                   HTML(paste("<span style=\"padding: 2px; ",
                                              background.color, " ",
                                              text.color, "\">",
                                              x, " (", num.rows, ")",
                                              "</span>",
                                              sep = ""))
                                 }))
    )
  }
  
  # Re-populate other inputs that depend on these dataframes.
  updatePickerInput(
    session = session,
    inputId = "demographicsCircleGenderPicker",
    choices = sort(unique(heads.df$gender))
  )
  updatePickerInput(
    session = session,
    inputId = "demographicsSetGenderPicker",
    choices = sort(unique(heads.df$gender))
  )
  updatePickerInput(
    session = session,
    inputId = "clothesTreemapTypePicker",
    choices = data.frame(
      clothes.type.df %>%
        group_by(type) %>%
        summarize(total.parts = sum(num.parts)) %>%
        mutate(label = paste(type, " (", total.parts, ")", sep = "")) %>%
        arrange(desc(total.parts))
    )$label
  )
  updatePickerInput(
    session = session,
    inputId = "fashionSetHairStylePicker",
    choices = sort(unique(hair.style.df$style))
  )
  updatePickerInput(
    session = session,
    inputId = "fashionSetClothingTypePicker",
    choices = sort(unique(clothes.type.df$type))
  )
  updatePickerInput(
    session = session,
    inputId = "fashionSetAccessoryPicker",
    choices = sort(unique(accessory.parts.df$accessory))
  )
  updatePickerInput(
    session = session,
    inputId = "moodsPolarGenderPicker",
    choices = sort(unique(moods.df$gender))
  )
  updatePickerInput(
    session = session,
    inputId = "moodsSetGenderPicker",
    choices = sort(unique(moods.df$gender))
  )
  
})
