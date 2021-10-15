
### --- project
### --- Language is Dead
### --- Similarity structures

### --- script
### --- languageisdead.R

### --- references
# - The UNESCO language status data:
# - Moseley, Christopher (ed.). 2010. 
# - Atlas of the World’s Languages in Danger, 3rd edn. 
# - Paris, UNESCO Publishing. 
# - Online version: http://www.unesco.org/culture/en/endangeredlanguages/atlas

### --- license

usethis::use_cc0_license()

### --- sources

data_dir <- "_data/"
img_dir <- "_img/"
img_dir_print <- paste0(img_dir, "_print/")
img_dir_digital <- paste0(img_dir, "_digital/")
path_wmfanalytics <- "https://analytics.wikimedia.org/published/datasets/"
path_wmdeanalytics <- "wmde-analytics-engineering/Wikidata/"
path_languages <- "WD_Languages_Landscape/WD_Languages_Jaccard_Similarity.csv"
file_jaccard <- paste0(path_wmfanalytics, 
                       path_wmdeanalytics,
                       path_languages)
file_unesco <- "unesco_atlas_languages_limited_dataset.ods"

### --- datasets

data_jaccard <- read.csv(URLencode(file_jaccard), 
                         header = T, 
                         row.names = 1, 
                         check.names = FALSE,
                         stringsAsFactors = FALSE)
write.csv(data_jaccard, 
          paste0(data_dir, "data_jaccard.csv"))

data_unesco <- readODS::read.ods(
  paste0(data_dir, file_unesco)
  )
data_unesco <- data_unesco[[1]]
colnames(data_unesco) <- data_unesco[1, ]
data_unesco <- data_unesco[-1, ]

### --- prepare datasets

# - keep extinct/endangered languages only
data_unesco <- dplyr::filter(data_unesco, 
                             grepl("endangered|Extinct", 
                                   data_unesco$`Degree of endangerment`)
                             )

# - keep only languages w. ISO639-3
data_unesco <- dplyr::filter(data_unesco,
                             `ISO639-3 codes` != "")

# - how many structures?
found_critical <- 
  which(data_unesco$`ISO639-3 codes` %in% data_jaccard$language)
n_found_critical <- length(found_critical) 
print(n_found_critical)
print(data_unesco$`Name in English`[found_critical])

# - languages dataset
data_languages <- data_unesco[found_critical, ]
rm(data_unesco)
data_languages <- dplyr::select(data_languages,
                                `ISO639-3 codes`, 
                                `Name in English`, 
                                `Degree of endangerment`)
colnames(data_languages) <- c("iso", 
                              "language", 
                              "status")
data_languages$status <- ifelse(grepl("endangered", data_languages$status), 
                                "endagered", 
                                "extinct")
data_languages <- data_languages[!duplicated(data_languages$iso), ]
data_languages$language <- gsub(" \\(([[:alpha:]]|\\s)+\\)",
                                "", 
                                data_languages$language)
# - how many extinct languages?
table(data_languages$status)
# - only one; decision: keep only endangered for consistency
data_languages <- dplyr::filter(data_languages,
                                status != "extinct")
# - store
write.csv(data_languages, 
          paste0(data_dir, "data_languages.csv"))

# - jaccard dataset: remove unused
data_jaccard <- dplyr::select(data_jaccard, 
                              -reuse,
                              -item_count,
                              -num_items_reused)

### --- artwork
art_frame <- vector(mode = "list",
                    length = dim(data_languages)[1])
for (i in 1:dim(data_languages)[1]) {
  
  # - language
  lan <- data_languages$iso[i]
  lan_name <- data_languages$language[i]
  
  # - Jaccard similarity matrix w/p language
  w_lan <- which(colnames(data_jaccard) == lan)
  sim_mat <- data_jaccard[-w_lan, -w_lan]
  
  # - parameters
  choice <- sample(1:5)[1]
  crv <- runif(1, 0, 2.75)

  # - Graph data.frame: 3 NNs
  g <- lapply(sim_mat$language, function(x) {
    wl <- which(sim_mat$language == x)
    d <- sim_mat[wl, ]
    d$language <- NULL
    d <- d[, -wl]
    y <- sort(d, decreasing = TRUE)[1:choice]
    return(
      data.frame(outgoing = x,
                 incoming = names(y),
                 stringsAsFactors = FALSE)
      )
    })
  g <- data.table::rbindlist(g)
  
  # - filename
  name <- paste0(lan_name,
                 " (", 
                 lan,
                 ") is dead")
  filename <- paste0(lan_name,
                     " (", 
                     lan,
                     ")",
                     ".png")
  filename <- gsub(" ", "_", filename)
  pdf_filename <- paste0(lan_name,
                         " (", 
                         lan,
                         ")",
                         ".pdf")
  pdf_filename <- gsub(" ", "_", pdf_filename)
  
  
  # - igraph
  language_net <- igraph::graph.data.frame(g, directed = T)
  lyg <- igraph::layout_with_fr(language_net)

  # - art_frame
  art_frame[[i]] <- data.frame(piece = name,
                               filename = filename,
                               nns = choice, 
                               curvature = crv, 
                               timestamp = as.character(Sys.time()))
  
  # - plot w. {igraph} --- digital
  Cairo::CairoPNG(filename = paste0(img_dir_digital, filename),
                  width = 2000,
                  height = 2000,
                  units = "px",
                  dpi = 72, 
                  bg = "whitesmoke")
  
  # - random color
  plot(language_net,
       layout = lyg,
       edge.width = .75,
       edge.color = "black",
       edge.arrow.size = 0,
       edge.curved = crv,
       vertex.size = 1,
       vertex.color = "black",
       vertex.frame.color = "black",
       vertex.label.color = "black",
       vertex.label.font = 1,
       vertex.label.cex = 1.25,
       vertex.label.dist = .25,
       margin = c(rep(.125, 4)))
  title(tolower(name),
        cex.main = 2.75,
        col.main = "black", 
        line = -2)
  
  dev.off()
  
  # - plot w. {igraph} --- PRINT
  Cairo::CairoPDF(file = paste0(img_dir_print, pdf_filename),
                  width = 23.6220472,
                  height = 23.6220472)
  
  # - random color
  plot(language_net,
       layout = lyg,
       edge.width = .75,
       edge.color = "black",
       edge.arrow.size = 0,
       edge.curved = crv,
       vertex.size = 1,
       vertex.color = "black",
       vertex.frame.color = "black",
       vertex.label.color = "black",
       vertex.label.font = 1,
       vertex.label.cex = 1.25,
       vertex.label.dist = .25,
       margin = c(rep(.125, 4)))
  title(tolower(name),
        cex.main = 2.75,
        col.main = "black", 
        line = -2)
  
  dev.off()
  
  # - report
  print(paste0("Finished generating: ",
               name, " - ",
               i, ". out of ",
               dim(data_languages)[1], "."))
  
}

# - store art_frame
art_frame <- data.table::rbindlist(art_frame)
art_frame <- dplyr::arrange(art_frame, 
                            piece)
write.csv(art_frame, 
          paste0(data_dir, "art_frame.csv"))

