
### --- project
### --- Language is Dead
### --- Similarity structures

### --- script
### --- produce_languageisdead_Rmd.R

### --- sources

data_dir <- "_data/"
img_dir <- "_img/"
img_dir_print <- paste0(img_dir, "_print/")
img_dir_digital <- paste0(img_dir, "_digital/")

### --- lib
library(magrittr)

### --- header

header <- "---
title: Language is dead
author:
- name: \\@GSMilovanovic
date: \"`r format(Sys.time(), '%d %B %Y')`\"
abstract: 
output:
  html_document:
    theme: cosmo
    toc: no
    includes:
      in_header: header.html
---

"

### --- intro

intro <- 
  paste0("![](_img/twitter_header_1500_500_px.png)",
         "<br><br>",
         '<a href="https://opensea.io/collection/q1universeismine" 
          title="Buy on OpenSea" target="_blank">
          <img style="width:220px; border-radius:5px; box-shadow: 0px 1px 6px 
          rgba(0, 0, 0, 0.25);" 
          src="https://storage.googleapis.com/opensea-static/Logomark/Badge%20-%20Available%20On%20-%20BW.png" 
         alt="Available on OpenSea" /></a>',
         "<br><br>",
         "[![](_img/github.png)]",
         "(https://github.com/GoranMilovanovic/q1universeismine)  ",
         "[![](_img/twitter.png)]",
         "(https://twitter.com/q1universe)  ",
         "[![](_img/instagram.png)]",
         "(https://www.instagram.com/q1universeismine)  ",
         "<hr>",
         "<h3>Networks of languages: the dying ones go missing</h3>", 
         "55 similarity networks of human languages were generated from ",
         "[Wikidata](https://www.wikidata.org/wiki/Wikidata:Main_Page), ",
         "the largest open, machine-readable knowledge base on the Planet. ",
         "Each network is missing one language which is marked as endangared ",
         "by [UNESCO](http://www.unesco.org/languages-atlas/). **The missing language serves to 
         create the title of the respective network.** ",
         "55 pieces will be dropped in October 2021. ",
         "They will be dropped into [OpenSea](https://opensea.io/)",
         " via [Polygon](https://polygon.technology/). 
         Follow [\\@languageisdead](https://twitter.com/q1universe) ",
         " on Twitter for drop announcements. ",
         "**50% of proceeds will be donated to Wikidata, to be used in projects 
         aimed to support the representation of endangered languages and small linguistic communities.**"
  )

### --- art

# - art: load art_frame
art_frame <- read.csv(
  paste0(getwd(), "/_data/art_frame.csv"),
  header = TRUE,
  check.names = FALSE,
  row.names = 1,
  stringsAsFactors = FALSE
  )
# - pricing
art_frame$price_eth <- 1

# - art:components
lF <- list.files("_img/_digital/")
components <- lapply(art_frame$piece, function(x) {
  # - locate text
  w <- which(art_frame$piece == x)
  # - locate QR code
  wfile <- which(lF == art_frame$filename[w])
  # - identifier
  id_q <- paste0("{#lid", w, "}")
  # - produce
  text <- paste0("<hr>",
                 "<h3>", 
                 "<a name=\"", 
                 id_q, 
                 "\"></a>",
                 art_frame$piece[w],
                 "</h3>",
                 "[", paste0("https://www.languageisdead.net#",
                             "lid", w, "]", "(", 
                             "https://www.languageisdead.net#",
                             "lid", w, ")"),
                 "<br>",
                 art_frame$timestamp[w], " UTC",
                 "<br>",
                 "<h4>",
                 "**&Xi; ", art_frame$price_eth[w],
                 "**</h4>",
                 "<br>",
                 "[![](",
                 paste0("_img/_digital/", lF[wfile]),
                 ")]",
                 "{ width=45% }")
  text <- gsub("  ", " ", text, fixed = TRUE)
  # - output
  return(text)
})
components <- paste(components, 
                    sep = "<br>", 
                    collapse = "")

### --- footer 

footer <- paste0(
  "<br>",
  "<hr>",
  '<a href="https://opensea.io/collection/q1universeismine" 
          title="Buy on OpenSea" target="_blank">
          <img style="width:220px; border-radius:5px; box-shadow: 0px 1px 6px 
          rgba(0, 0, 0, 0.25);" 
          src="https://storage.googleapis.com/opensea-static/Logomark/Badge%20-%20Available%20On%20-%20BW.png" 
         alt="Available on OpenSea" /></a>',
  "<br><br>",
  "[![](_img/github.png)]", 
  "(https://github.com/GoranMilovanovic/q1universeismine)  ",
  "[![](_img/twitter.png)]",
  "(https://twitter.com/q1universe)  ",
  "[![](_img/instagram.png)]",
  "(https://www.instagram.com/q1universeismine)  ",
  "<br><br>",
  "**Language is Dead was rendered on 2021/10/15.**",
  "<br><br><br>"
  )

### --- compose

cmp_rmd <- paste0(
  header,
  intro,
  components, 
  footer
)
writeLines(cmp_rmd, 
           "index.Rmd")

### --- render
rmarkdown::render("index.Rmd")

### --- store art_frame
art_frame$url <- paste0("https://www.languageisdead.net#lid", 
                        1:dim(art_frame)[1])
write.csv(art_frame, 
          paste0(data_dir, "art_frame.csv"))
