require(tidyverse)

options(dplyr.print_max = 1e9)

if(! exists("DF")) DF <- NULL

catn <- function(string = NULL, color = NULL) cat(paste0("\n", string))

reset_DF <- function() {
  DF <<- tibble(
                 ID = character(),
                 Concept = character(),
                 `Abscence de` = character(),
                 `Suspicion de` = character(),
                 `ATCD famillial` = character(),
                 `Bien segmenté` = character()
               )
}

confirm_answer <- function(answer, message = "#> Vous pouvez modifier") {
  catn(paste(message, answer))
  catn()
  new_answer <- readline("#> (Entrée pour conserver) ? ")
  if (new_answer == "") answer
  else confirm_answer(new_answer)
}

read_answer <- function(default_answer = NULL,
                        accepted_answers = NULL,
                        message = "Veuillez saisir quelque chose") {
  message_unproper_answer <- function () {
    cat("\n**** ---------------------------------------------- ****")
    cat("\n**** Réponse incorrecte * Merci de saisir à nouveau ****")
    cat("\n**** ---------------------------------------------- ****\n")
  }
  read_answer_with_no_accepted_answers <- function() {
    answer <- readline("#> ")
    if (answer == "") {
      message_unproper_answer()
      read_answer(NULL, NULL, "Ce champ ne peut être vide...")
    }
    else confirm_answer(answer)
  }
  read_answer_with_accepted_answers <- function(default_answer,
                                                accepted_answers) {
    format_accepted_answers <- function(accepted_answers) {
      paste0('|', paste0(accepted_answers, "|", collapse = "") )
    }
    if (is.null(default_answer)) default_answer <- accepted_answers[[1]]
    prompt <- paste('Choix : ', format_accepted_answers(accepted_answers),
                    " (Simplement <ENTRÉE> pour ",
                    default_answer, ") ? ")
    answer <- readline(prompt = prompt)
    if(answer == "") default_answer
    else if ((answer %in% c(accepted_answers))) answer
    else {
      message_unproper_answer()
      read_answer(default_answer, accepted_answers)
    }
  }
  # -------------- Main part of funtion
  catn(message)
  catn()
  if(is.null(accepted_answers))
    read_answer_with_no_accepted_answers()
  else {
    read_answer_with_accepted_answers(default_answer, accepted_answers)
  }
}
# read_answer(NULL, NULL)

read_classification_correctness <- function(property, concept) {
  accepted_answers <- c("vn", "fn", "vp", "fp")
  catn(paste0('> Pour la propriété         -> ', property))
  catn(paste0('> Classification du concept -> ', concept))
  catn()
  read_answer(NULL, accepted_answers)
}
# read_classification_correctness("AVC", "Suspicion de")

read_proper_segmentation <- function(concept) {
  accepted_answers <- c("o", "n")
  cat(paste0('\n> Concept bien segmenté -> ', concept, '\n'))
  read_answer(NULL, accepted_answers)
}
# read_proper_segmentation("AVC")

info_for_concept <- function(concept) {
  properties <- c("Abscence de", "Suspicion de", "ATCD famillial de")
  l <- map(properties, ~ read_classification_correctness(., concept))
  names(l) <- properties
  segmentation <- read_proper_segmentation(concept)
  names(segmentation) <- "Bien segmenté"
  c(l, segmentation)
}
# info_for_concept("AVC")

entry_new_concept <- function(doc_id) {
  make_tibble_from_info_concept <- function(concpet, info) {
      tibble(
        ID = doc_id,
        Concept = concept,
        `Abscence de`    = info$`Abscence de`   ,
        `Suspicion de`   = info$`Suspicion de`  ,
        `ATCD famillial` = info$`ATCD famillial`,
        `Bien segmenté`  = info$`Bien segmenté`
     )
    }
  concept <- read_answer(NULL, NULL, paste("Création d'un concept pour le document", doc_id))
  info <- info_for_concept(concept)
  tb <- make_tibble_from_info_concept(concept, info)
  DF <<- rbind(DF, tb)
}
# entry_new_concept("123456")

ask_for_more <- function(message) {
  read_answer("o", c("o", "n"), message)
}
# ask_for_more("Y a-t-il encore des concepts à saisir pour ce document ?")

input_info_for_document <- function(doc_id) {
  catn(paste0("> Saisie des informations pour le document ", doc_id))
  catn()
  info_doc <- list()
  repeat{
    entry_new_concept(doc_id)
    if( ask_for_more("Y a-t-il encore des concepts à saisir pour ce document ?") == "n" )
      break
  }
}
# input_info_for_document("123456")

entry_new_document <- function() {
  if(! is.null(DF)) {
    last_doc_id <- DF$ID[[nrow(DF)]]
    doc_id <- confirm_answer(last_doc_id, "Saisissez l'ID d'un document ou continuer sur le document")
  }
  else doc_id <- read_answer(NULL, NULL, "Saisissez l'ID du document")
  info_doc <- input_info_for_document(doc_id)
}
# entry_new_document()

saisir_document <- function() {
  doc_tibble <- entry_new_document()
  DF <<- rbind(DF, doc_tibble)
}

ecrire_fichier <- function(filename = NULL) {
  print(list.files())
  if (is.null(filename)) {
    filename <- read_answer(NULL, NULL, "Veuillez choisir un nom de fichier")
  }
  saveRDS(DF, filename)
}

lire_fichier <- function(filename = NULL) {
  print(list.files())
  if (is.null(filename)) {
    filename <- read_answer(NULL, NULL, "Veuillez choisir un nom de fichier")
  }
  DF <<- readRDS(filename)
}
 
wait_for_enter <- function() {
  catn("<ENTRÉE> pour continuer...")
  catn()
  readline()
}

montrer_df <- function() {
  show_df()
  wait_for_enter()
}

export_to_csv <- function() {
  filename <- read_answer(NULL, NULL, "Veuillez choisir un nom de fichier")
  write.csv2(DF, filename)
}

modifier_df <- function() {
  DF <<- edit(DF)
}

show_df <- function() {
  if( ! is.null(DF)) {
    catn("Ab Su AF Sg  Id  Concept")
    catn()
    col_reordered_DF <- DF %>% select(c(3, 4, 5, 6, 1, 2))
    print_row <- function(x) {cat(x); catn()}
    apply(col_reordered_DF, 1, print_row)
  }
}

show_choices <- function() {
  catn("--------------------------------")
  catn("|  1. Saisir un document       |")
  catn("|  2. Montrer le data frame    |")
  catn("|  3. Éditer le data frame     |")
  catn("|  4. Exporter au format .csv  |")
  catn("|  0. Quitter                  |")
  catn("--------------------------------")
  catn()
}

run <- function() {
  choice <- -1
  while(choice != 0) {
    montrer_df()
    show_choices()
    choice <- read_answer(1, 0:6, "Que souhaitez-vous faire ?")
    choice <- as.integer(choice)
    switch(choice,
    { saisir_document()},
    {  },
    { modifier_df() },
    { export_to_csv() })
  }
}
