rm(list = ls())
require(tidyverse)

options(dplyr.print_max = 1e9)

SAISIE <- NULL
DF <- NULL

catn <- function(string = NULL) cat(paste0("\n", string))

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

read_answer <- function(default_answer = NULL,
                        accepted_answers = NULL,
                        message = "Veuillez saisir quelque chose") {
  confirm_answer <- function(answer) {
    catn(paste("#> Vous pouvez modifier", answer))
    catn()
    new_answer <- readline("#> (Entrée pour conserver) ? ")
    if (new_answer == "") answer
    else confirm_answer(new_answer)
  }
  read_answer_with_no_accepted_answers <- function() {
    answer <- readline("#> ")
    confirm_answer(answer)
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
      cat("\n**** ---------------------------------------------- ****")
      cat("\n**** Réponse incorrecte * Merci de saisir à nouveau ****")
      cat("\n**** ---------------------------------------------- ****\n")
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
  accepted_answers <- c("VN", "FN", "VP", "FP")
  catn(paste0('> Pour la propriété         -> ', property))
  catn(paste0('> Classification du concept -> ', concept))
  catn()
  read_answer(NULL, accepted_answers)
}
# read_classification_correctness("AVC", "Suspicion de")

read_proper_segmentation <- function(concept) {
  accepted_answers <- c("O", "N")
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

create_new_concept_and_collect_info <- function(doc_id) {
  concept <- read_answer(NULL, NULL, paste("Création d'un concept pour le document", doc_id))
  info <- list(info_for_concept(concept))
  names(info) <- concept
  info
}
# create_new_concept_and_collect_info("123456")

ask_for_more <- function(message) {
  read_answer("O", c("O", "N"), message)
}
# ask_for_more("Y a-t-il encore des concepts à saisir pour ce document ?")

input_info_for_document <- function(doc_id) {
  catn(paste0("> Saisie des informations pour le document ", doc_id))
  catn()
  info_doc <- list()
  while(ask_for_more("Y a-t-il encore des concepts à saisir pour ce document ?") == "O") {
    info_concept <- create_new_concept_and_collect_info(doc_id)
    info_doc <- c(info_doc, info_concept)
  }
  info_doc
}
# input_info_for_document("123456")

input_info_sample <- function() {
  catn("Saisie des informations pour l'échantillon : ")
  catn()
  info_sample <- list()
  while(ask_for_more("Y a-t-il encore des documents à saisir pour cet échantillon ?") == "O") {
    doc_id <- read_answer(NULL, NULL, "Veuillez saisir l'ID du document")
    info_doc <- list(input_info_for_document(doc_id))
    names(info_doc) <- doc_id
    info_sample <- c(info_sample, info_doc)
  }
  info_sample
}
# test <- input_info_sample()

make_tibble_from_info <- function(info_sample) {
  is <- info_sample
  all_lines <- map(names(is), function(id) {
    doc_lines <- map(names(is[[id]]), function(concept) {
      col_values <- map(names(is[[id]][[concept]]), function(col) {
        is[[id]][[concept]][[col]]
      })
      tibble(
        ID = id,
        Concept = concept,
        `Abscence de` = col_values[[1]],
        `Suspicion de` = col_values[[2]],
        `ATCD famillial` = col_values[[3]],
        `Bien segmenté` =  col_values[[4]])
    })
    do.call(rbind, doc_lines)
  })
  do.call(rbind, all_lines)
}

ecrire_fichier <- function(filename = NULL) {
  if (is.null(filename)) {
    filename <- read_answer(NULL, NULL, "Veuillez choisir un nom de fichier")
  }
  saveRDS(DF, filename)
}

lire_fichier <- function(filename = NULL) {
  if (is.null(filename)) {
    filename <- read_answer(NULL, NULL, "Veuillez choisir un nom de fichier")
  }
  DF <<- readRDS(filename)
}
 
saisir_donnees <- function() {
  info <- input_info_sample()
  SAISIE <<- info
}

ajouter_saisie_au_data_frame <- function() {
  DF <<- rbind(DF, make_tibble_from_info(SAISIE))
  SAISIE <<- NULL
}

wait_for_enter <- function() {
  catn("<ENTRÉE> pour continuer...")
  catn()
  readline()
}

montrer_saisie <- function() {
  print(make_tibble_from_info(SAISIE))
  wait_for_enter()
}

montrer_df <- function() {
  print(DF)
  wait_for_enter()
}

reset_saisie <- function() {
  SAISIE <<- NULL
}

export_to_csv <- function() {
  filename <- read_answer(NULL, NULL, "Veuillez choisir un nom de fichier")
  write.csv2(DF, filename)
}

modifier_df <- function() {
  DF <<- edit(DF)
}

show_choices <- function() {
  catn("---------------------------------------------")
  catn("|  1. Montrer le data frame                  |")
  catn("|  2. Continuer la saisie                    |")
  catn("|  3. Sauver le data frame dans un fichier   |")
  catn("|  4. Lire le data frame depuis un fichier   |")
  catn("|  5. Reset du data frame                    |")
  catn("|  6. Exporter au format .csv                |")
  catn("|  7. Éditer le data frame                   |")
  catn("|  0. Quitter                                |")
  catn("---------------------------------------------")
  catn()
}

run <- function() {
  choice <- -1
  while(choice != 0) {
    show_choices()
    choice <- read_answer(1, 0:7, "Que souhaitez-vous faire ?")
    choice <- as.integer(choice)
    switch(choice,
    {montrer_df()},
#    {montrer_saisie()},
    {saisir_donnees();
      ajouter_saisie_au_data_frame()},
 #   {ajouter_saisie_au_data_frame()},
    {ecrire_fichier()},
    {lire_fichier()},
#    {reset_saisie()},
    {reset_DF()},
    {export_to_csv()},
    {modifier_df()})
  }
}
