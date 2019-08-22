rm(list = ls())
require(purrr)

tb <- tibble(
  ID = character(),
  Concept = character(),
  `Abscence de` = character(),
  `Suspicion de` = character(),
  `ATCD famillial` = character(),
  `Bien segmenté` = character()
)

read_answer <- function(default_answer = NULL,
                        accepted_answers = NULL,
                        message = "Veuillez saisir quelque chose") {
  confirm_answer <- function(answer) {
    cat("\n#> Vous pouvez modifier", answer, "\n")
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
    if (is.null(default_answer)) default_answer <- accepted_answers[[1]]
    prompt <- paste(format_accepted_answers(accepted_answers),
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
  format_accepted_answers <- function(accepted_answers) {
    paste0('|', paste0(accepted_answers, "|", collapse = "") )
  }
  # -------------- Main part of funtion
  cat("\n#>", message, "\n")
  if(is.null(accepted_answers))
    read_answer_with_no_accepted_answers()
  else {
    read_answer_with_accepted_answers(default_answer, accepted_answers)
  }
}
# read_answer(NULL, NULL)

read_classification_correctness <- function(property, concept) {
  accepted_answers <- c("TN", "FN", "TP", "FP")
  cat(paste0('\n> Pour la propriété         -> ', property))
  cat(paste0('\n> Classification du concept -> ', concept), '\n')
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
  print(paste0("Saisie des informations pour le document ", doc_id))
  info_doc <- list()
  while(ask_for_more("Y a-t-il encore des concepts à saisir pour ce document ?") == "O") {
    info_concept <- create_new_concept_and_collect_info(doc_id)
    info_doc <- c(info_doc, info_concept)
  }
  info_doc
}
# input_info_for_document("123456")

input_info_sample <- function() {
  print("Saisie des informations pour l'échantillon : ")
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

# saveRDS(test, 'test.rds')

# info_sample <- readRDS('test.rds')

make_tibble_from_info <- function(info_sample) {
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

ecrire_fichier <- function(filename) {
  saveRDS(tb, filename)
}

lire_fichier <- function(filename) {
  readRDS(filemane)
}


 
