library(rollama)
library(crayon)
library(dplyr)
library(stringr)
library(R6)
library(yaml)

AutoGM <- R6Class("AutoGM",
  public = list(

    gm = NULL,
    gamebook = list(),
    protocol = NULL, # contains chat and internal commands and markers like session starts
    options = list(allow_simple_commands = TRUE,  # allow to write "command" instead of "[COMMAND]" etc.
                   allow_direct_commands = FALSE, # turn everything that doesn't follow special commands into general command
                   show_whole_protocol = FALSE),  # show everything from protocol (except system prompt)

    # Init AutoGM
    initialize = function(gamebook, savegame, autogm = "v1", options = list()) {

      if (!missing(gamebook)) {

        if (!missing(savegame)) {
          stop("You cannot load a savegame and a gamebook at the same time!")
        }

        # Load gm and gamebook
        private$.load_autogm(autogm)
        private$.load_gamebook(gamebook)

        # Init protocol with AutoGM system prompt
        system <- private$.get_system()
        self$protocol <- tibble(role = "system", content = system)

        private$.print_note("New game initiated")

      } else if (!missing(savegame)) {

        # Restore previously saved game
        if (!str_detect(savegame, "\\.sav$")) savegame <- paste0(savegame, ".sav")
        filepath <- file.path("savegames", savegame)
        savegame <- readRDS(filepath)
        self$gm       <- savegame$gm
        self$gamebook <- savegame$gamebook
        self$protocol <- savegame$protocol
        self$options  <- savegame$options

        private$.print_note("Game successfully restored.")

      }

      # update options
      self$options <- modifyList(self$options, options)

      # start session
      self$protocol <- add_row(self$protocol, role = "autogm", content = "session_start")

    },

    # Start a message loop
    play = function(intro = TRUE) {

      cat("\014") # Clear screen
      private$.print()

      if (intro) {
        input <- "[COMMAND] Shortly describe the group's current situation!"
        private$.input(input)
        private$.print()
      }

      while (TRUE) { # The game loop

        # get user input
        input <- readline(">>> ")

        tryCatch({

          # process top commands x, save, redo, help
          if (str_to_lower(input) == "x") {
            self$protocol <- add_row(self$protocol, role = "autogm", content = "session_end")
            return(invisible(self$protocol))
          }
          if (str_to_lower(str_sub(input, 1, 5)) == "save ") {
            file <- str_sub(input, 6)
            self$save(file)
            next
          }
          if (str_to_lower(input) == "redo" || str_to_lower(str_sub(input, 1, 5)) == "redo ") {
            private$.redo(input)
            next
          }
          if (str_to_lower(input) == "help") { # TODO: make this an outside message using system only
            full_input <- "[COMMAND] Describe all commands that players can use and how they should write them. Keep it short and create your answer as a list. Explain the commands' meanings but don't write anything about how you are supposed to react to them."
            temp_chat <- add_row(self$protocol[1,], role = "user", content = full_input)
            answer <- query(temp_chat, screen = FALSE) # we only need system prompt for this
            self$protocol <- self$protocol %>%
              add_row(role = "special", content = "help") %>%
              add_row(role = "special_answer", content = answer$message$content)
            private$.print()
            next
          }

          # receive and print answer
          private$.input(input)
          private$.print()

        }, error = function(e) private$.print_error(e$message),
           warning = function(w) {})

      }

    },

    # Check whether AutoGM is ready or not
    ready = function() {
      is_ready <- TRUE
      if (!ping_ollama(silent = TRUE)) {
        cat(red("> Cannot reach Ollama - not running?\n"))
        is_ready <- FALSE
      }
      if (!all(c("setting") %in% names(self$gamebook))) {
        cat(red("> Gamebook is incomplete!\n"))
        is_ready <- FALSE
      }
      is_ready
    },

    save = function(file) {
      savegame <- list(gm = self$gm, gamebook = self$gamebook, protocol = self$protocol, options = self$options)
      if (!str_detect(file, "\\.sav$")) file <- paste0(file, ".sav")
      filepath <- file.path("savegames", file)
      saveRDS(savegame, filepath)
      private$.print_note(paste("Game saved in", filepath))
    }

  ),

  private = list(

    # Load AutoGM version files
    .load_autogm = function(autogm) {

      # Load AutoGM version setup
      self$gm <- read_yaml(file.path("versions", autogm, "setup.yaml"))
      self$gm[["_path"]] <- autogm
      self$gm[["_template"]] <- readLines(file.path("versions", autogm, "system.tmpl")) %>% paste(collapse = "\n")

      # Find all gamebook files (.gbk) in versions folder
      path <- file.path("versions", autogm)
      files <- list.files(path = path, pattern = "\\.gbk", recursive = TRUE)

      # Load gamebook files
      for (file in files) {
        data <- private$.read(file.path(path, file))
        key <- str_extract(file, "^[^\\.]*")
        self$gm$gbk[[key]] <- data
      }

    },

    # Load gamebook files
    .load_gamebook = function(gamebook) {

      # Find all gamebook files (.gbk) in gamebook folder
      path <- file.path("gamebooks", gamebook)
      pattern_file <- "([a-zA-Z][a-zA-Z0-9]*)(?:_([a-zA-Z][a-zA-Z0-9]*))?\\.gbk$"
      pattern_filepath <- paste0("^(?:[a-zA-Z][a-zA-Z0-9/]*/)?", pattern_file)
      files <- list.files(path = path, pattern = pattern_file, recursive = TRUE) %>%
        str_match(pattern_filepath)

      # Load gamebook content
      for (i in seq_len(nrow(files))) {
        data <- private$.read(file.path(path, files[i, 1]))
        if (is.na(files[i, 3])) {
          self$gamebook[[files[i, 2]]] <- data
        } else {
          self$gamebook[[files[i, 2]]][[files[i, 3]]] <- data
        }
      }

    },

    # Get system prompt
    .get_system = function() {

      # find placeholders in template
      template <- self$gm[["_template"]]
      placeholders <- template %>%
        str_match_all("\\{([^\\{\\}\\|]+)(?:\\|([^\\{\\}]+))?\\}") %>% .[[1]]
      placeholders[, 3] <- ifelse(is.na(placeholders[, 3]), "", placeholders[, 3])

      # replace placeholders
      for (i in seq_len(nrow(placeholders))) {
        item <- placeholders[i, 2]
        if (item %in% names(self$gamebook)) {
          item <- self$gamebook[[item]] %>% paste(collapse = "\n\n")
        } else {
          item <- self$gm$gbk[[item]] %>% paste(collapse = "\n\n")
        }
        template <- template %>% str_replace(fixed(placeholders[i, 1]), item)
      }

      template

    },

    # get clean chat from protocol
    .get_chat = function() {

      # melt latest addendum if available into user request
      protocol <- self$protocol
      if (tail(protocol$role, 1) == "addendum") {
        protocol$content[nrow(protocol) - 1] <- sprintf("%s\n\n(Note: %s)",
                                                        protocol$content[nrow(protocol) - 1],
                                                        protocol$content[nrow(protocol)])
        protocol <- protocol[-nrow(protocol), ]
      }

      # filter for chat-relevant entries only
      protocol %>% filter(role %in% c("system", "user", "assistant"))

    },

    # send a message to llm
    .send = function() {
      new_chat <- private$.get_chat()
      answer <- query(new_chat, screen = FALSE)
      self$protocol <- bind_rows(self$protocol, answer$message)
    },

    # send normal command
    .input = function(input, remember = TRUE) {

      # if simple commands allowed convert first
      if (self$options$allow_simple_commands) {
        for (command in names(self$gm$commands)) {
          pattern <- paste0("^", command, " ")
          if (str_detect(input, pattern)) {
            input <- str_replace(input, pattern, paste0(self$gm$commands[[command]], " "))
          }
        }
      }

      # check validity
      valid <- FALSE
      pattern_character <- paste0("^(", paste(names(self$gamebook$pc), collapse = "|"), "):? ") # accept lowercase
      if (str_detect(input, regex(pattern_character, ignore_case = TRUE))) valid <- TRUE
      pattern_commands <- paste0("^(", paste(private$quotemeta(self$gm$commands), collapse = "|"), ") ")
      if (str_detect(input, pattern_commands)) valid <- TRUE

      if (!valid) {
        if (self$options$allow_direct_commands) {
          input <- paste(self$gm$commands$command, input)
        } else {
          stop("Invalid input!")
        }
      }

      self$protocol <- add_row(self$protocol, role = "user", content = input)
      private$.send()

    },

    # redo one prompt with or without addendum
    .redo = function(input) {

      if (nrow(self$protocol) < 2 ||
           (!(all(tail(self$protocol$role, 2) == c("user", "assistant")) ||
              all(tail(self$protocol$role, 2) == c("addendum", "assistant"))))) {
        stop("You cannot do a redo at this moment!")
      }

      before_addendum <- tail(self$protocol$role, 2)[1] == "addendum"
      addendum <- str_match(input, "^redo ?(.*)$")[1, 2]

      if (before_addendum) {
        self$protocol$role <- c(head(self$protocol$role, -3), c("hidden_user", "addendum", "hidden_assistant"))
        if (addendum != "") {
          addendum <- paste0(tail(self$protocol$content, 2)[1], "\n", addendum)
        } else {
          addendum <- tail(self$protocol$content, 2)[1]
        }
        self$protocol <- add_row(self$protocol, role = "user", content = tail(self$protocol$content, 3)[1])
      } else {
        self$protocol$role <- c(head(self$protocol$role, -2), c("hidden_user", "hidden_assistant"))
        self$protocol <- add_row(self$protocol, role = "user", content = tail(self$protocol$content, 2)[1])
      }
      if (addendum != "") {
        self$protocol <- add_row(self$protocol, role = "addendum", content = addendum)
      }

      private$.send()
      private$.print()

    },

    # Read gamebook files from local storage
    .read = function(path) {
      readLines(path) %>% paste(collapse = "\n")
    },

    # Print the chat history
    .print = function() {
      if (self$options$show_whole_protocol) {
        chat <- self$protocol
      } else {
        chat <- private$.get_chat()
      }
      cat("\014") # Clear screen
      for (i in seq_len(nrow(chat))) {
        if (chat$role[i] == "user") {
          cat(blue(chat$content[i]), "\n\n")
        } else if (chat$role[i] == "assistant") {
          cat("AutoGM:", chat$content[i], "\n\n")
        } else if (chat$role[i] != "system") {
          cat(white(chat$content[i]), "\n\n")
        }
      }
    },

    # Print a note from the GM system
    .print_note = function(note) {
      cat(green(sprintf("%s\n\n", note)))
    },

    # Print an error from the GM system
    .print_error = function(note) {
      cat(red(sprintf("%s\n", note)))
    },

    # Quote special characters for regex matching
    quotemeta = function(string) {
      str_replace_all(string, "(\\W)", "\\\\\\1")
    }

  )
)

gm <- AutoGM$new("simple1", options = list(show_whole_protocol = TRUE))
gm <- AutoGM$new(savegame = "test1.sav")
gm$ready()
gm$play()


# [COMMAND] Shortly describe the group's current situation!
# Sandra: I stand up, walk a few steps and see if I can find anything in the sand.
# Robert: "Hello?! Anybody here?"
# Jill: I walk to the jungle to see if there is something to eat growing on the trees.

