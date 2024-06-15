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
      self$protocol <- add_row(self$protocol, role = "event", content = "session_start")

    },

    # Start a message loop
    play = function(intro = TRUE) {

      cat("\014") # Clear screen
      private$.print()

      if (intro) {
        content <- "[COMMAND] Shortly describe the group's current situation!" # TODO: put this in yaml
        self$protocol <- add_row(self$protocol, role = "user", content = content)
        private$.send()
        private$.print()
      }

      while (TRUE) { # The game loop

        # get user input
        input <- readline(">>> ")

        # prepare for detection
        regex_pcs <- paste0("^(", paste(str_escape(names(self$gamebook$pc)), collapse = "|"), "):? ") %>%
          regex(ignore_case = TRUE)
        regex_commands <- paste0("^(", paste(str_escape(self$gm$commands), collapse = "|"), ") ") %>%
          regex(ignore_case = TRUE)
        regex_simple_commands <- paste0("^(", paste(str_escape(names(self$gm$commands)), collapse = "|"), ") ") %>%
          regex(ignore_case = TRUE)

        tryCatch({

          # Event "x": no query, close app
          if (str_to_lower(input) == "x") {
            self$protocol <- add_row(self$protocol, role = "event", content = "session_end")
            return(invisible(self$protocol))

          # Event "save": no query
          } else if (str_to_lower(str_sub(input, 1, 5)) == "save ") {
            self$protocol <- add_row(self$protocol, role = "event", content = "save")
            file <- str_sub(input, 6)
            self$save(file)
            next

          # Special Command "redo": send query
          } else if (str_to_lower(input) == "redo" || str_to_lower(str_sub(input, 1, 5)) == "redo ") {
            content <- str_match(input, regex("^redo ?(.*)$", ignore_case = TRUE))[1, 2]
            self$protocol <- add_row(self$protocol, role = "redo", content = content)

          # Special Command "help": add default content and send query
          } else if (str_to_lower(input) == "help") {
            self$protocol <- add_row(self$protocol, role = "help", content = "")

          # Player commands: send query
          } else if (str_detect(input, regex_pcs)) {
            self$protocol <- add_row(self$protocol, role = "user", content = input)

          # Version commands: send query
          } else if (str_detect(input, regex_commands)) {
            self$protocol <- add_row(self$protocol, role = "user", content = input)

          # Simple GM commands (if allowed): switch to standard commands and send query
          } else if (self$options$allow_simple_commands && str_detect(input, regex_simple_commands)) {
            command <- str_match(regex_simple_commands)
            content <- str_replace(input, regex_simple_commands, paste0(self$gm$commands[[command]], " "))
            self$protocol <- add_row(self$protocol, role = "user", content = content)

          # Direct commands (if allowed): add standard command and send query
          } else if (self$options$allow_direct_commands) {
            content <- paste(self$gm$commands$command, input)
            self$protocol <- add_row(self$protocol, role = "user", content = content)

          # Invalid input: no query
          } else {
            stop("Invalid input!")
          }

          # receive and print answer
          private$.send()
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

      # find all to send
      protocol <- self$protocol %>%
        filter(send != "never"  & role %in% c("system", "user", "assistant"))

      # melt consecutive user entries
      runs <- rle(protocol$role)
      protocol$number <- rep(1:length(runs$lengths), runs$lengths)
      protocol %>%
        mutate(notes = duplicated(number)) %>%
        mutate(content = ifelse(notes, paste0("(Note: ", content, ")"), content)) %>%
        group_by(number) %>%
        summarize(role = role[1], content = paste(content, collapse = "\n\n")) %>%
        ungroup() %>%
        select(role, content)

    },

    # send a message to llm
    .send = function() {

      chat <- self$protocol

      # build system prompt
      # TODO: this is for later when system gets additional info from advisor agent

      # remove events
      chat <- chat %>% filter(role != "event")

      # handle redo: remove revised assistant, if redo: else: remove all redo
      remove_assistant <- which(chat$role[-1] == "redo")
      if (length(remove_assistant)) chat <- chat[-remove_assistant, ]
      if (tail(chat$role, 1) == "redo") {
        # current redo
        last_user <- max(which(chat$role == "user"))
        redo_text <- chat %>%
          filter(row_number() > last_user) %>%
          pull(content) %>%
          paste(collapse = "\n")
        if (redo_text != "") {
          chat$content[last_user] <- paste0(
            chat$content[last_user],
            "\n\n[INFO] Additional Information: ", # TODO: put this template into yaml
            redo_text)
        }
      }
      chat <- chat %>% filter(role != "redo")

      # handle help: remove old help + assistant, current help to user
      help_text <- "[COMMAND] Describe all commands that players can use and how they should write them. Keep it short and create your answer as a list. Explain the commands' meanings but don't write anything about how you are supposed to react to them."
      remove_help <- which(chat$role == "help") %>% .[. != nrow(chat)] %>% c(., . + 1)
      if (length(remove_help)) {
        chat <- chat[-remove_help, ]
      }
      chat <- chat %>%
        mutate(content = ifelse(role == "help", !!help_text, content)) %>%
        mutate(role = ifelse(role == "help", "user", role))

      answer <- query(chat, screen = FALSE)
      self$protocol <- bind_rows(self$protocol, c(answer$message))

    },

    # Read gamebook files from local storage
    .read = function(path) {
      readLines(path) %>% paste(collapse = "\n")
    },

    # Print the chat history
    .print = function() {

      # Remove system prompt
      protocol <- self$protocol %>%
        filter(role != "system")

      # Determine visibility
      protocol <- protocol %>%
        mutate(show = ifelse(role == "user" |
                             role == "assistant" & lag(role, 1, "") != "help" & lead(role, 1, "") != "redo" |
                             row_number() >= n() - 1,
                             TRUE, FALSE))

      # Print protocol # TODO: this could be nice with all roles individually handled
      cat("\014") # Clear screen
      for (i in seq_len(nrow(protocol))) {
        content <- protocol$content[i]
        if (protocol$role[i] == "assistant") content <- paste("AutoGM:", content)
        if (protocol$role[i] %in% c("redo", "help")) content <- paste(protocol$role[i], content)
        if (!protocol$show[i]) {
          if (self$options$show_whole_protocol) {
            cat(white(content))
          } else {
            next
          }
        } else {
          if (protocol$role[i] == "user") {
            cat(blue(content))
          } else if (protocol$role[i] == "assistant") {
            cat(content)
          } else {
            cat(green(content))
          }
        }
        cat("\n\n")
      }

    },

    # Print a note from the GM system # TODO: this now clashes with event color / and never used
    .print_note = function(note) {
      cat(green(sprintf("%s\n\n", note)))
    },

    # Print an error from the GM system
    .print_error = function(note) {
      cat(red(sprintf("\n%s\n\n", note)))
    },

    # Quote special characters for regex matching TODO: where do we use this? use index()? or str_escape() instead
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

# TODO: we need a good dialog template here and an alternative for play() which loops through stack
