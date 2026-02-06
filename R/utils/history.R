#' Undo/Redo State Management
#'
#' Tracks changes to student data and allows undo/redo operations

library(R6)

#' History Manager Class
#'
#' Manages undo/redo stack for student data changes
HistoryManager <- R6::R6Class(
  "HistoryManager",

  public = list(
    #' @description
    #' Initialize with empty history
    initialize = function() {
      private$undo_stack <- list()
      private$redo_stack <- list()
      private$current_state <- NULL
      private$max_stack_size <- 20
    },

    #' @description
    #' Save current state to undo stack
    #' @param state The data to save (typically the student data.frame)
    #' @param description Character description of the action
    push = function(state, description = "") {
      # Store current state if it exists
      if (!is.null(private$current_state)) {
        # Add to undo stack with metadata
        history_entry <- list(
          state = private$current_state,
          description = description,
          timestamp = Sys.time()
        )

        private$undo_stack <- c(list(history_entry), private$undo_stack)

        # Limit stack size to prevent memory issues
        if (length(private$undo_stack) > private$max_stack_size) {
          private$undo_stack <- private$undo_stack[1:private$max_stack_size]
        }
      }

      # Clear redo stack (new action after undo invalidates redo history)
      private$redo_stack <- list()

      # Update current state
      private$current_state <- state

      invisible(self)
    },

    #' @description
    #' Undo last action
    #' @return Previous state or NULL if cannot undo
    undo = function() {
      if (!self$can_undo()) {
        warning("Cannot undo: no history available")
        return(NULL)
      }

      # Save current state to redo stack
      if (!is.null(private$current_state)) {
        redo_entry <- list(
          state = private$current_state,
          description = "Undone action",
          timestamp = Sys.time()
        )
        private$redo_stack <- c(list(redo_entry), private$redo_stack)
      }

      # Pop from undo stack
      previous_entry <- private$undo_stack[[1]]
      private$undo_stack <- private$undo_stack[-1]

      # Update current state
      private$current_state <- previous_entry$state

      return(list(
        state = previous_entry$state,
        description = previous_entry$description
      ))
    },

    #' @description
    #' Redo last undone action
    #' @return Next state or NULL if cannot redo
    redo = function() {
      if (!self$can_redo()) {
        warning("Cannot redo: no redo history available")
        return(NULL)
      }

      # Save current state to undo stack
      if (!is.null(private$current_state)) {
        undo_entry <- list(
          state = private$current_state,
          description = "Redone action",
          timestamp = Sys.time()
        )
        private$undo_stack <- c(list(undo_entry), private$undo_stack)
      }

      # Pop from redo stack
      next_entry <- private$redo_stack[[1]]
      private$redo_stack <- private$redo_stack[-1]

      # Update current state
      private$current_state <- next_entry$state

      return(list(
        state = next_entry$state,
        description = next_entry$description
      ))
    },

    #' @description
    #' Check if undo is available
    #' @return Logical
    can_undo = function() {
      length(private$undo_stack) > 0
    },

    #' @description
    #' Check if redo is available
    #' @return Logical
    can_redo = function() {
      length(private$redo_stack) > 0
    },

    #' @description
    #' Get current state
    #' @return Current state data
    get_current = function() {
      private$current_state
    },

    #' @description
    #' Get history summary
    #' @return List with undo/redo counts and descriptions
    summary = function() {
      list(
        undo_available = self$can_undo(),
        redo_available = self$can_redo(),
        undo_count = length(private$undo_stack),
        redo_count = length(private$redo_stack),
        undo_descriptions = sapply(private$undo_stack, function(x) x$description),
        redo_descriptions = sapply(private$redo_stack, function(x) x$description)
      )
    },

    #' @description
    #' Clear all history
    clear = function() {
      private$undo_stack <- list()
      private$redo_stack <- list()
      private$current_state <- NULL
      invisible(self)
    }
  ),

  private = list(
    undo_stack = NULL,
    redo_stack = NULL,
    current_state = NULL,
    max_stack_size = NULL
  )
)

#' Create History Reactive (Alternative Simpler Approach)
#'
#' @param initial_data Initial student data
#' @return Reactive list with data and undo/redo functions
#' @export
createHistoryReactive <- function(initial_data) {
  # Initialize reactive values
  values <- reactiveValues(
    data = initial_data,
    undo_stack = list(),
    redo_stack = list()
  )

  # Return reactive list with methods
  list(
    data = reactive({ values$data }),

    push = function(new_data, description = "") {
      # Save current state to undo stack
      values$undo_stack <- c(
        list(list(
          state = values$data,
          description = description,
          timestamp = Sys.time()
        )),
        values$undo_stack
      )

      # Limit stack size
      if (length(values$undo_stack) > 20) {
        values$undo_stack <- values$undo_stack[1:20]
      }

      # Clear redo stack
      values$redo_stack <- list()

      # Update data
      values$data <- new_data
    },

    undo = function() {
      if (length(values$undo_stack) > 0) {
        # Save current to redo stack
        values$redo_stack <- c(
          list(list(
            state = values$data,
            description = "Undone action",
            timestamp = Sys.time()
          )),
          values$redo_stack
        )

        # Pop from undo stack
        previous <- values$undo_stack[[1]]
        values$undo_stack <- values$undo_stack[-1]

        # Restore data
        values$data <- previous$state

        return(previous$description)
      }
      return(NULL)
    },

    redo = function() {
      if (length(values$redo_stack) > 0) {
        # Save current to undo stack
        values$undo_stack <- c(
          list(list(
            state = values$data,
            description = "Redone action",
            timestamp = Sys.time()
          )),
          values$undo_stack
        )

        # Pop from redo stack
        next_state <- values$redo_stack[[1]]
        values$redo_stack <- values$redo_stack[-1]

        # Restore data
        values$data <- next_state$state

        return(next_state$description)
      }
      return(NULL)
    },

    can_undo = reactive({ length(values$undo_stack) > 0 }),
    can_redo = reactive({ length(values$redo_stack) > 0 })
  )
}
