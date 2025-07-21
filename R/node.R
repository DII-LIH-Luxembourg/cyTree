Node <- R6::R6Class("Node",
                    public = list(
                      id = NULL,
                      name = NULL,
                      parent = NULL,
                      children = list(),
                      positive_markers = list(),
                      negative_markers = list(),

                      initialize = function(id, name, parent = NULL, positive_markers = list(), negative_markers = list()) {
                        self$id <- id
                        self$name <- name
                        self$parent <- parent
                        self$positive_markers <- positive_markers
                        self$negative_markers <- negative_markers
                        self$children <- list()
                      },

                      add_child = function(child_node) {
                        stopifnot(inherits(child_node, "Node"))
                        child_node$parent <- self
                        self$children <- c(self$children, list(child_node))
                      },

                      get_path = function() {
                        if (is.null(self$parent)) {
                          return(self$name)
                        } else {
                          return(paste0(self$parent$get_path(), " > ", self$name))
                        }
                      },

                      to_list = function() {
                        list(
                          id = self$id,
                          name = self$name,
                          positive_markers = self$positive_markers,
                          negative_markers = self$negative_markers,
                          children = lapply(self$children, function(c) c$to_list())
                        )
                      },

                      print_node = function(indent = 0) {
                        cat(paste0(strrep("  ", indent), "- ", self$name, " (ID: ", self$id, ")\n"))
                        for (child in self$children) {
                          child$print_node(indent + 1)
                        }
                      },

                      find_by_id = function(id) {
                        if (self$id == id) return(self)
                        for (child in self$children) {
                          found <- child$find_by_id(id)
                          if (!is.null(found)) return(found)
                        }
                        return(NULL)
                      },

                      to_dataframes = function() {
                        nodes <- data.frame()
                        edges <- data.frame()

                        # Recursive helper function
                        walk_tree <- function(node) {
                          node_row <- data.frame(
                            id = node$id,
                            name = node$name,
                            positive_markers = paste(unlist(node$positive_markers), collapse = ","),
                            negative_markers = paste(unlist(node$negative_markers), collapse = ","),
                            stringsAsFactors = FALSE
                          )
                          nodes <<- rbind(nodes, node_row)

                          for (child in node$children) {
                            edges <<- rbind(edges, data.frame(from = node$id, to = child$id, stringsAsFactors = FALSE))
                            walk_tree(child)
                          }
                        }

                        walk_tree(self)
                        return(list(nodes = nodes, edges = edges))
                      }

                    )
)
