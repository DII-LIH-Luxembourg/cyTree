#' @title Node Class for cyTree
#' @name Node
#'
#' @description
#' A tree node used to annotate CyTOF cluster hierarchies. Each node contains a unique ID,
#' a name, marker metadata, and pointers to its parent and children.
#'
#' @export
Node <- R6::R6Class("Node",
                    public = list(

                      #' @description Create a new Node
                      #' @param id Character. Unique ID for the node.
                      #' @param name Character. Name/label of the node.
                      #' @param parent Node or NULL. Parent node.
                      #' @param positive_markers List of marker names that are positive.
                      #' @param negative_markers List of marker names that are negative.
                      initialize = function(id, name, parent = NULL,
                                            positive_markers = list(), negative_markers = list()) {
                        self$id <- id
                        self$name <- name
                        self$parent <- parent
                        self$positive_markers <- positive_markers
                        self$negative_markers <- negative_markers
                        self$children <- list()
                      },

                      #' @description Add a child node
                      #' @param child_node A Node object to add as a child.
                      add_child = function(child_node) {
                        stopifnot(inherits(child_node, "Node"))
                        self$children <- c(self$children, list(child_node))
                        child_node$parent <- self
                      },

                      #' @description Get full path from root to this node
                      get_path = function() {
                        if (is.null(self$parent)) return(self$name)
                        paste0(self$parent$get_path(), " > ", self$name)
                      },

                      #' @description Recursively convert node and children to nested list
                      to_list = function() {
                        list(
                          id = self$id,
                          name = self$name,
                          metadata = list(
                            positive_markers = self$positive_markers,
                            negative_markers = self$negative_markers
                          ),
                          children = lapply(self$children, function(c) c$to_list())
                        )
                      },

                      #' @description Print the node as a tree
                      #' @param indent Indentation level (for pretty printing)
                      print_node = function(indent = 0) {
                        cat(strrep("  ", indent), "- ", self$name, "\n", sep = "")
                        for (child in self$children) {
                          child$print_node(indent + 1)
                        }
                      },

                      #' @description Find a node by ID
                      #' @param id Character. ID to search for.
                      find_by_id = function(id) {
                        if (self$id == id) return(self)
                        for (child in self$children) {
                          found <- child$find_by_id(id)
                          if (!is.null(found)) return(found)
                        }
                        return(NULL)
                      },

                      #' @description Find a node by name
                      #' @param name Character. Name to search for.
                      find_by_name = function(name) {
                        if (self$name == name) return(self)
                        for (child in self$children) {
                          found <- child$find_by_name(name)
                          if (!is.null(found)) return(found)
                        }
                        return(NULL)
                      },

                      #' @description Return data.frames of all nodes and edges
                      to_dataframes = function() {
                        nodes <- data.frame()
                        edges <- data.frame()

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
