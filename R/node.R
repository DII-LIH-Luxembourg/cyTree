#' @title Node Class for cyTree
#' @name Node
#'
#' @description
#' A tree node used to annotate CyTOF cluster hierarchies. Each node contains a unique ID,
#' a name, marker metadata, and pointers to its parent and children.
#'
#' @field id Character. Unique ID for the node.
#' @field name Character. Name/label of the node.
#' @field parent `Node` or `NULL`. Pointer to the parent node.
#' @field children List of `Node` objects. Direct children of this node.
#' @field positive_markers List. Names of markers considered positive.
#' @field negative_markers List. Names of markers considered negative.
#'
#' @export
Node <- R6::R6Class("Node",
                    public = list(
                      id = NULL,
                      name = NULL,
                      parent = NULL,
                      children = NULL,
                      positive_markers = NULL,
                      negative_markers = NULL,

                      #' @description Create a new Node
                      #' @param id Character. Unique ID for the node.
                      #' @param name Character. Name/label of the node.
                      #' @param parent `Node` or `NULL`. Parent node.
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
                      #' @param child_node A `Node` object to add as a child.
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
                      #' @param indent Integer. Indentation level for pretty printing.
                      print_node = function(indent = 0) {
                        cat(strrep("  ", indent), "- ", self$name, "\n", sep = "")
                        for (child in self$children) child$print_node(indent + 1)
                      },

                      #' @description Find a node by ID (first match)
                      #' @param id Character. ID to search for.
                      find_by_id = function(id) {
                        if (self$id == id) return(self)
                        for (child in self$children) {
                          found <- child$find_by_id(id)
                          if (!is.null(found)) return(found)
                        }
                        NULL
                      },

                      #' @description Find a node by name (first match)
                      #' @param name Character. Name to search for.
                      find_by_name = function(name) {
                        if (self$name == name) return(self)
                        for (child in self$children) {
                          found <- child$find_by_name(name)
                          if (!is.null(found)) return(found)
                        }
                        NULL
                      },

                      #' @description Return data.frames of all nodes and edges
                      to_dataframes = function() {
                        nodes <- data.frame()
                        edges <- data.frame()
                        walk_tree <- function(node) {
                          nodes <<- rbind(nodes, data.frame(
                            id = node$id,
                            name = node$name,
                            positive_markers = paste(unlist(node$positive_markers), collapse = ","),
                            negative_markers = paste(unlist(node$negative_markers), collapse = ","),
                            stringsAsFactors = FALSE
                          ))
                          for (child in node$children) {
                            edges <<- rbind(edges, data.frame(from = node$id, to = child$id, stringsAsFactors = FALSE))
                            walk_tree(child)
                          }
                        }
                        walk_tree(self)
                        list(nodes = nodes, edges = edges)
                      },

                      #' @description Get direct children of this node
                      #' @return List of `Node` objects (direct children).
                      get_children = function() {
                        self$children
                      },

                      #' @description Get all descendant nodes of this node (recursive)
                      #' @return List of all descendant `Node` objects (children, grandchildren, etc.).
                      get_descendants = function() {
                        descendants <- list()
                        collect <- function(node) {
                          for (child in node$children) {
                            descendants <<- c(descendants, list(child))
                            collect(child)
                          }
                        }
                        collect(self)
                        descendants
                      },

                      #' @description Get the parent of this node
                      #' @return A `Node` object representing its parent, or NULL if this is the root.
                      get_parent = function() {
                        self$parent
                      },

                      #' @description Plot the tree structure in the terminal with ASCII indentation
                      #' @param id Character. ID of the node to plot (defaults to this node).
                      #' @param name Character. Name of the node to plot (if id is NULL).
                      #' @param show_ids Logical. Include node IDs next to names if TRUE. Default FALSE.
                      #' @param indent_char Character. String to use for one level of indentation. Default two spaces.
                      plot_tree = function(id = NULL, name = NULL, show_ids = FALSE, indent_char = "  ") {
                        # Determine target node
                        target <- self
                        if (!is.null(id)) {
                          found <- self$find_by_id(id)
                          if (is.null(found)) stop("No node with id=", id)
                          target <- found
                        } else if (!is.null(name)) {
                          found <- self$find_by_name(name)
                          if (is.null(found)) stop("No node with name=", name)
                          target <- found
                        }
                        # Recursive printer
                        print_fn <- function(node, indent = 0) {
                          prefix <- strrep(indent_char, indent)
                          label <- if (show_ids) paste0(node$name, " (", node$id, ")") else node$name
                          cat(prefix, "- ", label, "\n", sep = "")
                          for (child in node$children) print_fn(child, indent + 1)
                        }
                        print_fn(target, 0)
                      }
                    )
)
