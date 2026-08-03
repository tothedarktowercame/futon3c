#!/usr/bin/env Rscript
# dosearch boundary check for the Q2 joint channel/outcome response.

suppressPackageStartupMessages(library(jsonlite))
if (!requireNamespace("dosearch", quietly = TRUE)) {
  stop("dosearch is unavailable; installation outcome must be reported")
}
suppressPackageStartupMessages(library(dosearch))

args <- commandArgs(trailingOnly = FALSE)
file_arg <- sub("^--file=", "", args[grep("^--file=", args)])
here <- dirname(normalizePath(file_arg))
export <- fromJSON(file.path(here, "engine-export.json"), simplifyVector = FALSE)
mediation <- export$`memory-mediation-projection`

# The Clojure export computes the ancestral subgraph, marks every non-kept node
# latent, and reuses admg/latent-project. This asks for the same joint
# post-intervention channel/outcome distribution as before. It is not an
# NDE/NIE query: dosearch has no path-specific intervention syntax.
query_string <- "p(V13,V14,V18|do(V07))"
directed_edges <- vapply(mediation$arrows, function(edge) {
  paste(edge$from, "->", edge$to)
}, character(1))
bidirected_edges <- vapply(mediation$bidirected, function(edge) {
  paste(edge[[1]], "<->", edge[[2]])
}, character(1))
graph_string <- paste(c(directed_edges, bidirected_edges), collapse = "\n")
with_variables <- unlist(mediation$variables, use.names = FALSE)
without_variables <- setdiff(with_variables, c("V13", "V14"))
without_data <- paste0("p(", paste(without_variables, collapse = ","), ")")
with_data <- paste0("p(", paste(with_variables, collapse = ","), ")")

run_query <- function(data_string) {
  tryCatch({
    answer <- dosearch(data_string, query_string, graph_string)
    info <- summary(answer)
    list(status = if (isTRUE(info$identifiable)) "identifiable" else "non-identifiable",
         identifiable = isTRUE(info$identifiable),
         formula = if (is.null(info$formula)) NULL else info$formula,
         error = NULL)
  }, error = function(err) {
    message <- conditionMessage(err)
    list(status = if (grepl("more than 30 nodes", message, fixed = TRUE))
                    "rejected-size-limit" else "error",
         identifiable = NULL, formula = NULL, error = message)
  })
}

result <- list(
  tool_version = as.character(packageVersion("dosearch")),
  encoding = paste(
    "Joint channel/outcome response after admg/latent-project of the exact",
    "ancestral reduction; not an NDE/NIE or other path-specific estimand."
  ),
  kept_set = unlist(mediation$`kept-set`, use.names = FALSE),
  ancestral_nodes = unlist(mediation$`ancestral-nodes`, use.names = FALSE),
  projected_away = unlist(mediation$`projected-away`, use.names = FALSE),
  node_count = length(with_variables),
  internal_node_count = 2L * length(with_variables),
  package_node_limit = 30L,
  without_s05 = c(list(data = without_data, query = query_string,
                       graph = graph_string), run_query(without_data)),
  with_s05 = c(list(data = with_data, query = query_string,
                    graph = graph_string), run_query(with_data))
)
write_json(result, file.path(here, "dosearch-results.json"), pretty = TRUE,
           auto_unbox = TRUE, null = "null")
cat(sprintf("dosearch without/with S05: %s / %s (projected nodes %d; internal %d; limit %d)\n",
            result$without_s05$status, result$with_s05$status,
            result$node_count, result$internal_node_count,
            result$package_node_limit))
