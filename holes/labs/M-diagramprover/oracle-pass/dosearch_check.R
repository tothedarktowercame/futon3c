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
mediation <- export$`memory-mediation-graph`

# The faithful ancestral reduction for {V07,V13,V14,V18} has 18 variables.
# This asks for the joint post-intervention channel/outcome distribution. It is
# not an NDE/NIE query: dosearch has no path-specific intervention syntax.
query_string <- "p(V13,V14,V18|do(V07))"
graph_string <- paste(vapply(mediation$arrows, function(edge) {
  paste(edge$from, "->", edge$to)
}, character(1)), collapse = "\n")
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
    "Joint channel/outcome response on the exact ancestral reduction;",
    "not an NDE/NIE or other path-specific estimand."
  ),
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
cat(sprintf("dosearch without/with S05: %s / %s (ancestral nodes %d; internal %d; limit %d)\n",
            result$without_s05$status, result$with_s05$status,
            result$node_count, result$internal_node_count,
            result$package_node_limit))
