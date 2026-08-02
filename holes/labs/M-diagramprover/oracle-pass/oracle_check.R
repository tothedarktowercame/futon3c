#!/usr/bin/env Rscript
# Independent dagitty structure checks over the durable engine export.

suppressPackageStartupMessages(library(jsonlite))
if (!requireNamespace("dagitty", quietly = TRUE)) {
  stop("dagitty is unavailable; installation outcome must be reported")
}
suppressPackageStartupMessages(library(dagitty))

args <- commandArgs(trailingOnly = FALSE)
file_arg <- sub("^--file=", "", args[grep("^--file=", args)])
here <- dirname(normalizePath(file_arg))
export <- fromJSON(file.path(here, "engine-export.json"), simplifyVector = FALSE)

dagitty_graph <- function(data) {
  quote_id <- function(id) paste0('"', gsub('"', '\\"', id), '"')
  nodes <- paste(vapply(data$variables, quote_id, character(1)), collapse = " ")
  arrows <- vapply(data$arrows, function(edge) {
    paste0(quote_id(edge$from), " -> ", quote_id(edge$to))
  }, character(1))
  dagitty(paste0("dag { ", nodes, "; ", paste(arrows, collapse = "; "), " }"))
}

memory <- dagitty_graph(export$`memory-graph`)
implication_results <- lapply(export$`implied-independencies`, function(ci) {
  given <- unlist(ci$given, use.names = FALSE)
  holds <- dseparated(memory, ci$x, ci$y, given)
  list(x = ci$x, y = ci$y, given = given, holds = isTRUE(holds))
})
implication_disagreements <- Filter(function(item) !item$holds, implication_results)

# Converse is verdict-level only: dagitty's basis enumeration intentionally
# differs from the engine's minimal-set enumeration.
basis <- impliedConditionalIndependencies(memory)
basis_results <- lapply(basis, function(ci) {
  list(x = ci$X, y = ci$Y, given = unlist(ci$Z, use.names = FALSE))
})
write_json(basis_results, file.path(here, "dagitty-basis.json"), pretty = TRUE,
           auto_unbox = TRUE, null = "null")

q3 <- list()
for (name in names(export$`q3-variants`)) {
  graph <- dagitty_graph(export$`q3-variants`[[name]])
  q3[[name]] <- list(
    marginal_separated = isTRUE(dseparated(graph, "M-in-store", "V12-minus-M")),
    v18_separated_given_v12_minus_m = isTRUE(
      dseparated(graph, "M-in-store", "V18", "V12-minus-M")
    )
  )
}
q3_expected <- list(
  `star-forest` = list(marginal_separated = TRUE,
                       v18_separated_given_v12_minus_m = TRUE),
  `populated-graph` = list(marginal_separated = FALSE,
                           v18_separated_given_v12_minus_m = TRUE)
)
q3_disagreements <- Filter(function(name) !identical(q3[[name]], q3_expected[[name]]),
                           names(q3))

result <- list(
  tool_versions = list(R = paste(R.version$major, R.version$minor, sep = "."),
                       dagitty = as.character(packageVersion("dagitty"))),
  dagitty_implications = list(
    checked = length(implication_results),
    agreements = length(implication_results) - length(implication_disagreements),
    disagreements = implication_disagreements
  ),
  dagitty_converse = list(
    emitted = length(basis_results),
    basis_file = "dagitty-basis.json"
  ),
  q3 = list(verdicts = q3, disagreements = q3_disagreements)
)
write_json(result, file.path(here, "r-results.json"), pretty = TRUE,
           auto_unbox = TRUE, null = "null")

if (length(implication_disagreements) || length(q3_disagreements)) {
  stop("dagitty disagreement; inspect r-results.json")
}
cat(sprintf("dagitty implications: %d agreements, 0 disagreements\n",
            length(implication_results)))
cat(sprintf("dagitty converse: emitted %d CIs for engine verification\n",
            length(basis_results)))
cat("Q3: 4/4 verdicts agree\n")
