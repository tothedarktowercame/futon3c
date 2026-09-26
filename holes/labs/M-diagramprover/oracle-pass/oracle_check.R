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
lean <- dagitty_graph(export$`lean-graph`)
check_implications <- function(graph, implications) {
  results <- lapply(implications, function(ci) {
    given <- unlist(ci$given, use.names = FALSE)
    holds <- dseparated(graph, ci$x, ci$y, given)
    list(x = ci$x, y = ci$y, given = given, holds = isTRUE(holds))
  })
  disagreements <- Filter(function(item) !item$holds, results)
  list(checked = length(results),
       agreements = length(results) - length(disagreements),
       disagreements = disagreements)
}
memory_implications <- check_implications(memory, export$`implied-independencies`)
lean_implications <- check_implications(lean, export$`lean-implied-independencies`)

# Converse is verdict-level only: dagitty's basis enumeration intentionally
# differs from the engine's minimal-set enumeration.
basis_records <- function(graph) lapply(impliedConditionalIndependencies(graph), function(ci) {
  list(x = ci$X, y = ci$Y, given = unlist(ci$Z, use.names = FALSE))
})
basis_results <- list(memory = basis_records(memory), lean = basis_records(lean))
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

receipt_by_id <- function(id) {
  Filter(function(item) identical(item$id, id), export$receipts)[[1]]
}
r2_receipt <- receipt_by_id("R2")
r2_expected <- setNames(
  lapply(r2_receipt$verdicts, function(item) item[["holds?"]]),
  vapply(r2_receipt$verdicts, function(item) item$graph, character(1))
)
r2_expected$`content-removal-effect` <-
  r2_receipt$`duplication-debt`[["content-removal-effect?"]]
r2_copied <- dagitty_graph(export$`r2-variants`$`copied-class`)
r2_extracted <- dagitty_graph(export$`r2-variants`$`extracted-class`)
r2 <- list(
  `copied-class` = !isTRUE(dseparated(r2_copied, "P19", "P16")),
  `extracted-class` = !isTRUE(dseparated(r2_extracted, "P19", "P16")),
  `content-removal-effect` =
    !isTRUE(dseparated(r2_copied, "remove-content", "P16"))
)
r2_disagreements <- Filter(function(name) !identical(r2[[name]], r2_expected[[name]]),
                           names(r2))

r3_receipt <- receipt_by_id("R3")
r3_expected <- setNames(
  lapply(r3_receipt$verdicts, function(item) item[["holds?"]]),
  vapply(r3_receipt$verdicts, function(item) item$graph, character(1))
)
r3_current <- dagitty_graph(export$`r3-variants`$`current-sensors`)
r3_hypothetical <- dagitty_graph(
  export$`r3-variants`$`with-hypothetical-t05`
)
r3 <- list(
  `current-sensors` = isTRUE(dseparated(
    r3_current, "P16-at-k+1", "P10-at-k", "T04-at-k"
  )),
  `with-hypothetical-t05` = isTRUE(dseparated(
    r3_hypothetical, "P16-at-k+1", "T04-at-k", "T05-at-k"
  ))
)
r3_disagreements <- Filter(function(name) !identical(r3[[name]], r3_expected[[name]]),
                           names(r3))

# Fixture 3: U is explicitly latent, so dagitty may only return observed sets.
bow_frontdoor <- dagitty(paste0(
  "dag { U [latent]; smoking; tar; cancer; ",
  "U -> smoking; U -> cancer; smoking -> tar; tar -> cancer }"
))
bow_adjustment_sets <- adjustmentSets(
  bow_frontdoor, exposure = "smoking", outcome = "cancer", type = "all"
)
bow_adjustment_vectors <- lapply(bow_adjustment_sets, as.character)
bow_observed_sets <- Filter(function(nodes) !any(nodes %in% c("U")),
                            bow_adjustment_vectors)
bow_frontdoor_result <- list(
  dagitty_sets = bow_adjustment_vectors,
  rejected_latent_sets = Filter(function(nodes) any(nodes %in% c("U")),
                                bow_adjustment_vectors),
  observed_candidate_sets = bow_observed_sets,
  candidate_set_count = length(bow_observed_sets),
  backdoor_exhaustion_agrees = identical(length(bow_observed_sets), 0L)
)

result <- list(
  tool_versions = list(R = paste(R.version$major, R.version$minor, sep = "."),
                       dagitty = as.character(packageVersion("dagitty"))),
  dagitty_implications = memory_implications,
  dagitty_lean_implications = lean_implications,
  dagitty_converse = list(
    memory_emitted = length(basis_results$memory),
    lean_emitted = length(basis_results$lean),
    basis_file = "dagitty-basis.json"
  ),
  q3 = list(verdicts = q3, disagreements = q3_disagreements),
  r2 = list(verdicts = r2, disagreements = r2_disagreements),
  r3 = list(verdicts = r3, disagreements = r3_disagreements),
  bow = list(frontdoor_adjustment = bow_frontdoor_result)
)
write_json(result, file.path(here, "r-results.json"), pretty = TRUE,
           auto_unbox = TRUE, null = "null")

if (length(memory_implications$disagreements) ||
    length(lean_implications$disagreements) || length(q3_disagreements) ||
    length(r2_disagreements) || length(r3_disagreements) ||
    !bow_frontdoor_result$backdoor_exhaustion_agrees) {
  stop("dagitty disagreement; inspect r-results.json")
}
cat(sprintf("dagitty memory/Lean implications: %d/%d agreements, 0 disagreements\n",
            memory_implications$agreements, lean_implications$agreements))
cat(sprintf("dagitty converse: emitted memory/Lean %d/%d CIs\n",
            length(basis_results$memory), length(basis_results$lean)))
cat("Q3: 4/4 verdicts agree\n")
cat("R2/R3: 3/3 and 2/2 verdicts agree\n")
cat(sprintf("Book-of-Why dagitty: %d observed adjustment sets; backdoor exhaustion agrees\n",
            bow_frontdoor_result$candidate_set_count))
