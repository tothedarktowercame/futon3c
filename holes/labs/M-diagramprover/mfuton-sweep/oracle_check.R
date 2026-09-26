#!/usr/bin/env Rscript
# Independent dagitty d-separation checks for every emitted mfuton CI.

suppressPackageStartupMessages(library(jsonlite))
if (!requireNamespace("dagitty", quietly = TRUE)) {
  stop("dagitty is unavailable; the sweep requires the existing oracle venv")
}
suppressPackageStartupMessages(library(dagitty))

args <- commandArgs(trailingOnly = FALSE)
file_arg <- sub("^--file=", "", args[grep("^--file=", args)])
here <- dirname(normalizePath(file_arg))
engine <- fromJSON(file.path(here, "engine-results.json"), simplifyVector = FALSE)

quote_id <- function(id) paste0('"', gsub('"', '\\"', id), '"')

load_graph <- function(source_file) {
  spec <- fromJSON(file.path(here, "converted", source_file), simplifyVector = FALSE)
  nodes <- vapply(spec$variables, function(variable) {
    suffix <- if (identical(variable$kind, "latent-unobserved")) " [latent]" else ""
    paste0(quote_id(variable$id), suffix)
  }, character(1))
  arrows <- vapply(spec$arrows, function(edge) {
    paste0(quote_id(edge$from), " -> ", quote_id(edge$to))
  }, character(1))
  dagitty(paste0("dag { ", paste(c(nodes, arrows), collapse = "; "), " }") )
}

checked <- 0L
agreements <- 0L
discrepancies <- list()
fixture_results <- list()

for (fixture in engine$fixtures) {
  graph <- load_graph(fixture$`source-file`)
  fixture_checked <- 0L
  fixture_discrepancies <- list()
  for (ci in fixture$`implied-independencies`) {
    given <- unlist(ci$given, use.names = FALSE)
    holds <- isTRUE(dseparated(graph, ci$x, ci$y, given))
    checked <- checked + 1L
    fixture_checked <- fixture_checked + 1L
    if (holds) {
      agreements <- agreements + 1L
    } else {
      discrepancy <- list(
        fixture = fixture$`example-id`,
        verdict_type = "d-separation",
        our_verdict = list(x = ci$x, y = ci$y, given = given, holds = TRUE),
        oracle = "dagitty",
        oracle_verdict = list(holds = FALSE),
        rob_expectation = NULL
      )
      discrepancies[[length(discrepancies) + 1L]] <- discrepancy
      fixture_discrepancies[[length(fixture_discrepancies) + 1L]] <- discrepancy
    }
  }
  fixture_results[[length(fixture_results) + 1L]] <- list(
    example_id = fixture$`example-id`,
    checked = fixture_checked,
    discrepancies = fixture_discrepancies
  )
}

result <- list(
  tool_versions = list(
    R = paste(R.version$major, R.version$minor, sep = "."),
    dagitty = as.character(packageVersion("dagitty"))
  ),
  dagitty = list(checked = checked, agreements = agreements),
  fixtures = fixture_results,
  discrepancies = discrepancies
)
write_json(result, file.path(here, "r-results.json"), pretty = TRUE,
           auto_unbox = TRUE, null = "null")
cat(sprintf("mfuton dagitty %d/%d; discrepancies %d\n",
            agreements, checked, length(discrepancies)))
