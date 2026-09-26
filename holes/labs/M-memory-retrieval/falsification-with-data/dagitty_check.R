suppressPackageStartupMessages(library(dagitty))
suppressPackageStartupMessages(library(jsonlite))

here <- "holes/labs/M-memory-retrieval/falsification-with-data"
engine <- fromJSON(file.path(here, "engine.json"), simplifyVector = FALSE)
dat <- read.csv(file.path(here, "data.csv"), check.names = FALSE,
                na.strings = "")

q <- function(x) paste0('"', x, '"')
edges <- vapply(engine$projection$directed,
                function(e) paste(q(e[[1]]), "->", q(e[[2]])), "")
bi <- vapply(engine$projection$bidirected,
             function(e) paste(q(e[[1]]), "<->", q(e[[2]])), "")
graph <- dagitty(paste("dag {", paste(c(edges, bi), collapse = "; "), "}"))

rows <- lapply(engine$`implied-cis`, function(ci) {
  vars <- c(ci$x, ci$y, unlist(ci$given))
  supports <- vapply(dat[vars], function(x) length(unique(x[!is.na(x)])), 0L)
  complete <- sum(complete.cases(dat[vars]))
  vacuous <- any(supports < 2) || complete < 5
  test_string <- if (length(ci$given)) {
    paste(q(ci$x), "_||_", q(ci$y), "|",
          paste(vapply(unlist(ci$given), q, ""), collapse = ","))
  } else paste(q(ci$x), "_||_", q(ci$y))
  if (vacuous) {
    return(list(x = ci$x, y = ci$y, given = unlist(ci$given),
                test = test_string, n = complete,
                p_value = NULL, status = "survived-vacuous",
                reason = paste0("constant/thin column support: ",
                                paste(names(supports), supports,
                                      sep = "=", collapse = ", "))))
  }
  ci_object <- structure(list(X = ci$x, Y = ci$y,
                              Z = as.list(unlist(ci$given))),
                         class = "dagitty.ci")
  complete_data <- dat[complete.cases(dat[vars]), vars, drop = FALSE]
  one <- tryCatch(suppressWarnings(
                    localTests(graph, data = complete_data, type = "cis",
                               tests = list(ci_object),
                               abbreviate.names = FALSE)), error = identity)
  if (inherits(one, "error")) {
    return(list(x = ci$x, y = ci$y, given = unlist(ci$given),
                test = test_string, n = complete, p_value = NULL,
                status = "untestable",
                reason = conditionMessage(one)))
  }
  p <- as.numeric(one[1, "p.value"])
  list(x = ci$x, y = ci$y, given = unlist(ci$given), test = test_string,
       n = complete, p_value = p,
       status = if (p < 0.05) "violated" else "survived",
       reason = NULL)
})

write_json(list(dagitty_version = as.character(packageVersion("dagitty")),
                graph = as.character(graph), tests = rows),
           file.path(here, "dagitty-results.json"), pretty = TRUE,
           auto_unbox = TRUE, digits = 17, null = "null")
