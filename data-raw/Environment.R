## Restore the INCOMELEVEL category labels of `Environment` to the order of the
## SODAS 2.50 source files (enviro.sds / enviro.xml): 25, 75, 50, 90, 100.
##
## Versions up to 0.2.5 stored the labels as 25, 50, 75, 90, 100 while keeping
## the proportions in the source order, so the labels 50 and 75 were interchanged.
## Only these labels change. All 266 modal proportions and all 364 interval
## endpoints are untouched and agree with the source position by position.
##
## Run from the package root:  Rscript data-raw/Environment.R

load("data/Environment.rda")

old_labels <- c("25", "50", "75", "90", "100")
new_labels <- c("25", "75", "50", "90", "100")

x <- Environment$INCOMELEVEL
stopifnot(inherits(x, "symbolic_modal"), length(x) == 14L)

y <- lapply(unclass(x), function(e) {
  stopifnot(identical(e$var, old_labels), length(e$prop) == 5L)
  e$var <- new_labels
  e
})
attributes(y) <- attributes(x)

# Replace the column without touching any attribute of the table.
table_attributes <- attributes(Environment)
columns <- unclass(Environment)
columns$INCOMELEVEL <- y
attributes(columns) <- table_attributes
Environment <- columns

save(Environment, file = "data/Environment.rda", compress = "gzip", version = 3)
