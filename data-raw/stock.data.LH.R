# Rebuild data/stock.data.LH.rda from the package source directory.
# Source: TTR 0.24.4, ttrc (randomly generated; not an actual security).
# This script uses the stored TTR data and does not generate random draws.
# Install ggInterval and TTR before running it; no internet download is used.
stopifnot(as.character(utils::packageVersion("TTR")) == "0.24.4")
stopifnot(file.exists("DESCRIPTION"), dir.exists("data"))
source_env <- new.env(parent = baseenv())
utils::data("ttrc", package = "TTR", envir = source_env)
stock.data <- subset(source_env$ttrc[, c("Date", "Close", "Low", "High")],
                     format(Date, "%Y-%m") %in% c("1985-01", "1985-02", "1985-03"))
stock.data$Month <- factor(month.abb[as.integer(format(stock.data$Date, "%m"))],
                           levels = month.abb[1:3])
stock.data$Day <- as.integer(format(stock.data$Date, "%d"))
stock.data.LH <- ggInterval::classic2sym(stock.data, groupby = "customize",
                                       minData = stock.data$Low,
                                       maxData = stock.data$High)
stock.data.LH <- dplyr::bind_cols(stock.data.LH$intervalData, stock.data)
stopifnot(nrow(stock.data.LH) == 62L,
          identical(names(stock.data.LH),
                    c("V1", "Date", "Close", "Low", "High", "Month", "Day")),
          identical(stock.data.LH$V1$min, stock.data.LH$Low),
          identical(stock.data.LH$V1$max, stock.data.LH$High))
save(stock.data.LH, file = "data/stock.data.LH.rda", compress = "xz", version = 2)
