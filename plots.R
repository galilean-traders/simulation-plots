#!/usr/bin/env Rscript

library(data.table)
library(rjson)
library(argparse)
library(ggplot2)
library(tools)
library(chron)


save_plot <- function(basename, plot) {
    name <- paste(basename, deparse(substitute(plot)), ".png", sep="_")
    ggsave(
        filename=name,
        plot=plot,
        width=8, height=4.8)
}

parser <- ArgumentParser(description="make plots from the trades json")
parser$add_argument("filename", nargs="?",
                    default="data/eur_usd_trades.json")
args <- parser$parse_args()
basename <- file_path_sans_ext(basename(args$filename))
dt <- rbindlist(fromJSON(file=args$filename))
dt[, datetime := as.POSIXct(time, format="%Y-%m-%dT%H:%M:%OSZ")]
dt[, length := end_candle - start_candle]
dt[, is_weekend := is.weekend(datetime)]
dt[, hours := hours(datetime)]
dt[, weekday := wday(datetime)]
setkey(dt, outcome)
setkey(dt, is_weekend)
setkey(dt, direction)
dt[direction == "buy", profit := close_price - open_price]
dt[direction == "sell", profit := open_price - close_price]

hours_histogram <- ggplot(dt, aes(x=hours, fill=outcome)) +
    geom_histogram(binwidth=1, position="identity", alpha=0.4)
save_plot(basename, hours_histogram)

length_histogram <- ggplot(dt, aes(x=length, fill=outcome)) +
    geom_histogram(binwidth=10, position="identity", alpha=0.4)
save_plot(basename, length_histogram)

weekday_histogram <- ggplot(dt, aes(x=weekdays(datetime), fill=outcome)) +
    geom_histogram(binwidth=1, position="identity", alpha=0.4) +
    scale_x_chron(format="%A", n=7) +
    theme(axis.text.x = element_text(angle=70, hjust=1))
save_plot(basename, weekday_histogram)

spread_histogram <- ggplot(dt, aes(x=(spread / pip), fill=outcome)) +
    geom_histogram(position="identity", alpha=0.4)
save_plot(basename, spread_histogram)

#message("Press Return To Continue")
#invisible(readLines("stdin", n=1))
