#!/usr/bin/env Rscript

library(data.table)
library(rjson)
library(argparse)
library(ggplot2)
library(chron)

calculate_profit = function(start, end, direction) {
    if (identical(direction, "buy")) {
        return(end - start)
    }
    else {
        return(start - end)
    }
}
parser <- ArgumentParser(description="make plots from the trades json")
parser$add_argument("filename", nargs="?",
                    default="data/eur_usd_trades.json")

args <- parser$parse_args()
dt <- rbindlist(fromJSON(file=args$filename))
dt[, time := as.chron(time)]
dt[, profit := calculate_profit(open_price, close_price, direction)]
setkey(dt, outcome)
setkey(dt, direction)
print(dt)

