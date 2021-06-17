library("data.table")
library("ggplot2")

rawData <- fread("data/all_stocks_5yr.csv")
data <- rawData[, .(date, close, Name)]

obsPerStock <- data[, .N, keyby = .(Name)]
stocksPerDay <- data[, .N, keyby = .(date)]

calculateReturn <- function(current, previous) {
  (current - previous) / previous
}

sortedData <- data[order(Name, date)]
sortedData <- sortedData[,
                         .(
                           date = date[2:(length(close) - 1)],
                           close = close[2:(length(close) - 1)],
                           return = calculateReturn(close[2:(length(close) - 1)], close[1:(length(close) - 2)]),
                           nextReturn = calculateReturn(close[3:length(close)], close[2:(length(close) - 1)])
                         ),
                         keyby = .(Name)]

sortedData <- sortedData[,
                         .(
                           Name,
                           close,
                           return,
                           nextReturn,
                           place = rank(-return, ties.method = "first"),
                           nextPlace = rank(-nextReturn, ties.method = "first"),
                           rev_place = rank(return, ties.method = "first"),
                           next_rev_Place = rank(nextReturn, ties.method = "first")
                         ),
                         keyby = .(date)]

calculateValue <- function(data) {
  data <- data[order(date)]
  data$value <- cumprod(data$nextReturn + 1)
  list(data)
}

strategy <- sortedData[,
                       .(strategy = calculateValue(.SD),
                         stdDev = sd(.SD$nextReturn)),
                       keyby = .(place)]

strategy$profit <- sapply(strategy$strategy, function(tb) {last(tb$value)})

plotStrategy <- function(strategy) {
  table <- strategy$strategy[[1]]
  x <- 1:length(table$value)
  title <- paste0("Strategy ", strategy$place)
  list(
    ggplot(table, aes(x = x, y = value)) + geom_point(shape = 1, size = 1) + ggtitle(title),
    ggplot(table, aes(x = nextPlace)) + geom_histogram(
      binwidth = 1,
      colour = "black",
      fill = "green"
    ) + ggtitle(title),
    ggplot(table, aes(x = nextPlace)) + geom_density(
      bw = 4,
      alpha = .5,
      colour = "black",
      fill = "green"
    ) + ggtitle(title)
  )
}

plotStrategy(strategy[1])
plotStrategy(strategy[200])
plotStrategy(strategy[470])

meanProfit <- mean(strategy$profit)
(ggplot(strategy, aes(x = place, y = profit))
  + geom_point(shape = 1, size = 1)
  + ggtitle("Profit for each strategy")
  + geom_hline(aes(yintercept = meanProfit),  color = "red", linetype = "solid", size = 1))

meanStdDev <- mean(strategy$stdDev)
(ggplot(strategy, aes(x = place, y = stdDev))
+ geom_point(shape = 1, size = 1)
+ ggtitle("Std dev for each strategy")
+ geom_hline(aes(yintercept = meanStdDev),  color = "red", linetype = "solid", size = 1))


rev_strategy <- sortedData[,
                       .(strategy = calculateValue(.SD),
                         stdDev = sd(.SD$nextReturn)),
                       keyby = .(rev_place)]

rev_strategy$profit <- sapply(rev_strategy$strategy, function(tb) {last(tb$value)})

plot_rev_Strategy <- function(strategy) {
  table <- strategy$strategy[[1]]
  x <- 1:length(table$value)
  title <- paste0("Strategy ", strategy$rev_place, ". (Reverse order)")
  list(
    ggplot(table, aes(x = x, y = value)) + geom_point(shape = 1, size = 1) + ggtitle(title),
    ggplot(table, aes(x = next_rev_Place)) + geom_histogram(
      binwidth = 1,
      colour = "black",
      fill = "green"
    ) + ggtitle(title),
    ggplot(table, aes(x = next_rev_Place)) + geom_density(
      bw = 4,
      alpha = .5,
      colour = "black",
      fill = "green"
    ) + ggtitle(title)
  )
}

plot_rev_Strategy(rev_strategy[1])
plot_rev_Strategy(rev_strategy[200])
plot_rev_Strategy(rev_strategy[470])

rev_meanProfit <- mean(rev_strategy$profit)
(ggplot(rev_strategy, aes(x = rev_place, y = profit))
  + geom_point(shape = 1, size = 1)
  + ggtitle("Profit for each strategy. (Reverse order)")
  + geom_hline(aes(yintercept = rev_meanProfit),  color = "red", linetype = "solid", size = 1))

rev_meanStdDev <- mean(rev_strategy$stdDev)
(ggplot(rev_strategy, aes(x = rev_place, y = stdDev))
  + geom_point(shape = 1, size = 1)
  + ggtitle("Std dev for each strategy. (Reverse order)")
  + geom_hline(aes(yintercept = rev_meanStdDev),  color = "red", linetype = "solid", size = 1))

groupNumber <- 5
groups <- 1:(2*groupNumber)
defaultLength <- 476
groupSize <- defaultLength%/%(2*groupNumber)

calculateGroupNextReturn <- function(data) {
  data <- data[order(place)]
  aggregatedNextReturn <- sapply(split(
        data$nextReturn,
        ceiling(seq_along(data$nextReturn)/groupSize)
        ), mean)
  list(data.table(group = groups, nextReturn = aggregatedNextReturn[groups]))
}

groupedData <- sortedData[,
                       .(groupedNextReturns = calculateGroupNextReturn(.SD)),
                       keyby = .(date)]

getGroupStrategy <- function(i) {
  nextReturn <- sapply(groupedData$groupedNextReturns, function(table) {table$nextReturn[i]})
  value <- cumprod(nextReturn + 1)
  data.table(date = groupedData$date,
             nextReturn,
             value)
}

groupStrategy <- data.table(group = groups, strategy = lapply(groups, getGroupStrategy))

groupStrategy$stdDev <- sapply(groupStrategy$strategy, function(table) {sd(table$nextReturn)})

groupStrategy$profit <- sapply(groupStrategy$strategy, function(table) {last(table$value)})

plotGroupStrategy <- function(strategy) {
  table <- strategy$strategy[[1]]
  x <- 1:length(table$value)
  title <- paste0("Strategy for group ", strategy$group)
  list(
    ggplot(table, aes(x = x, y = value)) + geom_point(shape = 1, size = 1) + ggtitle(title)
  )
}

plotGroupStrategy(groupStrategy[1])
plotGroupStrategy(groupStrategy[5])
plotGroupStrategy(groupStrategy[9])

meanProfit <- mean(groupStrategy$profit)
(ggplot(groupStrategy, aes(x = group, y = profit, label = group))
  + geom_point(shape = 1, size = 10)
  + ggtitle("Profit for each group strategy")
  + geom_hline(aes(yintercept = meanProfit),  color = "red", linetype = "solid", size = 1)
  + geom_text())

meanStdDev <- mean(groupStrategy$stdDev)
(ggplot(groupStrategy, aes(x = group, y = stdDev, label = group))
  + geom_point(shape = 1, size = 10)
  + ggtitle("Std dev for each group strategy")
  + geom_hline(aes(yintercept = meanStdDev),  color = "red", linetype = "solid", size = 1)
  + geom_text())

(ggplot(groupStrategy, aes(x = stdDev, y = profit, label = group))
  + geom_point(shape = 1, size = 10)
  + ggtitle("Profit vs Std Dev")
  + geom_text())

rev_calculateGroupNextReturn <- function(data) {
  data <- data[order(rev_place)]
  aggregatedNextReturn <- sapply(split(
    data$nextReturn,
    ceiling(seq_along(data$nextReturn)/groupSize)
  ), mean)
  list(data.table(group = groups, nextReturn = aggregatedNextReturn[groups]))
}

rev_groupedData <- sortedData[,
                          .(groupedNextReturns = rev_calculateGroupNextReturn(.SD)),
                          keyby = .(date)]

rev_getGroupStrategy <- function(i) {
  nextReturn <- sapply(rev_groupedData$groupedNextReturns, function(table) {table$nextReturn[i]})
  value <- cumprod(nextReturn + 1)
  data.table(date = rev_groupedData$date,
             nextReturn,
             value)
}

rev_groupStrategy <- data.table(group = groups,
                            strategy = lapply(groups, rev_getGroupStrategy)
)

rev_groupStrategy$stdDev <- sapply(rev_groupStrategy$strategy, function(table) {sd(table$nextReturn)})

rev_groupStrategy$profit <- sapply(rev_groupStrategy$strategy, function(table) {last(table$value)})

plotGroupStrategy(rev_groupStrategy[1])
plotGroupStrategy(rev_groupStrategy[5])
plotGroupStrategy(rev_groupStrategy[9])

meanProfit <- mean(rev_groupStrategy$profit)
(ggplot(rev_groupStrategy, aes(x = group, y = profit, label = group))
  + geom_point(shape = 1, size = 10)
  + ggtitle("Profit for each group strategy. (Reverse order)")
  + geom_hline(aes(yintercept = meanProfit),  color = "red", linetype = "solid", size = 1)
  + geom_text())

meanStdDev <- mean(rev_groupStrategy$stdDev)
(ggplot(rev_groupStrategy, aes(x = group, y = stdDev, label = group))
  + geom_point(shape = 1, size = 10)
  + ggtitle("Std dev for each group strategy. (Reverse order)")
  + geom_hline(aes(yintercept = meanStdDev),  color = "red", linetype = "solid", size = 1)
  + geom_text())

(ggplot(rev_groupStrategy, aes(x = stdDev, y = profit, label = group))
  + geom_point(shape = 1, size = 10)
  + ggtitle("Profit/Std Dev. (Reverse order)")
  + geom_text())

allGroupData <- data.table(
                  group = groups,
                  profit = c(groupStrategy$profit[1:groupNumber], rev(rev_groupStrategy$profit[1:groupNumber])),
                  stdDev = c(groupStrategy$stdDev[1:groupNumber], rev(rev_groupStrategy$stdDev[1:groupNumber])),
                  strategy = c(groupStrategy$strategy[1:groupNumber], rev(rev_groupStrategy$strategy[1:groupNumber])))

(ggplot(allGroupData, aes(x = group, y = profit, label = group))
  + geom_point(shape = 16, size = 10, colour = "green", alpha = 0.5)
  + ggtitle("Profit per group. (All groups)")
  + geom_text())

(ggplot(allGroupData, aes(x = group, y = stdDev, label = group))
  + geom_point(shape = 16, size = 10, colour = "green", alpha = 0.5)
  + ggtitle("Std dev for each group strategy. (All groups)")
  + geom_text())

(ggplot(allGroupData, aes(x = stdDev, y = profit, label = group))
  + geom_point(shape = 16, size = 10, colour = "green", alpha = 0.5)
  + ggtitle("Profit vs Std Dev. (All groups)")
  + geom_text())

firstGroup <- strategy[1:groupSize, -(1:2)]
firstGroup$group <- "1"
middleGroup <- rev_strategy[(4*groupSize + 1):(5*groupSize), -(1:2)]
middleGroup$group <- "6"
lastGroup <- rev_strategy[1:groupSize, -(1:2)]
lastGroup$group <- "10"

someGroups <- rbind(firstGroup, middleGroup, lastGroup)

(ggplot(someGroups, aes(x = stdDev, y = profit, label = group, color = group))
  + geom_point(shape = 16, size = 4, alpha = 0.5)
  + ggtitle("Profit vs Std Dev. (1, 6, 10 groups)")
  #+ geom_text()
  )

