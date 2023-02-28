library(arules)
library(arulesViz)

setwd("/Users/vineethreddy/Documents/projects")

data <- read.csv("international_matches.csv")

transactions <- lapply(1:nrow(data), function(i) unlist(data[i,c("home_team","away_team","tournament","home_team_result")], use.names = FALSE))

transactions <- as(transactions, "transactions")
inspect(transactions)
rules <- apriori(transactions, parameter = list(supp = 0.01, conf = 0.1, target = "rules"))

# Print the rules
inspect(rules)

plot(rules, method = "scatterplot")

plot(rules, method = "graph", main = "Rule Coverage")


itemFrequencyPlot(transactions, topN=20, type="absolute")

SortedRules_s <- sort(rules, by="support", decreasing=TRUE)
apriori(inspect(SortedRules_s[1:15]))

SortedRules_c <- sort(rules, by="confidence", decreasing=TRUE)
apriori(inspect(SortedRules_c[1:15]))

SortedRules_l <- sort(rules, by="lift", decreasing=TRUE)
apriori(inspect(SortedRules_l[1:15]))