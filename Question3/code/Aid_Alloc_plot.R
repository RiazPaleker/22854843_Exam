alloc <- read.csv("./data/Ukraine_Aid/Financial Allocations.csv")
commit <- read.csv("./Data/Ukraine_Aid/Financial Commitments.csv")

library(dplyr)
library(tidyr)

EU_alloc <- function(alloc) {

    alloc_EU <- alloc %>%
        filter(EU.member == 1) %>%
        select(-EU.member) %>%
        gather(Variable, value, -Country)
    return(alloc_EU)
}

alloc_EU_long <- EU_alloc(alloc)
