## packages
library(tidyverse)    # a set of tools to wrangle the data
library(magrittr)    # just for use the pipe-operator: %<>%(compound assignment pipe-operator)
library(lubridate)    # transform the dates stroed as chr
library(readxl)

excel_sheets('C:/R/2019.9.9_r_try/test_scaner_combi_191019.xlsx')    # check sheets
scanner <- read_excel('C:/R/2019.9.9_r_try/test_scaner_combi_191019.xlsx')


names(scanner)
scanner %<>% select(2, 3, 4, 5, 6, 7, 8, 9, 10, 13)  

# the col of Date & time is chr, so add the col that transform the type into dttm
# scanner_date <- scanner %>% select(6, 3)
scanner %<>%    
    mutate(time = paste(Date, Time)) %>%
    mutate(TimeCST = ymd_hms(time, tz = "Asia/Taipei")) %>%
    mutate(Btwn1970 = as.numeric(TimeCST)) %>% 
    mutate(hour = hour(TimeCST)) %>%
    select(1, 2, 5, 7, 6, 8, 9, 3, 12, 14, 13) 

# function
# the element minus the next element
minus_next <- function(x) {
    for(i in x) {
        position <- which(x == i)    # the first argument of which is a logical vector or array
        x[position] <- x[position + 1] - x[position]
    }
    return(x)    # 後一項減前一項的function    
}

# if the value of minus is greater than the Max interval
interval_check <- function(x, y) {
    cond_switch <- function(cond) {
        switch(cond, 
               "As_1m" = 82.6,
               "As_0m" = 221.7,
               "Nc_1m" = 119.00,
               "Nc_0m" = 736.6)
    }
    ifelse(x > cond_switch(y) | is.na(x), 1, x)
}
# niche index
niche_index <- function(species1, species2) {
    list(niche_breadth = Breadth_index2(species1, species2),    # breadth index2
         niche_oberlap = overlap_index(species1, species2))    # overlap index
}

# breadth index2
Breadth_index2 <- function(species1, species2) {
    list(Hi = list(species1 = -(sum(species1*log(species1), na.rm = TRUE)),
                   species2 = -(sum(species2*log(species2), na.rm = TRUE))),
         Bi = list(species1 = reciprocal(sum(species1**2, na.rm = TRUE)),     # reciprocal
                   species2 = reciprocal(sum(species2**2, na.rm = TRUE))))
}

# reciprocal
reciprocal <- function(x){
    1/x
}

# overlap index
overlap_index <- function(species1, species2) {
    list(Alpha = alpha(species1, species2), Pianka = pianka(species1, species2))
}    # alpha & pianka

# alpha
alpha <- function(species1, species2) {
    list(alpha12 = sum(species1*species2, na.rm = TRUE)/sum(species1**2, na.rm = TRUE), 
         alpha21 = sum(species1*species2, na.rm = TRUE)/sum(species2**2, na.rm = TRUE))
}

# pianka
pianka <- function(species1, species2) {
    sum(species1*species2, na.rm = TRUE)/sqrt(sum(species1**2, na.rm = TRUE)*sum(species2**2, na.rm = TRUE))
}

# in particular period
corr_minus2 <- function(species, height, hr1, hr2, condi) {
    scanner %>% 
        filter(Species == species, Height == height, hour >= hr1, hour <= hr2) %>%
        arrange(Btwn1970) %>%
        group_by(Order, Site, ID) %>%
        mutate(minus = minus_next(Btwn1970)) %>%
        mutate(corr = interval_check(minus, condi)) %>%
        ungroup %>%
        select(corr) %>%
        sum
}
corr_minus3 <- function(species, hr1, hr2, condi1, condi2) {
    corr_minus2(species, 0, hr1, hr2, condi1) + corr_minus2(species, 1, hr1, hr2, condi2)
}

corr_minus <- function(species, height, hr, condi) {
    scanner %>% 
        filter(Species == species, Height == height, hour == hr) %>%
        arrange(Btwn1970) %>%
        group_by(Order, Site, ID) %>%
        mutate(minus = minus_next(Btwn1970)) %>%
        mutate(corr = interval_check(minus, condi)) %>%
        ungroup %>%
        select(corr) %>%
        sum
}

corr_minus4 <- function(species, hr, condi1, condi2) {
    corr_minus(species, 0, hr, condi1) + corr_minus(species, 1, hr, condi2)
}

#### 2 periods ####
## a. 18-23 b. 0-5
# PAs2, PNc2
PAs2a <- corr_minus3("As.", 18, 23, "As_0m", "As_1m")
PAs2b <- corr_minus3("As.", 0, 5, "As_0m", "As_1m")
PAs2 <- c(PAs2a/(PAs2a+PAs2b), PAs2b/(PAs2a+PAs2b))

PNc2a <- corr_minus3("Nc.", 18, 23, "Nc_0m", "Nc_1m")
PNc2b <- corr_minus3("Nc.", 0, 5, "Nc_0m", "Nc_1m")
PNc2 <-c(PNc2a/(PNc2a+PNc2b), PNc2b/(PNc2a+PNc2b))

periods2 <- tibble(PAs2, PNc2)

niche_index(PAs2, PNc2)
#### 3 periods ####
## a. 18-21 b. 22-1 c. 2-5
# PAs3, PNc3
PAs3a <- corr_minus3("As.", 18, 21, "As_0m", "As_1m")
PAs3b1 <- corr_minus3("As.", 22, 23, "As_0m", "As_1m")
PAs3b2 <- corr_minus3("As.", 0, 1, "As_0m", "As_1m")
PAs3b <- PAs3b1 + PAs3b2
PAs3c <- corr_minus3("As.", 2, 5, "As_0m", "As_1m")
PAsabc <- PAs3a + PAs3b + PAs3c
PAs3 <- c(PAs3a/PAsabc, PAs3b/PAsabc, PAs3c/PAsabc)

PNc3a <- corr_minus3("Nc.", 18, 21, "Nc_0m", "Nc_1m")
PNc3b1 <- corr_minus3("Nc.", 22, 23, "Nc_0m", "Nc_1m")
PNc3b2 <- corr_minus3("Nc.", 0, 1, "Nc_0m", "Nc_1m")
PNc3b <- PNc3b1 + PNc3b2
PNc3c <- corr_minus3("Nc.", 2, 5, "Nc_0m", "Nc_1m")
PNcabc <- PNc3a + PNc3b + PNc3c
PNc3 <- c(PNc3a/PNcabc, PNc3b/PNcabc, PNc3c/PNcabc)

periods3 <- tibble(period = c("18-21", "22-1", "2-5"), PAs3, PNc3)
periods3 %>%
    gather(PAs3, PNc3, key = "species", value = "prob") %>%
    ggplot() +
    geom_bar(
        mapping = aes(x = period, y = prob, fill = species),
        stat = "identity",
        position = "dodge"
    ) +
    scale_x_discrete(limit = periods3$period)

niche_index(PAs3, PNc3)


#### 4 periods ####
## a. 18-20 b. 21-23 c. 0-2 d. 3-5
# PAs4, PNc4
PAs4a <- corr_minus3("As.", 18, 20, "As_0m", "As_1m")
PAs4b <- corr_minus3("As.", 21, 23, "As_0m", "As_1m")
PAs4c <- corr_minus3("As.", 0, 2, "As_0m", "As_1m")
PAs4d <- corr_minus3("As.", 3, 5, "As_0m", "As_1m")
PAs4ad <- PAs4a + PAs4b + PAs4c +PAs4d
PAs4 <- c(PAs4a/PAs4ad, PAs4b/PAs4ad, PAs4c/PAs4ad, PAs4d/PAs4ad)

PNc4a <- corr_minus3("Nc.", 18, 20, "Nc_0m", "Nc_1m")
PNc4b <- corr_minus3("Nc.", 21, 23, "Nc_0m", "Nc_1m")
PNc4c <- corr_minus3("Nc.", 0, 2, "Nc_0m", "Nc_1m")
PNc4d <- corr_minus3("Nc.", 3, 5, "Nc_0m", "Nc_1m")
PNc4ad <- PNc4a + PNc4b + PNc4c + PNc4d
PNc4 <- c(PNc4a/PNc4ad, PNc4b/PNc4ad, PNc4c/PNc4ad, PNc4d/PNc4ad)

niche_index(PAs4, PNc4)

#### 6 periods ####
## a. 18-19 b. 20-21 c. 22-23 d. 0-1 e. 2-3 f. 4-5
# PAs6, PNc6
PAs6a <- corr_minus3("As.", 18, 19, "As_0m", "As_1m")
PAs6b <- corr_minus3("As.", 20, 21, "As_0m", "As_1m")
PAs6c <- corr_minus3("As.", 22, 23, "As_0m", "As_1m")
PAs6d <- corr_minus3("As.", 0, 1, "As_0m", "As_1m")
PAs6e <- corr_minus3("As.", 2, 3, "As_0m", "As_1m")
PAs6f <- corr_minus3("As.", 4, 5, "As_0m", "As_1m")
PAs6af <- PAs6a + PAs6b + PAs6c + PAs6d + PAs6e + PAs6f
PAs6 <- c(PAs6a/PAs6af, PAs6b/PAs6af, PAs6c/PAs6af, PAs6d/PAs6af, PAs6e/PAs6af, PAs6f/PAs6af)

PNc6a <- corr_minus3("Nc.", 18, 19, "Nc_0m", "Nc_1m")
PNc6b <- corr_minus3("Nc.", 20, 21 , "Nc_0m", "Nc_1m")
PNc6c <- corr_minus3("Nc.", 22, 23, "Nc_0m", "Nc_1m")
PNc6d <- corr_minus3("Nc.", 0, 1, "Nc_0m", "Nc_1m")
PNc6e <- corr_minus3("Nc.", 2, 3, "Nc_0m", "Nc_1m")
PNc6f <- corr_minus3("Nc.", 4, 5, "Nc_0m", "Nc_1m")
PNc6af <- PNc6a + PNc6b + PNc6c + PNc6d + PNc6e + PNc6f
PNc6 <- c(PNc6a/PNc6af, PNc6b/PNc6af, PNc6c/PNc6af, PNc6d/PNc6af, PNc6e/PNc6af, PNc6f/PNc6af)

niche_index(PAs6, PNc6)
#### 12 periods ####
# PAs12, PNc12
PAs12a <- corr_minus4("As.", 18, "As_0m", "As_1m")
PAs12b <- corr_minus4("As.", 19, "As_0m", "As_1m")
PAs12c <- corr_minus4("As.", 20, "As_0m", "As_1m")
PAs12d <- corr_minus4("As.", 21, "As_0m", "As_1m")
PAs12e <- corr_minus4("As.", 22, "As_0m", "As_1m")
PAs12f <- corr_minus4("As.", 23, "As_0m", "As_1m")
PAs12g <- corr_minus4("As.", 0, "As_0m", "As_1m")
PAs12h <- corr_minus4("As.", 1, "As_0m", "As_1m")
PAs12i <- corr_minus4("As.", 2, "As_0m", "As_1m")
PAs12j <- corr_minus4("As.", 3, "As_0m", "As_1m")
PAs12k <- corr_minus4("As.", 4, "As_0m", "As_1m")
PAs12l <- corr_minus4("As.", 5, "As_0m", "As_1m")
PAs12l <- 1173    # 因為5~6點As在1m處沒資料
PAs12al <- PAs12a + PAs12b + PAs12c + PAs12d + PAs12e + PAs12f + PAs12g + PAs12h +
    PAs12i + PAs12j + PAs12k + PAs12l
PAs12 <- c(PAs12a/PAs12al, PAs12b/PAs12al, PAs12c/PAs12al, PAs12d/PAs12al,
           PAs12e/PAs12al, PAs12f/PAs12al, PAs12g/PAs12al, PAs12h/PAs12al,
           PAs12i/PAs12al, PAs12j/PAs12al, PAs12k/PAs12al, PAs12l/PAs12al)

PNc12a <- corr_minus4("Nc.", 18, "Nc_0m", "Nc_1m")
PNc12b <- corr_minus4("Nc.", 19, "Nc_0m", "Nc_1m")
PNc12c <- corr_minus4("Nc.", 20, "Nc_0m", "Nc_1m")
PNc12d <- corr_minus4("Nc.", 21, "Nc_0m", "Nc_1m")
PNc12e <- corr_minus4("Nc.", 22, "Nc_0m", "Nc_1m")
PNc12f <- corr_minus4("Nc.", 23, "Nc_0m", "Nc_1m")
PNc12g <- corr_minus4("Nc.", 0, "Nc_0m", "Nc_1m")
PNc12h <- corr_minus4("Nc.", 1, "Nc_0m", "Nc_1m")
PNc12i <- corr_minus4("Nc.", 2, "Nc_0m", "Nc_1m")
PNc12j <- corr_minus4("Nc.", 3, "Nc_0m", "Nc_1m")
PNc12k <- corr_minus4("Nc.", 4, "Nc_0m", "Nc_1m")
PNc12l <- corr_minus4("Nc.", 5, "Nc_0m", "Nc_1m")
PNc12l <- 0    # 因為5~6點處沒有Nc資料
PNc12al <- PNc12a + PNc12b + PNc12c + PNc12d + PNc12e + PNc12f + PNc12g + PNc12h +
    PNc12i + PNc12j + PNc12k + PNc12l
PNc12 <- c(PNc12a/PNc12al, PNc12b/PNc12al, PNc12c/PNc12al, PNc12d/PNc12al,
           PNc12e/PNc12al, PNc12f/PNc12al, PNc12g/PNc12al, PNc12h/PNc12al,
           PNc12i/PNc12al, PNc12j/PNc12al, PNc12k/PNc12al, PNc12l/PNc12al)

periods12 <- tibble(period = as.character(c(18:23, 0:5)), PAs12, PNc12)

periods12 %>% 
    gather(PAs12, PNc12, key = "species", value = "prob") %>%
    ggplot() + 
    geom_bar(
        mapping = aes(x = period, y = prob, fill = species),
        stat = "identity",
        position = "dodge"
    ) +
    scale_x_discrete(limit = periods12$period)
niche_index(PAs12, PNc12)







# github test
scanner <- scanner %>% 
    select(2, 3, 4, 5, 6, 7, 8, 9, 10) %>% 
    mutate(time = paste(Date, Time, sep = " ")) %>% 
    mutate(TimeCST = ymd_hms(time, tz = "Asia/Taipei")) %>% 
    filter(!is.na(TimeCST)) %>%     # filter wrong format(time)
    select(1, 2, 7, 5, 6, 8, 9, 11) %>% 
    filter(Species %in% c("As.", "Nc."))
