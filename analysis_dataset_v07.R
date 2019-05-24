library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(fBasics)
library(stargazer)
library(sjPlot)
require(scales)
library(stringr)


# Set wd
# setwd("C:/Users/gmatejka/Cross Listing/Data")
setwd("C:/Users/Gabriel/Desktop/masterarbeit_dateien/19.03/data")


#df <- read_xlsx(paste0(getwd(), "/bonds2.xlsx"), n_max = 10000)
df <- read_xlsx(paste0(getwd(), "/bonds2.xlsx"))


df <- df %>% 
  rename(
    issuer_name = Issuer,
    issuer_nationality = `Issuer Nationality of Operations`,
    issuer_rating = `S&P Issuer Rating (Current)`,
    issue_type = `Issue Type`, # ? 
    pricing_year = `pricing year`,
    
    deal_type = `Deal Type`, #MBS, ... 
    deal_nationality = `Deal Nationality`,
    deal_value_face = `Deal Value $ (Face)`,
    
    listings_nationality = `Listings Nationality`, #Where is the bond listed
    listings = Listings, #No idea
    listings_currency = `Currency Code`,
    
    bank_parent = `Bank Parent`, 
    float_yn = `Float (Y/N)`,
    syndicated_yn = `Syndicated (Y/N)`,
    tranch_value = `Tranche Value $ (Face)`, # This variable is also used by gu et al, what does it mean?
    maturity_years = `maturity year`,
 
    spread_percent = `Spread To Benchmark/Discount margin %`,
    spread_bps = `Spread to Benchmark/Discount margin (bps)`,

    industry_general = `General Industry Group (GIG)`,
    industry_specific = `Specific Industry Group (SIG)`
  )



# Data preparation
df$listings_nationality <- as.factor(df$listings_nationality)
origin.countries <- c("USA", "CAN", "CHN", "JPN", "GER", "FRA")

# Create a new df only with the foreign bonds
x <- df$deal_nationality != df$listings_nationality
x[is.na(x)] <- FALSE

df_cross <- df[x, ]
df_cross <- df_cross[df_cross$listings_nationality != "Unknown", ]

# Change Rating and Industry to numbers 


x <- c("AAA","AA+","AA","AA-","A+","A","A-","B+","B","B-","BB+","BB","BB-","BBB+","BBB","BBB-","CCC+","CCC","CCC-","D")
x <- as.data.frame(x); x

head(df_cross)

x$x %in% df_cross$issuer_rating



# Create Vector with unique countries 
df_cross.countries <-
  df_cross$listings_nationality %>%
  as.character() %>% 
  str_split(pattern = ";") %>% 
  unlist(use.names = FALSE) %>% 
  trimws(which = "left") %>%
  unique()
  
df_cross.countries
  

# Cleaning
#-> double the 
#head(df_cross[,c(18,19)])


# Duplicate the rows which have multiple listings
df_cross <- df_cross %>% 
  mutate(listings_nationality = strsplit(as.character(listings_nationality), ";")) %>% 
  unnest(listings_nationality)

df_cross <- unique(df_cross)


df_cross2 <- df_cross[df_cross$listings_nationality != "Luxembourg", ]
fit <- lm(spread_percent ~ issuer_rating + deal_value_face, data = df_cross)
fitx <- summary(fit); fitx

fit <- lm(spread_percent ~ issuer_rating + deal_value_face + industry_general + gov + stab + law + voi
          , data = df_cross)
fitx <- summary(fit); fitx

fit <- lm(spread_percent ~ issuer_rating + deal_value_face + industry_general + X1 + X2 + X3 + X4 + X5, data = df_cross2)
fitx <- summary(fit); fitx




fitx <- summary(fit) # show results
stargazer(fitx, type="html", out = paste0(getwd(), "test3.html"))

stargazer(fit, type="html", out = paste0(getwd(), "test3.html"), title="Results", align=TRUE)

fit <- glm(`Spread_To_Benchmark/Discount_margin_%` ~ experience.stan +
             `S&P_Issuer_Rating_(Launch)` +
             `Deal_Value_$_(Face)` +
             `General_Industry_Group_(GIG)`, data=yiha)
summary(fit) # show results





# Bank parent experience --------------------------------------------------
# 
# ### Data preparation
# df_cross <- df_cross[order(df_cross$pricing_date, decreasing = FALSE), ]
# 
# df_bankparent <- strsplit(df_cross$Bank_Parent, split = ";")
# df_bankparent <- plyr::ldply(df_bankparent, rbind)
# df_bankparent <- as.data.frame(apply(df_bankparent, 2, function(x) gsub('\\s+', '',x)))
# 
# bankparent_list <- as.character(unique(unlist(df_bankparent)))
# bankparent_list <- bankparent_list[!is.na(bankparent_list)]
# 
# # create vector for experience list for each observation
# experience.list <- rep(0, nrow(df_bankparent))
# 
# # Create vector for experience count for each bank
# experience.count <- as.data.frame(cbind(bankparent_list, rep(0, length(bankparent_list))))
# experience.count <- experience.count[!is.na(experience.count$bankparent_list), ]
# experience.count$V2 <- as.numeric(as.character(experience.count$V2))
# 
# 
# # Create empty matrix to capture the current experience level
# #df_bankparent_count[,] <- matrix(ncol=ncol(df_bankparent), rep(NA, prod(dim(df_bankparent))))
# 
# df_bankparent_count <- matrix(ncol = ncol(df_bankparent), nrow = nrow(df_bankparent))
# head(df_bankparent_count)
# 
# 
# 
# ### Capture Experience
# # Loop through all rows and columns: capture experience & increase by 1 per observed entry
# for (i_row in 1:nrow(df_bankparent)) {
# 
#   for (i_col in 1:ncol(df_bankparent)) {
#   if (is.na(df_bankparent[i_row, i_col])) next
# 
#     for (i_b in 1:length(bankparent_list)) {
# 
#       # look for the bank in the bank-matrix
#       if (df_bankparent[i_row, i_col] == bankparent_list[i_b]) {
# 
#         # enter current experience into the exp-matrix
#         df_bankparent_count[i_row, i_col] <- as.numeric(experience.count[experience.count$bankparent_list == bankparent_list[i_b], ][2])
# 
#         # add entry to experience count
#         experience.count[experience.count$bankparent_list == bankparent_list[i_b], ][2] <- experience.count[experience.count$bankparent_list == bankparent_list[i_b], ][2] + 1
#       }
#     }
#   }
# }
# 
# experience.list <- rowSums(df_bankparent_count, na.rm = T); experience.list
#  
# 
# #################
# 
# yiha <- cbind(df_cross, experience.list)
# yiha$experience.log <- log(experience.list)
# 
# yiha$experience.stan <- (experience.list - mean(experience.list))/sd(experience.list)
# 


# Governance Indicators ---------------------------------------------------

df.gov <- read.csv("WGIData.csv", sep = ",")  

df.gov.estimate <- df.gov[str_detect(df.gov$Indicator.Name, pattern = "Estimate"), ]
df.gov.estimate <- df.gov.estimate[, c(1:4, 13:23)]

cols <- names(df.gov.estimate)[5:15]
df.gov.estimate[cols] <- lapply(df.gov.estimate[cols], as.numeric)
str(df.gov.estimate)


############################################################################################################
# create a preliminary distance measure for each observation in the data set
# the distance is calculated by subtracting (?) deal_nationality from listings_nationality
# the gov.distance measures

df.gov.estimate <- df.gov.estimate %>% 
  rename(
    country_name = ï..Country.Name,
    country_code = Country.Code,
    indicator_name = Indicator.Name,
    indicator_code = Indicator.Code
  )


# 1 all gov-measures from listings_nationality
# 2 all gov measures from deal_nationality

ls_list.nationality <- list()
for (i in 1:nrow(df_cross)){
  ls_list.nationality[[i]] <- df.gov.estimate[df.gov.estimate$country_name == paste0(df_cross$listings_nationality[i]), ]
  }

ls_deal.nationality <- list()
for (i in 1:nrow(df_cross)){
  ls_deal.nationality[[i]] <- df.gov.estimate[df.gov.estimate$country_name == paste0(df_cross$deal_nationality[i]), ]
}

ls_distance <- list()
for (i in 1:nrow(df_cross)) {
    if(nrow(ls_list.nationality[[i]][5:15]) != nrow(ls_deal.nationality[[i]][5:15])) {
    ls_distance[[i]] <- NA
  } else
  ls_distance[[i]] <- abs(ls_list.nationality[[i]][5:15] - ls_deal.nationality[[i]][5:15])
}

# shows the column of 2007
# ls_distance[[22]] <- ls_distance[[22]][,(df_cross$pricing_year[22] - 2006)]; ls_distance[[22]]


ls_distance_yr <- list()
for (i in 1:nrow(df_cross)) {
  if (is.na(ls_distance[[i]])) {ls_distance_yr[[i]] <- NA} else
  ls_distance_yr[[i]] <- ls_distance[[i]][,(df_cross$pricing_year[i] - 2006)]
}

df_distance_yr <- do.call(rbind.data.frame, ls_distance_yr)

names(df_distance_yr) <- c("cor", "gov", "stab", "reg", "law", "voi")

df_cross <- cbind(df_cross, df_distance_yr)




#abs(lx[,5:15]-ch[,5:15])


x2 <- rowMeans(abs(
  df.gov.estimate[df.gov.estimate$country_name == paste0(x1$listings_nationality[1]),][5:15] -
    df.gov.estimate[df.gov.estimate$country_name == paste0(x1$deal_nationality[1]),][5:15]))

x3 <- rowMeans(abs(
  df.gov.estimate[df.gov.estimate$country_name == paste0(x1$listings_nationality[2]),][5:15] -
    df.gov.estimate[df.gov.estimate$country_name == paste0(x1$deal_nationality[2]),][5:15]))

x4 <- rbind(x2, x3)


# Loop all absolute differences

ls_diff <- list()

for (i in 1:(nrow(df_cross))) {
  tryCatch({   
    ls_diff[[i]] <- 
      rowMeans(abs(
        df.gov.estimate[df.gov.estimate$country_name == paste0(x1$listings_nationality[i]),][5:15] -
          df.gov.estimate[df.gov.estimate$country_name == paste0(x1$deal_nationality[i]),][5:15]))
    }, error=function(e){})
}






# Calculate average score over the time period
# df.gov.estimate$average <- rowSums(df.gov.estimate[, 5:15])/10


#Countries in the sample
df_cross.countries


#! two country names missing atm
df_cross.countries %in% df.gov.estimate$ï..Country.Name
df.gov.estimate.sample <- df.gov.estimate[df.gov.estimate$ï..Country.Name %in% df_cross.countries, ]









#GOV.data
gov.USA <- df.gov.estimate[df.gov.estimate$Country.Code == "USA", ]$average
gov.CAN <- df.gov.estimate[df.gov.estimate$Country.Code == "CAN", ]$average
gov.GBR <- df.gov.estimate[df.gov.estimate$Country.Code == "GBR", ]$average
gov.GER <- df.gov.estimate[df.gov.estimate$Country.Code == "DEU", ]$average
gov.FRA <- df.gov.estimate[df.gov.estimate$Country.Code == "FRA", ]$average
gov.JPN <- df.gov.estimate[df.gov.estimate$Country.Code == "JPN", ]$average
gov.CHN <- df.gov.estimate[df.gov.estimate$Country.Code == "CHN", ]$average


#GOV.distance
df.gov.estimate.sample$distance_USA <- abs(df.gov.estimate.sample[, "average"] - gov.USA) #R uses 6 units and starts over again
df.gov.estimate.sample$distance_CAN <- abs(df.gov.estimate.sample[, "average"] - gov.CAN)
df.gov.estimate.sample$distance_GBR <- abs(df.gov.estimate.sample[, "average"] - gov.GBR)
df.gov.estimate.sample$distance_GER <- abs(df.gov.estimate.sample[, "average"] - gov.GER)
df.gov.estimate.sample$distance_FRA <- abs(df.gov.estimate.sample[, "average"] - gov.FRA)
df.gov.estimate.sample$distance_JPN <- abs(df.gov.estimate.sample[, "average"] - gov.JPN)
df.gov.estimate.sample$distance_CHN <- abs(df.gov.estimate.sample[, "average"] - gov.CHN)


## Summarize output for presentation
table.gov <- aggregate(df.gov.estimate.sample$average, by=list(Category=df.gov.estimate.sample$Country.Code), FUN=mean)
table.gov$USA <- aggregate(df.gov.estimate.sample$distance_USA, by=list(Category=df.gov.estimate.sample$Country.Code), FUN=mean)[2]
table.gov$CAN <- aggregate(df.gov.estimate.sample$distance_CAN, by=list(Category=df.gov.estimate.sample$Country.Code), FUN=mean)[2]
table.gov$GBR <- aggregate(df.gov.estimate.sample$distance_GBR, by=list(Category=df.gov.estimate.sample$Country.Code), FUN=mean)[2]
table.gov$GER <- aggregate(df.gov.estimate.sample$distance_GER, by=list(Category=df.gov.estimate.sample$Country.Code), FUN=mean)[2]
table.gov$CHN <- aggregate(df.gov.estimate.sample$distance_CHN, by=list(Category=df.gov.estimate.sample$Country.Code), FUN=mean)[2]
table.gov$JPN <- aggregate(df.gov.estimate.sample$distance_JPN, by=list(Category=df.gov.estimate.sample$Country.Code), FUN=mean)[2]
table.gov$FRA <- aggregate(df.gov.estimate.sample$distance_FRA, by=list(Category=df.gov.estimate.sample$Country.Code), FUN=mean)[2]


plot(table.gov$x)
summary(table.gov)

(distance_a - distance_b) / std_dev_dataset


table.gov[,3] <- round(table.gov[,3], digits = 2)
table.gov[,4] <- round(table.gov[,4], digits = 2)
table.gov[,5] <- round(table.gov[,5], digits = 2)
table.gov[,6] <- round(table.gov[,6], digits = 2)
table.gov[,7] <- round(table.gov[,7], digits = 2)
table.gov[,8] <- round(table.gov[,8], digits = 2)
table.gov[,9] <- round(table.gov[,9], digits = 2)


stargazer(table.gov[,c(1, 3:9)], digits = 1, summary=FALSE, type="html", out = paste0(getwd(), "test2.html"))



# --> Can I do it per year, if I have the data?
# Different measurements --> I want the "Estimate"
# Corruption, 




# Trust distance ----------------------------------------------------------

trust <- read.csv("IGO_igounit_v2.3/igounit_v2.3.csv")

#! missing countries: JPN, CAN, FRA, 
trust_colnames <- colnames(trust)

tolower(df_cross.countries) %in% trust_colnames
# 3-5, 14-16, 25, 26, 30 not working

trust2 <- trust[, trust_colnames %in% tolower(df_cross.countries)]
trust <- cbind(trust[,1:3], trust2)


# Create a dyadic matrix china
trust.china <- as.data.frame(matrix(ncol = ncol(trust), nrow = nrow(trust)))

for (ii in 1:nrow(trust)) {
  for (i in 1:ncol(trust)) {
    if (trust[ii, i] == 1 & trust$china[ii] == 1) {
      trust.china[ii, i] <- 1
    } else {
      trust.china[ii, i] <- 0
    }
  }
}



trust.china <- cbind(trust[, 1:3], trust.china[, 4:ncol(trust.china)])
trust.china <- aggregate(. ~ ioname + orgname, trust.china, sum)

xx <- colSums(trust.china[, 4:ncol(trust.china)])
x2 <- colSums(trust.china[, 4:ncol(trust.china)])

plot(xx, xlab = "Countries", ylab = "Count", main = "IGO Membership Count + Duration")
plot(x2, xlab = "Countries", ylab = "Count", main = "IGO Membership Count")


#Count only treaties together, not total years spent together

for (ii in 1:nrow(trust.china)) {
  for (i in 4:ncol(trust.china)) {
    if (trust.china[ii, i] >0) {
      trust.china[ii, i] <- 1
    } else next
  }
}


### Output for presentation
table.chn <- colSums(trust.china[, 4:ncol(trust.china)])
table.chn <- as.data.frame(cbind(x.names, table.chn))

table.chn$table.chn <- as.numeric(as.character(table.chn$table.chn))
x.names <- colnames(trust[4:ncol(trust)])

#ggplot(table.chn, aes(x=reorder(x.names, -table.chn), y=table.chn)) +
  ggplot(table.chn, aes(x=x.names, y=table.chn)) +
  geom_point() +  theme(axis.text.x = element_text(angle = 40, hjust = 1, vjust = 1))


#how to compute the distance measure??
# Control variables? Distance, size?  
# treaty with the EU?


fit <- lm(`Spread_To_Benchmark/Discount_margin_%` ~ `S&P_Issuer_Rating_(Launch)`, data=df_cross)
summary(fit) # show results


fit <- lm(`Spread_To_Benchmark/Discount_margin_%` ~ experience.list + `S&P_Issuer_Rating_(Launch)` + `Deal_Value_$_(Face)`, data=df_cross)
summary(fit) # show results


fit1 <- lm(income~gender+race,data=Dataframe1)
summary(fit1)




# Other code --------------------------------------------------------------

print.tbl_df <- function(x, ...) {
  print.data.frame(x, ...)
  invisible(x)
}

#trust.china <- as.data.frame(apply(trust.china,  c(1,2), function(x) {sub(pattern = -1, replacement = 0, x)}))

plots.dir.path <- list.files(tempdir(), pattern="rs-graphics", full.names = TRUE); 
plots.png.paths <- list.files(plots.dir.path, pattern=".png", full.names = TRUE)

file.copy(from=plots.png.paths, to="plots")




#Literatur

Wooldrich oder Green --> empirische Literatur

Representativitätsproblem -> wie stellt sich das Sampling dar?

Das Sample muss groß genug sein!
Besteht eine Systematik, ob Unternehmen in den Bereich kommen oder nicht?

Kleine Bonds werden nicht aufgenommen?? Einschränkungen bei der Representativität

Beispiel quantitative Forschungsbeiträge --> Best practices in data collection and preparation
Herman Aguinis --> outlier management


Quantitativ besser wenn um objektivität besonders wichtig ist,
Allgemein generalisierbar auf die population

experience --> diskrete variable

Klassifizierung von quantiativen Variablen
Ebenenen der Variablen --> Ebene besprechen

Beobachtbarkeit der variable
Variable latent --> trust (Messen erfolgt über approximation)
Validitätsproblem? Ländernähe, Alter des Landes

#eigentlich habe ich hierarchische daten, für die aber nicht kontrolliert wird
# andere variablen mit trust nehmen, die auch möglich sind

bond spread --> diskret, kann nicht negativ werden (glm, ml)
bond spread --> intervall skaliert

logarithmierung der erklärenden variable (independent):
1% in der erklärenden, x in der abhängigen variable

zu jeder variable eine referenzliteratur, wie sie dort behandelt wird

mahalanobis distanz messen!!!
eine datei machen, in der alle variablen drinnen sind!!!
  
  
http://www.physiol.ox.ac.uk/~raac/R.shtml
