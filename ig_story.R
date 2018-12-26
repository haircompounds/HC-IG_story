rm(list = ls())
install.packages("plotly")
install.packages("data.tree")
install.packages("treemap")
library(plotly)
library(data.tree)
library(dplyr)
library(treemap)

setwd("~/hc_IG_story_analysis")

dat <- read.csv("ig_story.csv", na.strings = "")

#dat <- dat[-c(995:nrow(dat)),]
dat <- dat[-which(is.na(dat)),]

#remove whitespace and lower
dat$Category <- sapply(dat$Category, tolower)
dat$Category <- sapply(dat$Category, trimws)
dat$Category <- as.factor(dat$Category)
dat$Impressions <- as.numeric(dat$Impressions)
dat$Viewers <- as.numeric(dat$Viewers)
dat$Reach <- as.numeric(dat$Reach)
dat$Date <- as.Date(dat$Date, format = "%m-%d-%Y")
class(dat$Website.Clicks)
#
#levels(dat$Category) <- c(levels(dat$Category), "employee on-site, education, hair products, other")

# correct spelling errata 
tmp <- as.character(gsub("(emplo)(ee)", "\\1y\\2", dat[(grep("^emploee", dat$Category)),3])[[1]])
levels(dat$Category) <- c(levels(dat$Category), tmp)

tmp1 <- as.character(gsub("(transformat)(on)", "\\1i\\2", dat[(grep("transformaton", dat$Category)),3]))
# because multiple matches
for(i in unique(tmp1)) {
  
  levels(dat$Category) <- c(levels(dat$Category), i)
  
}

tmp2 <- as.character(gsub("(poll)l", "\\1", dat[(grep("polll", dat$Category)),3])[[1]])
levels(dat$Category) <- c(levels(dat$Category), tmp2)

tmp3 <- as.character(gsub("question[s]? and answer[s]?", "q&a", dat[grep("question.*?s", dat$Category), 3]))[1]
levels(dat$Category) <- c(levels(dat$Category), tmp3)

# change the level on emploee and tranformaton
dat[(grep("^emploee", dat$Category)),3] <- tmp
dat[(grep("hair transformaton, in+", dat$Category)),3] <- tmp1[[1]]
dat[(grep("hair transformaton, before+", dat$Category)),3] <- tmp1[[2]]
dat[(grep("polll", dat$Category)),3] <- tmp2
dat[grep("question.*?s", dat$Category), 3] <- tmp3

#unique levels and freqs
#table(sort(dat$Category))

#function to get the first level
first_level <- function(x) {
  
  return(strsplit(as.character(x), ",")[[1]][1])
  
}

first_level_table <- table(sapply(dat$Category, first_level))
first_level_table_data <- data.frame(first_level_table)
first_level_table_data

p <- plot_ly(first_level_table_data, x = ~Var1, y = ~Freq, type = 'bar', name = 'First Level of Categories')
p

# append first_level to dat dataframe
dat$cat_level_1 <- sapply(dat$Category, first_level)

#function to get the second level
second_level <- function(x, p) {
  
  str_box <- strsplit(as.character(x), ",")[[1]]
  
  if(length(str_box) > 1) {
    if(p == "nosplit")
      return(gsub("(?<=[\\s])\\s*|^\\s+|\\s+$","",str_box[2], perl = TRUE))
    else
      return(paste(trimws(str_box[1]), gsub("(?<=[\\s])\\s*|^\\s+|\\s+$","",str_box[2], perl = TRUE), sep = "::"))
  }
  else {
    
    return(str_box[1])
    
  }
  
}
second_level_table <- table(sapply(dat$Category, second_level, p = "split"))
second_level_table_data <- data.frame(second_level_table)
# append second_level to dat dataframe
dat$cat_level_2 <- sapply(dat[, "Category"], second_level, p = "notsplit")

#function to get the third level

third_level <- function(x, p = "nosplit") {
  
  str_box <- strsplit(as.character(x), ",")[[1]]
  
  if(length(str_box) > 2) {
    if(p == "nosplit")
      return(gsub("(?<=[\\s])\\s*|^\\s+|\\s+$","",str_box[2], perl = TRUE))
    else
      return(paste(trimws(str_box[1]), gsub("(?<=[\\s])\\s*|^\\s+|\\s+$","",str_box[2], perl = TRUE), gsub("(?<=[\\s])\\s*|^\\s+|\\s+$","",str_box[3], perl = TRUE), sep = "::"))
  }
  else {
    if(p = "nosplit")
      return(NA)
    else
      return(paste(trimws(str_box[1]), gsub("(?<=[\\s])\\s*|^\\s+|\\s+$","",str_box[2], perl = TRUE), sep = "::")) #could be NA
    
  }
  
}

dat$cat_level_3 <- sapply(dat[, "Category"], third_level, p = "split")

#function to get the fourth level
fourth_level <- function(x, p = "nosplit") {
  
  str_box <- strsplit(as.character(x), ",")[[1]]
  
  if(length(str_box) > 3) {
    if(p == "nosplit")
      return(gsub("(?<=[\\s])\\s*|^\\s+|\\s+$","",str_box[4], perl = TRUE))
    else
      return(paste(trimws(str_box[1]), gsub("(?<=[\\s])\\s*|^\\s+|\\s+$","",str_box[2], perl = TRUE), 
                   gsub("(?<=[\\s])\\s*|^\\s+|\\s+$","",str_box[3], perl = TRUE),
                   gsub("(?<=[\\s])\\s*|^\\s+|\\s+$","",str_box[4], perl = TRUE), sep = "::"))
    
  }
  else if(length(str_box) < 3) {
    if(p == "nosplit")
      return(NA)
    else
      return(paste(trimws(str_box[1]), gsub("(?<=[\\s])\\s*|^\\s+|\\s+$","",str_box[2], perl = TRUE), sep = "::"))
      
  }
  else {
    if (p == "nosplit")
      return(NA)
    else
      return(return(paste(trimws(str_box[1]), gsub("(?<=[\\s])\\s*|^\\s+|\\s+$","",str_box[2], perl = TRUE), 
                          gsub("(?<=[\\s])\\s*|^\\s+|\\s+$","",str_box[3], perl = TRUE), sep = "::")))
    
  }
  
}

dat$cat_level_4 <- sapply(dat[, "Category"], fourth_level, p = "split")

#function to get the fifth level
fifth_level <- function(x) {
  
  str_box <- strsplit(as.character(x), ",")[[1]]
  
  if(length(str_box) > 4) {
    
    return(gsub("(?<=[\\s])\\s*|^\\s+|\\s+$","",str_box[5], perl = TRUE))
    
  }
  else {
    
    return(NA)
    
  }
  
}

dat$cat_level_5 <- sapply(dat[, "Category"], fifth_level)

## Construct a tree
paste5 <- function(..., sep = " ", collapse = NULL, na.rm = F) {
  if (na.rm == F)
    paste(..., sep = sep, collapse = collapse)
  else
    if (na.rm == T) {
      paste.na <- function(x, sep) {
        x <- gsub("^\\s+|\\s+$", "", x)
        ret <- paste(na.omit(x), collapse = sep)
        is.na(ret) <- ret == ""
        return(ret)
      }
      df <- data.frame(..., stringsAsFactors = F)
      ret <- apply(df, 1, FUN = function(x) paste.na(x, sep))
      
      if (is.null(collapse))
        ret
      else {
        paste.na(ret, sep = collapse)
      }
    }
}
#dat$pathString <- paste("Categories", dat$cat_level_1, dat$cat_level_2, dat$cat_level_3, dat$cat_level_4, dat$cat_level_5, sep = "/")
dat$pathString <- paste5("Categories", dat$cat_level_1, dat$cat_level_2, dat$cat_level_3, dat$cat_level_4, sep = "/", na.rm = TRUE)

categories <- as.Node(dat)
print(categories, "Impressions")

SetGraphStyle(categories, rankdir = "LR")
plot(categories)

dat$count_dummy <- 1

# treemap
treemap(dat, index = c("cat_level_1", "cat_level_2", "cat_level_3", "cat_level_4"), vSize = "count_dummy", vColor = "Profile.Visits", type = "value")
treemap(dat, index = c("cat_level_1", "cat_level_2"), vSize = "count_dummy", vColor = "Replies", type = "value")


# group by
result <- dat %>% group_by(cat_level_1) %>% summarise(n = n(), mean_impressions  = mean(Impressions, na.rm = TRUE), 
                                            mean_viewers = mean(Viewers, na.rm = TRUE), mean_reach = mean(Reach, na.rm = TRUE), 
                                            mean_website = mean(Website.Clicks, na.rm = TRUE), mean_profile = mean(Profile.Visits, na.rm = TRUE),
                                            mean_replies = mean(Replies, na.rm = TRUE),
                                            mean_follows = mean(Follows, na.rm = TRUE))
result <- as.data.frame(result)
result

p <- plot_ly(dat, x = ~Date, y = ~Profile.Visits, type = "scatter", mode = "lines")
p

################################## BAR PLOTS #############################################

## Plot two or three levels of hierarchy for follows
tmp_df <- aggregate(Follows~cat_level_3, dat, sum)
tmp_df
p <-plot_ly(tmp_df, x = ~cat_level_3, y = ~Follows, type = 'bar')
p

## Plot two or three levels of hierarchy for website
tmp_df <- aggregate(Website.Clicks~cat_level_3, dat, sum)
tmp_df
p <-plot_ly(tmp_df, x = ~cat_level_3, y = ~Reach, type = 'bar')
p

## Plot two or three levels of hierarchy for Shares
tmp_df <- aggregate(Shares~cat_level_3, dat, sum)
tmp_df
p <-plot_ly(tmp_df, x = ~cat_level_3, y = ~Shares, type = 'bar')
p

## Plot two or three levels of hierarchy for Replies
tmp_df <- aggregate(Replies~cat_level_3, dat, sum)
tmp_df
p <-plot_ly(tmp_df, x = ~cat_level_3, y = ~Replies, type = 'bar')
p

## Plot two or three levels of hierarchy for Profile visits
tmp_df <- aggregate(Profile.Visits~cat_level_4, dat, sum)
tmp_df
p <-plot_ly(tmp_df, x = ~cat_level_4, y = ~Profile.Visits, type = 'bar')
p



