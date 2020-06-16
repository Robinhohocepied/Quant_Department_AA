# -----------------------------------------------------------------------------------------------------------------------------------    
# ---------------------------------------------------------------------------------------------------------------------------------    
# ------------------------------- Step 1 - Import of the data--------------------------------------------------------
# -----------------------------------------------------------------------------------------------------------------------------------
# ----------------------------------------------------------------------------------------------------------------------------------- 

# Pivot table used in excel to get the max Mcap of NSE & BSE
# Import of raw data from Ace Equity (Company data & Index data)

remove(list=ls())
dfx<-read.csv("C:/Users/robin.hocepied/Documents/databasis_r.csv",header=TRUE)
df_index<-read.csv("C:/Users/robin.hocepied/Documents/databasis_index_r.csv",header=TRUE)


dfx[dfx == 0] <- NA
df_index[df_index == 0] <- NA

df <- dfx[ which(dfx$X0 >100),] 

mcapx <- as.data.frame(c("Market cap"))

colnames(df)[3] <- "Market cap"

# Separate text values from numerical values and apply matrix function over numerical values    


m <- data.matrix(df[-(1:3)])
m_index <- t((as.data.frame(df_index[-(1:3)], stringsAsFactors=FALSE)))


# Duplicate table for a Vlookup process in the end of the code (After filter companies)

yy <- data.matrix(df[-(1:3)])
mm <- cbind(df[1:3], yy)


# Computation of the return percentage change over the data matrix (companies and index)

col=ncol(m)

new <- cbind(df[1:3], (m[,2:col] - m[,1:col-1])/m[,1:col-1])


neww <- as.data.frame(mm, stringsAsFactors=FALSE)

row=nrow(m_index)
col=ncol(m_index)

new_index <- as.data.frame(apply(m_index, 2, function(x) (x[2:row] - x[1:row-1])/x[1:row-1]))


# Tables used in the end of the code

new_indexxx <- as.data.frame(new_index,stringsAsFactors=FALSE)
new_indexxx <- as.data.frame(t(new_indexxx), stringsAsFactors=FALSE)


# -----------------------------------------------------------------------------------------------------------------------------------    
# ---------------------------------------------------------------------------------------------------------------------------------    
# ------------------------------ Step 2 - Clustering--------------------------------------------------------
# -----------------------------------------------------------------------------------------------------------------------------------
# ----------------------------------------------------------------------------------------------------------------------------------- 


# Clustering of the percentage change into a range of letters from A to C


breaks = c(-Inf,-0.075,0.075, Inf)
labels = rev(LETTERS[1:3])
f <- function(col) cut(col, breaks = breaks, labels = labels)
new <- data.frame(new[1:3], lapply(new[-c(1:3)], f))


# Clustering of the percentage change into a range of letters from A to F - 5% change

# breaks = c(-Inf,-0.1, -0.05, 0, 0.05, 0.10, Inf)
# labels = rev(LETTERS[1:6])
# f <- function(col) cut(col, breaks = breaks, labels = labels)
#new <- data.frame(new[1:3], lapply(new[-c(1:3)], f))


# Applying the above clustering function (Index) (f)    

new_index <- data.frame(new_index, lapply(new_index, f))
new_index <- (as.data.frame(t(new_index[-c(1)]))) 


# Sector filter (for one sector but can be applied for cross sectors - use  "chv")    

chr <- "Chemicals "

#chv <- "Chemicals "

# Creating 4 tables with the filtering - 4 time periods to be analysed

firstpair1 <- new[new[2] == chr,] 
firstpair2 <- new[new[2] == chr,] 
firstpair3 <- new[new[2] == chr,] 
firstpair4 <- new[new[2] == chr,] 

# For a cross sector correlation startegy use the beneath tables  

# secondpair1 <- new[new[2] == chv,] 
# secondpair2 <- new[new[2] == chv,] 
# secondpair3 <- new[new[2] == chv,] 
# secondpair4 <- new[new[2] == chv,] 

# new1 <- rbind(firstpair1, secondpair1 )
# new2 <- rbind(firstpair2, secondpair2 )
# new3 <- rbind(firstpair3, secondpair3 )
# new4 <- rbind(firstpair3, secondpair3 )



# Tables to be used for the different periods (4 diff. periods + 1 overall)

new1 <- firstpair1
new2 <- firstpair2
new3 <- firstpair3
new4 <- firstpair4


new_index1 <- new_index 
new_index2 <- new_index 
new_index3 <- new_index 
new_index4 <- new_index 



# 1st Date period filter from 2000-2006 ----------------------------------------------------------

# Zoo function - https://cran.r-project.org/web/packages/zoo/zoo.pdf

# Main data set filter for companies using function zoo (time series)

library(zoo);
sel.col <- function(start, end, new1) {
  dates <- as.yearmon(sub("X", "", colnames(new1[-c(1:3)])), "%Y%m");
  start <- as.yearmon(start, "%Y%m");
  end <- as.yearmon(end, "%Y%m");
  which(dates >= start & dates <= end) + 1;
}

# Insert the date period (in this case first period)

dat<-new1[, sel.col("200004", "200603", new1)]
dat<- as.data.frame(t(dat), stringsAsFactors = F)

# Main data set filter for Index

library(zoo);
sel.ind <- function(start, end, new_index) {
  dates <- as.yearmon(sub("X", "", colnames(new_index)[]), "%Y%m");
  start <- as.yearmon(start, "%Y%m");
  end <- as.yearmon(end, "%Y%m");
  which(dates >= start & dates <= end);
}

dat_index<-new_index[, sel.ind("200004", "200603", new_index)]
dat_index<- as.data.frame(dat_index)



# Correlation computation of all the companies within the sector 


# Two 'lapply function' to cross-over other columns over each column (Companies)

final_r <- lapply(dat, function(leftcomp) {
  lapply(dat, function(rightcomp) {
    mean(as.character(leftcomp) == as.character(rightcomp), na.rm = T)
  })})


# Unlist data to get n*n vector which has all the values in one row instead of a correlation mtx

results <- unlist(final_r)

# Some logic to collect the required elements only (delete diagonal matrix and duplicate values)

l <- length(dat)
a <- 1:(l*l)
b <- rep(seq(1,l*l,by = l+1),times = rep(l,times = l))

log_vec <- a > b

v <- results[log_vec]

v <- data.frame(v)

v <- t(v)



# Data frame adjustment - Company name (format) -Executed only once 

new1 <- t(new1)
data_withconame1 <- as.data.frame(new1[1,])
data_withconame1 <- t(data_withconame1)


# Data frame adjustment - Company name (format) - Executed only once 


data_withmcap <- as.data.frame(new1[3,])
data_withmcap <- t(data_withmcap)


# Names of the companies put together with the respective correlation - Executed only once 

newdf <- data.frame(some.colname = unlist(
  lapply(seq_along(data_withconame1)[-length(seq_along(data_withconame1))], function(x){
    y <- (x + 1):ncol(data_withconame1)
    paste0(data_withconame1[x], ' & ',data_withconame1[y])
  })
))



newdf <- t(newdf)
colnames(v) <- newdf[1, ]
colnames(dat) <- data_withconame1[1, ]


# MCap of the companies put together with the respective correlation 

# Two ways to do it, second choice selected 

# 1    
# mcapdf <- data.frame(some.colname = unlist(
#   lapply(seq_along(data_withmcap)[-length(seq_along(data_withmcap))], function(x){
#     y <- (x + 1):ncol(data_withmcap)
#     paste0(data_withmcap[x], ' & ',data_withmcap[y])
#   })
# ))

# 2     
mcapdf <- as.data.frame.matrix(t(combn(data_withmcap[1,],2)), stringsAsFactors = F)
mcapdf<- t(mcapdf)
colnames(mcapdf) <- newdf[1, ]



# Count True & False 

# True = number of times the pair shows the same letter /NA
# False = number of times the pair shows different letters /Na

# Two 'lapply function' to cross-over other columns over each column

countrue <- lapply(dat, function(leftcomp) {
  lapply(dat, function(rightcomp) {
    sum(as.character(leftcomp) == as.character(rightcomp), na.rm = T)
  })})
countfalse <- lapply(dat, function(leftcomp) {
  lapply(dat, function(rightcomp) {
    sum(as.character(leftcomp) != as.character(rightcomp), na.rm = T)
  })})

# Unlist data to get n*n vector which has all the values

truenb <- unlist(countrue)
falsenb <- unlist(countfalse)

t <- truenb[log_vec]
f <- falsenb[log_vec]

t <- data.frame(t) 
f <- data.frame(f)

t <- t(t)
f <- t(f)

colnames(t) <- newdf[1, ]
colnames(f) <- newdf[1, ]



dat[is.na(dat)] <- 0
dat_index <- as.data.frame(t(dat_index), stringsAsFactors = F)
dat_index[is.na(dat_index)] <- 0



# Computation of the number of combinations in which the pair of companies
# exhibites one of these two combinations A=A or C=C
# and the index exhibites the opposite sign ex: A=A & C or C=C & B

# Dlypr function - https://cran.r-project.org/web/packages/dplyr/dplyr.pdf

library(dplyr)


comp_func <- function(x, y, temp, index){
  temp <- bind_cols(temp[,!is.na(match(names(temp), c(x, y)))], index)
  temp[,] <- lapply(temp, function(i) as.character(i))
  ret <- sum(temp[,1] == temp[,2] & 
               temp[,1] %in% c('A', 'B', 'C') &
               ((temp[,1]=='A' & temp[,3]=='C') | (temp[,1]=='C' & temp[,3]=='A')| (temp[,1]=='A' & temp[,3]=='B')| (temp[,1]=='C' & temp[,3]=='B')))
  return(ret)
}

frame_df  <- as.data.frame.matrix(t(combn(names(dat),2)), stringsAsFactors = F)


index_df <- frame_df %>%
  rowwise() %>%
  mutate(val = comp_func(V1, V2, dat, dat_index))


index_df <- (t(index_df))

tfinal <- rbind(index_df, v,t,f, mcapdf)

tfinal <- data.frame(tfinal)


# Final table which will be printed in the pre-final excel table - 1st part 

tfinal <- t(tfinal)



# 2nd date period filter from 2006-2009 ----------------------------------------------------------    


# Repeating of the process for the 1st date period filter 

library(zoo);
sel.col1 <- function(start, end, new2) {
  dates <- as.yearmon(sub("X", "", colnames(new2)[-1]), "%Y%m");
  start <- as.yearmon(start, "%Y%m");
  end <- as.yearmon(end, "%Y%m");
  which(dates >= start & dates <= end) + 1;
}

dat1 <-new2[, sel.col1("200604", "200901", new2)]
dat1 <- as.data.frame(t(dat1), stringsAsFactors = F)


# Main data set filter for Index

library(zoo);
sel.ind1 <- function(start, end, new_index1) {
  dates <- as.yearmon(sub("X", "", colnames(new_index1)[]), "%Y%m");
  start <- as.yearmon(start, "%Y%m");
  end <- as.yearmon(end, "%Y%m");
  which(dates >= start & dates <= end);
}

dat_index1<-new_index1[, sel.ind1("200604", "200901", new_index1)]
dat_index1<- as.data.frame(dat_index1, stringsAsFactors = F)


# Correlation computation of all the companies within the sector 


# Two 'lapply function' to cross-over other columns over each column (Companies)    

final_r1 <- lapply(dat1, function(leftcomp) {
  lapply(dat1, function(rightcomp) {
    mean(as.character(leftcomp) == as.character(rightcomp), na.rm = T)
  })})

# Unlist data to get n*n vector which has all the values in one row instead of a correlation mtx

results1 <- unlist(final_r1)

# Some logic to collect the required elements only (delete diagonal matrix and duplicate values)

l <- length(dat1)
a <- 1:(l*l)
b <- rep(seq(1,l*l,by = l+1),times = rep(l,times = l))

log_vec1 <- a > b


v1 <- results1[log_vec1]

v1 <- data.frame(v1)

v1 <- t(v1)

colnames(v1) <- newdf[1, ]


# Count True & False 

# True = number of times the pair shows the same letter /NA
# False = number of times the pair shows different letters /Na

# Two 'lapply function' to cross-over other columns over each column

countrue1 <- lapply(dat1, function(leftcomp) {
  lapply(dat1, function(rightcomp) {
    sum(as.character(leftcomp) == as.character(rightcomp), na.rm = T)
  })})
countfalse1 <- lapply(dat1, function(leftcomp) {
  lapply(dat1, function(rightcomp) {
    sum(as.character(leftcomp) != as.character(rightcomp), na.rm = T)
  })})

# Unlist data to get n*n vector which has all the values in one row instead of a correlation mtx

truenb1 <- unlist(countrue1)
falsenb1 <- unlist(countfalse1)

t1 <- truenb1[log_vec]
f1 <- falsenb1[log_vec]


t1 <- data.frame(t1) 
f1 <- data.frame(f1)

t1 <- t(t1)
f1 <- t(f1)

colnames(t1) <- newdf[1, ]
colnames(f1) <- newdf[1, ]


dat1[is.na(dat1)] <- 0

dat_index1 <- as.data.frame(t(dat_index1), stringsAsFactors = F)
dat_index1[is.na(dat_index1)] <- 0


# Computation of the number of combinations in which the pair of companies
# exhibites one of these two combinations A=A or C=C
# and the index exhibites the opposite sign ex: A=A & C or C=C & B

# Dlypr function - https://cran.r-project.org/web/packages/dplyr/dplyr.pdf

library(dplyr)

comp_func1 <- function(x, y, temp, index){
  temp <- bind_cols(temp[,!is.na(match(names(temp), c(x, y)))], index)
  temp[,] <- lapply(temp, function(i) as.character(i))
  ret <- sum(temp[,1] == temp[,2] & 
               temp[,1] %in% c('A', 'B', 'C') &
               ((temp[,1]=='A' & temp[,3]=='C') | (temp[,1]=='C' & temp[,3]=='A')| (temp[,1]=='A' & temp[,3]=='B')| (temp[,1]=='C' & temp[,3]=='B')))
  return(ret)
}

frame_df1  <- as.data.frame.matrix(t(combn(names(dat1),2)), stringsAsFactors = F)


index_df1 <- frame_df1 %>%
  rowwise() %>%
  mutate(val = comp_func1(V1, V2, dat1, dat_index1))

index_df1 <- t(index_df1)

tfinal1 <- rbind(index_df1, v1,t1,f1, mcapdf)

tfinal1 <- data.frame(tfinal1)

colnames(tfinal1) <- newdf[1, ]

# Final table which will be printed in the final excel table - 2nd part

tfinal1 <- t(tfinal1)





# 3rd date period filter from 2009-2014 ----------------------------------------------------------        




# Rerun of the process computed for the 1st date period filter 

library(zoo);
sel.col2 <- function(start, end, new3) {
  dates <- as.yearmon(sub("X", "", colnames(new3)[-1]), "%Y%m");
  start <- as.yearmon(start, "%Y%m");
  end <- as.yearmon(end, "%Y%m");
  which(dates >= start & dates <= end) + 1;
}

dat2<-new3[, sel.col2("200903", "201401", new3)]
dat2<- as.data.frame(t(dat2), stringsAsFactors = F)

# Main data set filter for Index

library(zoo);
sel.ind2 <- function(start, end, new_index2) {
  dates <- as.yearmon(sub("X", "", colnames(new_index2)[]), "%Y%m");
  start <- as.yearmon(start, "%Y%m");
  end <- as.yearmon(end, "%Y%m");
  which(dates >= start & dates <= end);
}

dat_index2<-new_index2[, sel.ind2("200903", "201401", new_index2)]
dat_index2<- as.data.frame(dat_index2, stringsAsFactors = F)


# Correlation computation of all the companies within the sector 


# Two 'lapply function' to cross-over other columns over each column (Companies)   


final_r2 <- lapply(dat2, function(leftcomp) {
  lapply(dat2, function(rightcomp) {
    mean(as.character(leftcomp) == as.character(rightcomp), na.rm = T)
  })})

# Unlist data to get n*n vector which has all the values in one row instead of a correlation mtx

results2 <- unlist(final_r2)

# Some logic to collect the required elements only (delete diagonal matrix and duplicate values)

l <- length(dat2)
a <- 1:(l*l)
b <- rep(seq(1,l*l,by = l+1),times = rep(l,times = l))

log_vec2 <- a > b


v2 <- results2[log_vec2]


v2 <- data.frame(v2)

v2 <- t(v2)

colnames(v2) <- newdf[1, ]


# Count True & False 

# True = number of times the pair shows the same letter /NA
# False = number of times the pair shows different letters /Na

# Two 'lapply function' to cross-over other columns over each column

countrue2 <- lapply(dat2, function(leftcomp) {
  lapply(dat2, function(rightcomp) {
    sum(as.character(leftcomp) == as.character(rightcomp), na.rm = T)
  })})
countfalse2 <- lapply(dat2, function(leftcomp) {
  lapply(dat2, function(rightcomp) {
    sum(as.character(leftcomp) != as.character(rightcomp), na.rm = T)
  })})

# Unlist data to get n*n vector which has all the values in one row instead of a correlation mtx

falsenb2 <- unlist(countfalse2)
truenb2 <- unlist(countrue2)

t2 <- truenb2[log_vec]
f2 <- falsenb2[log_vec]


t2 <- data.frame(t2) 
f2 <- data.frame(f2)

t2 <- t(t2)
f2 <- t(f2)

colnames(t2) <- newdf[1, ]
colnames(f2) <- newdf[1, ]

dat2[is.na(dat2)] <- 0

dat_index2 <- as.data.frame(t(dat_index2), stringsAsFactors = F)
dat_index2[is.na(dat_index2)] <- 0

# Computation of the number of combinations in which the pair of companies
# exhibites one of these two combinations A=A or C=C
# and the index exhibites the opposite sign ex: A=A & C or C=C & B

# Dlypr function - https://cran.r-project.org/web/packages/dplyr/dplyr.pdf

library(dplyr)

comp_func2 <- function(x, y, temp, index){
  temp <- bind_cols(temp[,!is.na(match(names(temp), c(x, y)))], index)
  temp[,] <- lapply(temp, function(i) as.character(i))
  ret <- sum(temp[,1] == temp[,2] & 
               temp[,1] %in% c('A', 'B', 'C') &
               ((temp[,1]=='A' & temp[,3]=='C') | (temp[,1]=='C' & temp[,3]=='A')| (temp[,1]=='A' & temp[,3]=='B')| (temp[,1]=='C' & temp[,3]=='B')))
  return(ret)
}

frame_df2  <- as.data.frame.matrix(t(combn(names(dat2),2)), stringsAsFactors = F)


index_df2 <- frame_df2 %>%
  rowwise() %>%
  mutate(val = comp_func2(V1, V2, dat2, dat_index2))


index_df2 <- t(index_df2)

tfinal2 <- rbind(index_df2, v2,t2,f2, mcapdf)

tfinal2 <- data.frame(tfinal2)

colnames(tfinal2) <- newdf[1, ]

# Final table which will be printed in the final excel table - 3rd part  

tfinal2 <- t(tfinal2)



# 4th date period filter from 2014-2018 ----------------------------------------------------------       



# Rerun of the process computed for the 1st date period filter

library(zoo);
sel.col3 <- function(start, end, new4) {
  dates <- as.yearmon(sub("X", "", colnames(new4)[-1]), "%Y%m");
  start <- as.yearmon(start, "%Y%m");
  end <- as.yearmon(end, "%Y%m");
  which(dates >= start & dates <= end) + 1;
}

dat3<-new4[, sel.col3("201401", "201804", new4)]
dat3<- as.data.frame(t(dat3), stringsAsFactors = F)

# Main data set filter for Index

library(zoo);
sel.ind3 <- function(start, end, new_index3) {
  dates <- as.yearmon(sub("X", "", colnames(new_index3)[]), "%Y%m");
  start <- as.yearmon(start, "%Y%m");
  end <- as.yearmon(end, "%Y%m");
  which(dates >= start & dates <= end);
}

dat_index3<-new_index3[, sel.ind3("201401", "201804", new_index3)]
dat_index3<- as.data.frame(dat_index3, stringsAsFactors = F)

# Correlation computation of all the companies within the sector 


# Two 'lapply function' to cross-over other columns over each column (Companies)

final_r3 <- lapply(dat3, function(leftcomp) {
  lapply(dat3, function(rightcomp) {
    mean(as.character(leftcomp) == as.character(rightcomp), na.rm = T)
  })})

# Unlist data to get n*n vector which has all the values in one row instead of a correlation mtx   

results3 <- unlist(final_r3)

# Some logic to collect the required elements only (delete diagonal matrix and duplicate values)

l <- length(dat3)
a <- 1:(l*l)
b <- rep(seq(1,l*l,by = l+1),times = rep(l,times = l))

log_vec3 <- a > b


v3 <- results3[log_vec3]

v3 <- data.frame(v3)

v3 <- t(v3)

colnames(v3) <- newdf[1, ]



# Count True & False 

# True = number of times the pair shows the same letter /NA
# False = number of times the pair shows different letters /Na

# Two 'lapply function' to cross-over other columns over each column

countrue3 <- lapply(dat3, function(leftcomp) {
  lapply(dat3, function(rightcomp) {
    sum(as.character(leftcomp) == as.character(rightcomp), na.rm = T)
  })})
countfalse3 <- lapply(dat3, function(leftcomp) {
  lapply(dat3, function(rightcomp) {
    sum(as.character(leftcomp) != as.character(rightcomp), na.rm = T)
  })})

# Unlist data to get n*n vector which has all the values in one row instead of a correlation mtx

truenb3 <- unlist(countrue3)
falsenb3 <- unlist(countfalse3)

t3 <- truenb3[log_vec]
f3 <- falsenb3[log_vec]


t3 <- data.frame(t3) 
f3 <- data.frame(f3)

t3 <- t(t3)
f3 <- t(f3)

colnames(t3) <- newdf[1, ]
colnames(f3) <- newdf[1, ]

dat3[is.na(dat3)] <- 0

dat_index3 <- as.data.frame(t(dat_index3), stringsAsFactors = F)
dat_index3[is.na(dat_index3)] <- 0


# Computation of the number of combinations in which the pair of companies
# exhibites one of these two combinations A=A or C=C
# and the index exhibites the opposite sign ex: A=A & C or C=C & B

# Dlypr function - https://cran.r-project.org/web/packages/dplyr/dplyr.pdf

library(dplyr)

comp_func3 <- function(x, y, temp, index){
  temp <- bind_cols(temp[,!is.na(match(names(temp), c(x, y)))], index)
  temp[,] <- lapply(temp, function(i) as.character(i))
  ret <- sum(temp[,1] == temp[,2] & 
               temp[,1] %in% c('A', 'B', 'C') &
               ((temp[,1]=='A' & temp[,3]=='C') | (temp[,1]=='C' & temp[,3]=='A')| (temp[,1]=='A' & temp[,3]=='B')| (temp[,1]=='C' & temp[,3]=='B')))
  return(ret)
}

frame_df3  <- as.data.frame.matrix(t(combn(names(dat3),2)), stringsAsFactors = F)


index_df3 <- frame_df3 %>%
  rowwise() %>%
  mutate(val = comp_func3(V1, V2, dat3, dat_index3))


index_df3 <- t(index_df3)


tfinal3 <- rbind(index_df3, v3,t3,f3, mcapdf)

tfinal3 <- data.frame(tfinal3)

colnames(tfinal3) <- newdf[1, ]

# Final table which will be printed in the final excel table - 4th part

tfinal3 <- t(tfinal3)





# Overall date period filter from 2000-2018 ----------------------------------------------------------       




# Repeating of the process for the 1st date period filter

library(zoo);
sel.col4 <- function(start, end, new5) {
  dates <- as.yearmon(sub("X", "", colnames(new5)[-1]), "%Y%m");
  start <- as.yearmon(start, "%Y%m");
  end <- as.yearmon(end, "%Y%m");
  which(dates >= start & dates <= end) + 1;
}

dat4<-new4[, sel.col4("20004", "201804", new4)]
dat4<- as.data.frame(t(dat4), stringsAsFactors = F)

# Main data set filter for Index

library(zoo);
sel.ind4 <- function(start, end, new_index4) {
  dates <- as.yearmon(sub("X", "", colnames(new_index4)[]), "%Y%m");
  start <- as.yearmon(start, "%Y%m");
  end <- as.yearmon(end, "%Y%m");
  which(dates >= start & dates <= end);
}

dat_index4<-new_index4[, sel.ind4("20004", "201804", new_index4)]
dat_index4<- as.data.frame(dat_index4, stringsAsFactors = F)


# Correlation computation of all the companies within the sector 


# Two 'lapply function' to cross-over other columns over each column (Companies)

final_r4 <- lapply(dat4, function(leftcomp) {
  lapply(dat4, function(rightcomp) {
    mean(as.character(leftcomp) == as.character(rightcomp), na.rm = T)
  })})

# Unlist data to get n*n vector which has all the values in one row instead of a correlation mtx

results4 <- unlist(final_r4)

# Some logic to collect the required elements only (delete diagonal matrix and duplicate values)

l <- length(dat4)
a <- 1:(l*l)
b <- rep(seq(1,l*l,by = l+1),times = rep(l,times = l))

log_vec4 <- a > b


v4 <- results4[log_vec4]

v4 <- data.frame(v4)

v4 <- t(v4)

colnames(v4) <- newdf[1, ]



# Count True & False 

# True = number of times the pair shows the same letter /NA
# False = number of times the pair shows different letters /Na

# Two 'lapply function' to cross-over other columns over each column

countrue4 <- lapply(dat4, function(leftcomp) {
  lapply(dat4, function(rightcomp) {
    sum(as.character(leftcomp) == as.character(rightcomp), na.rm = T)
  })})
countfalse4 <- lapply(dat4, function(leftcomp) {
  lapply(dat4, function(rightcomp) {
    sum(as.character(leftcomp) != as.character(rightcomp), na.rm = T)
  })})

# Unlist data to get n*n vector which has all the values in one row instead of a correlation mtx

truenb4 <- unlist(countrue4)
falsenb4 <- unlist(countfalse4)

t4 <- truenb4[log_vec]
f4 <- falsenb4[log_vec]


t4 <- data.frame(t4) 
f4 <- data.frame(f4)

t4 <- t(t4)
f4 <- t(f4)

colnames(t4) <- newdf[1, ]
colnames(f4) <- newdf[1, ]


dat4[is.na(dat4)] <- 0

dat_index4 <- as.data.frame(t(dat_index4), stringsAsFactors = F)
dat_index4[is.na(dat_index4)] <- 0

# Computation of the number of combinations in which the pair of companies
# exhibites one of these two combinations A=A or C=C
# and the index exhibites the opposite sign ex: A=A & C or C=C & B

# Dlypr function - https://cran.r-project.org/web/packages/dplyr/dplyr.pdf

library(dplyr)

comp_func4 <- function(x, y, temp, index){
  temp <- bind_cols(temp[,!is.na(match(names(temp), c(x, y)))], index)
  temp[,] <- lapply(temp, function(i) as.character(i))
  ret <- sum(temp[,1] == temp[,2] & 
               temp[,1] %in% c('A', 'B', 'C') &
               ((temp[,1]=='A' & temp[,3]=='C') | (temp[,1]=='C' & temp[,3]=='A')| (temp[,1]=='A' & temp[,3]=='B')| (temp[,1]=='C' & temp[,3]=='B')))
  return(ret)
}



frame_df4  <- as.data.frame.matrix(t(combn(names(dat4),2)), stringsAsFactors = F)


index_df4 <- frame_df4 %>%
  rowwise() %>%
  mutate(val = comp_func4(V1, V2, dat4, dat_index4))


both1 <- cbind(dat4, dat_index4)

index_df4 <- t(index_df4)


tfinal4 <- rbind(index_df4,v4,t4,f4, mcapdf)

tfinal4 <- data.frame(tfinal4)

colnames(tfinal4) <- newdf[1, ]



# Final table which will be printed in the final excel table - Overall part 

tfinal4 <- t(tfinal4)


finaltablexcel <- as.data.frame(cbind(tfinal,tfinal1,tfinal2,tfinal3,tfinal4), stringsAsFactors = F)



# 1st Part results: Wise-pair of companies(C1 & C2) + Nb of times moving against the index (MA1)
# + Correlation of the pair (Corre1)+ Mcap1 + Mcap 2 +  True (T1) & Falses (F1) 

#write.csv(finaltablexcel, file = "Chemicals.csv")



# -----------------------------------------------------------------------------------------------------------------------------------    
# ---------------------------------------------------------------------------------------------------------------------------------    
# -----------------------------Step 3 - Ranking and filtering--------------------------------------------------------
# -----------------------------------------------------------------------------------------------------------------------------------
# ----------------------------------------------------------------------------------------------------------------------------------- 


# 1st condition: Market cap. >100 (already executed in the first lines of the code)

# 2nd condition: Rank companies in terms of correlation in period 3 & 4 (ideally top 10)



finaltablexcel[finaltablexcel=="NaN"]<-0



# Rename the column names to correlctly subset the data

colnames(finaltablexcel)<- c("C1","C2","MAI1","Correlation1",	"T1",	"F1",	"Mcap11",	"Mcap21",	"NA11",	"NA21",	"MAI2",	"Correlation2",	"T2",	"F2",	"Mcap12",	"Mcap22",	"NA12",	"NA22",	"MAI3",	"X3",	"T3",	"F3",	"Mcap13",	"Mcap23",	"NA13",	"NA23",	"MAI4",	"X4",	"T4",	"F4",	"Mcap14",	"Mcap24",	"NA14",	"NA24",	"MAI5","Correlation5",	"T5",	"F5","Mcap15"	,"Mcap25")




# Correlation filters to answer the second condition


# 1st - Period 2009 to 2014     

finaltablexcelmcap <- subset(finaltablexcel, X3 > 0.5)


# 2nd - Period 2014 to 2018

finaltablexcel1 <- subset(finaltablexcelmcap, X4 > 0.5)    


# Extracting companies names to then remove duplicates

fin1 <- data.frame(finaltablexcel1$C1)

fin2 <- data.frame(finaltablexcel1$C2)

colnames(fin1)=c("Company.Name")

colnames(fin2)=c("Company.Name")


finfin1 <- as.data.frame(rbind(fin1, fin2),stringsAsFactors = F)

# Ranking companies by the number of times they show up 

ascending <- as.data.frame(finfin1[order(finfin1$Company.Name, decreasing = TRUE), ])

colnames(ascending) <- "Company.Name"

# ranky1 <- as.data.frame(count(ascending, "Company.Name"))

ranky1 <- as.data.frame(table(ascending))

ranky <- ranky1[
  with(ranky1, order(-Freq)),
  ]

# Filter the number of times the company has shown 

colnames(ranky)[1] <- "Company.Name"


# Selecting the top 10 companies

ranky <- ranky[1:10,]

# Companies names that have shown at least twice in the correlations 

ranky <- subset(ranky, Freq > 2)

ranky <- ranky[-c(2)]

colnames(ranky)[1] <- "Company.Name"



# VLOOKUP - Joint of the filtered companies with the initial returns databasis

last_df <- as.data.frame(merge(x = ranky, y = neww, all.x = TRUE), stringsAsFactors=FALSE)



#FINAL TABLE - TO be used in step 4 and 5

last_df <- last_df[-c(2,3)]

###########################################################################################
#######FINAL table w/ the companies that supposedly move in tandem within each sector #########
###########################################################################################

write.csv(last_df , file = "Agriculture.csv")





# -----------------------------------------------------------------------------------------------------------------------------------    
# ---------------------------------------------------------------------------------------------------------------------------------    
# ---------------------- Step 3.1- Part of the code - Graphs (not mandatory)---------------------------------------------------------
# -----------------------------------------------------------------------------------------------------------------------------------
# ----------------------------------------------------------------------------------------------------------------------------------- 



# Sum of all the companies returns (SUM)


last_b_graph1 <- as.data.frame(lapply(last_df[-(1:3)], function(x) if(is.numeric(x)) sum(x, na.rm=T)),stringsAsFactors=FALSE)


coly=ncol(last_b_graph1)

# Absolute returns of the companies (same formula used in the beginning of the code)

screenerdf1 <-  as.data.frame(((last_b_graph1[,2:coly] - last_b_graph1[,1:coly-1])/last_b_graph1[,1:coly-1]), stringsAsFactors=FALSE)

new_indexxx[is.na(new_indexxx)] <- 0

# Date period screening - Taking into account the  correlation filters in line 931 &936

# Company filter 

library(zoo);
sel.co <- function(start, end, screenerdf1) {
  dates <- as.yearmon(sub("X", "", colnames(screenerdf1)[]), "%Y%m");
  start <- as.yearmon(start, "%Y%m");
  end <- as.yearmon(end, "%Y%m");
  which(dates >= start & dates <= end);
}

#screenerdf<-screenerdf1[, sel.co("200603", "201804", screenerdf1)]
screenerdf<-screenerdf1[, sel.co("200903", "201804", screenerdf1)]
screenerdf<- as.data.frame(screenerdf)

# Index filter

library(zoo);
sel.in <- function(start, end, new_indexxx) {
  dates <- as.yearmon(sub("X", "", colnames(new_indexxx)[]), "%Y%m");
  start <- as.yearmon(start, "%Y%m");
  end <- as.yearmon(end, "%Y%m");
  which(dates >= start & dates <= end);
}


#new_indexx <-new_indexxx[, sel.in("200603", "201804", new_indexxx)]
new_indexx <-new_indexxx[, sel.in("200903", "201804", new_indexxx)]


new_indexx<- as.data.frame(new_indexx)




nub_lastdf <- nrow(last_df)

values_a_above <- as.data.frame(screenerdf[,new_indexx > 0.075 ])
values_b_between <- as.data.frame(screenerdf[,new_indexx >= -0.075 & new_indexx <= 0.075 ])
values_C_below <- as.data.frame(screenerdf[,new_indexx < -0.075 ])


values_above_a1 <- as.data.frame(values_a_above[, values_a_above > 0.075 ])
values_between_a2 <- as.data.frame(values_a_above[,values_a_above >= -0.075 & values_a_above <= 0.075 ])
values_below_a3 <- as.data.frame(values_a_above[,values_a_above < -0.075 ])

values_above_b1 <- as.data.frame(values_b_between[, values_b_between > 0.075 ])
values_between_b2 <- as.data.frame(values_b_between[,values_b_between >= -0.075 & values_b_between <= 0.075 ])
values_below_b3 <- as.data.frame(values_b_between[,values_b_between < -0.075 ])

values_above_c1 <- as.data.frame(values_C_below[, values_C_below > 0.075 ])
values_between_c2 <- as.data.frame(values_C_below[,values_C_below >= -0.075 & values_C_below <= 0.075 ])
values_below_c3 <- as.data.frame(values_C_below[,values_C_below < -0.075 ])


aaa <- ncol(values_a_above)

a1_c <- ncol(values_above_a1)
a2_c <- ncol(values_between_a2)
a3_c <- ncol(values_below_a3)

column_vectora <- as.data.frame(c(a3_c,a2_c, a1_c))


final_over_7.5a <- as.data.frame(column_vectora[, 1:ncol(column_vectora)]/ncol(values_a_above))

Last_over_7.5a <- unlist(final_over_7.5a)


bbb <- ncol(values_b_between)

b1_c <- ncol(values_above_b1)
b2_c <- ncol(values_between_b2)
b3_c <- ncol(values_below_b3)

column_vectorb <- as.data.frame(c(b3_c,b2_c, b1_c))


final_over_7.5b <- as.data.frame(column_vectorb[, 1:ncol(column_vectorb)]/ncol(values_b_between))

Last_over_7.5b <- unlist(final_over_7.5b)


ccc <- ncol(values_C_below)


c1_c <- ncol(values_above_c1)
c2_c <- ncol(values_between_c2)
c3_c <- ncol(values_below_c3)

column_vectorc <- as.data.frame(c(c3_c,c2_c,c1_c))


final_over_7.5c <- as.data.frame(column_vectorc[, 1:ncol(column_vectorc)]/ncol(values_C_below))

Last_over_7.5c <- unlist(final_over_7.5c)

c <- t(c(ccc, bbb, aaa, nub_lastdf))
cc <-c(ccc, bbb, aaa)



distribution_below_7.5 <- as.data.frame(lapply(Last_over_7.5c, function(x) Last_over_7.5c *aaa))
distribution_between_7.5 <- as.data.frame(lapply(Last_over_7.5b, function(x) Last_over_7.5b *bbb))
distribution_above_7.5 <- as.data.frame(lapply(Last_over_7.5a, function(x) Last_over_7.5a *ccc))

distribution_below_7.5 <- format(round(distribution_below_7.5, 0), nsmall = 2)
distribution_between_7.5 <- format(round(distribution_between_7.5, 0), nsmall = 2)
distribution_above_7.5 <- format(round(distribution_above_7.5, 0), nsmall = 2)

distribution_below_7.5 <- distribution_below_7.5[-(1:2)]
distribution_between_7.5 <- distribution_between_7.5[-(1:2)]
distribution_above_7.5 <- distribution_above_7.5[-(1:2)]


distribution_below_7.5 <- as.integer(unlist(distribution_below_7.5))
distribution_between_7.5 <- as.integer(unlist(distribution_between_7.5))
distribution_above_7.5 <- as.integer(unlist(distribution_above_7.5))

distribution_below_7.5 <- t(distribution_below_7.5)
distribution_between_7.5 <- t(distribution_between_7.5)
distribution_above_7.5 <- t(distribution_above_7.5)

db <- distribution_below_7.5
dbe <- distribution_between_7.5
da <- distribution_above_7.5


par(mfrow=c(2,2)) 

xx <- barplot(c, main="Number of periods for each Probability & Number of tradeable companies", 
              names.arg = c("Below -7.5","Between -7.5% and 7.5%","Above 7.5%", "Number of companies"),col = "darkgoldenrod1", ylab = "Count" )

text(x = xx, y = c, label = c, pos = 3 ,cex = 0.9, col = "black")

qq <- barplot(distribution_above_7.5, main="Probability Distribution  when Nifty50 is below 7.5% ", 
              names.arg = c("Below -7.5","Between -7.5% and 7.5%","Above 7.5%"),col = "grey", ylab = "Count" )
text(x = qq, y = da, label = da, pos = 3 ,cex = 0.9, col = "black")


zz <- barplot(distribution_between_7.5, main="Probability Distribution  when Nifty50 is between -7.5% and 7.5%", 
              names.arg = c("Below -7.5","Between -7.5% and 7.5%","Above 7.5%"),col = "grey", ylab = "Count" )

text(x = zz, y = dbe, label = dbe, pos = 3 ,cex = 0.9, col = "black")

yy <- barplot(distribution_below_7.5, main="Probability Distribution  when Nifty50 is over -7.5%", 
              names.arg = c("Below -7.5","Between -7.5% and 7.5%","Above 7.5%"),col = "grey", ylab = "Count" )

text(x = yy, y = db, label = db, pos = 3 ,cex = 0.9, col = "black")







