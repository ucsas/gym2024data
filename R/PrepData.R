library(pdftools)
library(tabulizer)
library(tidyverse)
library(stringr)
library(plyr)

## International 

cat('\nint_team\n')
all <- lapply(dir("int_team", full.names = TRUE), extract_tables, method='decide',
              guess=FALSE, lattice=TRUE, pages=1, area=list(c(200.03604, 93.75013
                                                              , 310.94567, 
                                                              480.88480)))
names(all) <- gsub(".pdf", "", dir("int_team"))
is.list(all)
names(all)

comps <- list()

for (i in 1:length(all)) {
  cat(i,'', names(all)[i], '\n')
  
  d <- all[[i]][[1]]
  if (d[1,1] == "USA - United States of America") {
    d <- d[2:11,]
  }
  for (j in seq(1, dim(d)[1], 2)) {
    name <- gsub("[0-9]+ ", "", d[j, 1])
    name <- gsub("M[Cc] ", "MC", name)
    year <- gsub("_.*", "", names(all)[i])
    type <- "INT"
    competition <- "team"
    qual <- FALSE
    if (grepl("_", names(all)[i], fixed=TRUE)) {
      qual <- TRUE
    }
    VT1_d <- strsplit(d[j, 3], " ")[[1]][1]
    VT1_e <- strsplit(d[j+1, 3], " ")[[1]][1]
    VT1_tot <- strsplit(d[j, 3], " ")[[1]][2]
    VT2_d <- NA
    VT2_e <- NA
    VT2_tot <- NA
    VT_tot <- NA
    UE_d <- strsplit(d[j, 4], " ")[[1]][1]
    UE_e <- strsplit(d[j+1, 4], " ")[[1]][1]
    UE_tot <- strsplit(d[j, 4], " ")[[1]][2]
    BB_d <- strsplit(d[j, 5], " ")[[1]][1]
    BB_e <- strsplit(d[j+1, 5], " ")[[1]][1]
    BB_tot <- strsplit(d[j, 5], " ")[[1]][2]
    FX_d <- strsplit(d[j, 6], " ")[[1]][1]
    FX_e <- strsplit(d[j+1, 6], " ")[[1]][1]
    FX_tot <- strsplit(d[j, 6], " ")[[1]][2]
    comps[[length(comps)+1]] <- data.frame(name, year, type, competition, qual, VT1_d, VT1_e, 
                             VT1_tot, VT2_d, VT2_e, VT2_tot, VT_tot, UE_d, UE_e,
                             UE_tot, BB_d, BB_e, BB_tot, FX_d, FX_e, FX_tot)
  }
}

scores <- do.call(rbind, comps)

cat('\nint_events\n')
all <- lapply(dir("int_events", full.names = TRUE), pdf_data)
names(all) <- gsub("TYPE4_", "", gsub(".pdf", "", dir("int_events")))
is.list(all)
names(all)

comps <- list()

for (i in 1:length(all)) {
  cat(i,'', names(all)[i], '\n')
  
  year <- gsub("_.*", "", names(all)[i])
  type <- "INT"
  d <- all[[i]]
  qual <- FALSE
  if (grepl("_", names(all)[i], fixed=TRUE)) {
    qual <- TRUE
  }
  for (j in 1:length(d)) {
    page <- d[[j]]
    y_vals <- sort(unique(page$y))
    USA_gymnasts <- unique(page[which(page$text == "USA"),]$y)
    if (length(USA_gymnasts) == 0) {
      break
    }
    for (k in 1:length(USA_gymnasts)) {
      gymnast <- page[which(page$y == USA_gymnasts[k]),]
      gymnast <- gymnast[order(gymnast$x),]
      if (gymnast[3,]$text == "Mc") {
        name <- paste(toupper(paste(gymnast[3,]$text, gymnast[4,]$text, sep="")),
                      gymnast[5,]$text, sep=" ")
        gymnast <- gymnast[-3,]
      }
      else {
        name <- paste(gymnast[3,]$text, gymnast[4,]$text, sep=" ")
      }
      VT1_d <- NA
      VT1_e <- NA
      VT1_tot <- NA
      VT2_d <- NA
      VT2_e <- NA
      VT2_tot <- NA
      VT_tot <- NA
      UE_d <- NA
      UE_e <- NA
      UE_tot <- NA
      BB_d <- NA
      BB_e <- NA
      BB_tot <- NA
      FX_d <- NA
      FX_e <- NA
      FX_tot <- NA
      if ("Vault" %in% page$text) {
        competition <- "VT"
        VT1_d <- as.numeric(gymnast[7,6])
        VT1_e <- as.numeric(gymnast[8,6])
        VT1_tot <- as.numeric(gymnast[9,6])
        VT2_index <- match(USA_gymnasts[k], y_vals) + 1
        VT2_d <- as.numeric(page[which(page$y == y_vals[VT2_index]),][2,6])
        VT2_e <- as.numeric(page[which(page$y == y_vals[VT2_index]),][3,6])
        VT2_tot <- as.numeric(page[which(page$y == y_vals[VT2_index]),][4,6])
        if (VT2_tot < 0) {
          VT2_tot <- as.numeric(page[which(page$y == y_vals[VT2_index]),][6,6])
        }
        VT_tot <- as.numeric(gymnast[10, 6])
        if (VT1_tot < 0) {
          VT1_tot <- as.numeric(gymnast[10,6])
          VT_tot <- as.numeric(gymnast[11,6])
        }
      }
      else if ("Uneven" %in% page$text) {
        competition <- "UE"
        UE_d <- as.numeric(gymnast[6,6])
        UE_e <- as.numeric(gymnast[7,6])
        UE_tot <- as.numeric(gymnast[8,6])
        if (UE_tot < 0) {
          UE_tot <- as.numeric(gymnast[9,6])
        }
      }
      else if ("Balance" %in% page$text) {
        competition <- "BB"
        BB_d <- as.numeric(gymnast[6,6])
        BB_e <- as.numeric(gymnast[7,6])
        BB_tot <- as.numeric(gymnast[8,6])
        if (BB_tot < 0) {
          BB_tot <- as.numeric(gymnast[9,6])
        }
        
      }
      else if ("Floor" %in% page$text) {
        competition <- "FX"
        FX_d <- as.numeric(gymnast[6,6])
        FX_e <- as.numeric(gymnast[7,6])
        FX_tot <- as.numeric(gymnast[8,6])
        if (FX_tot < 0) {
          FX_tot <- as.numeric(gymnast[9,6])
        }
      }
      comps[[length(comps)+1]] <- data.frame(name, year, type, competition, qual,
                                             VT1_d, VT1_e, VT1_tot, VT2_d, VT2_e,
                                             VT2_tot, VT_tot, UE_d, UE_e,
                                             UE_tot, BB_d, BB_e, BB_tot, FX_d, 
                                             FX_e, FX_tot)
    }
  }
}

scores <- rbind(scores, do.call(rbind, comps))

cat('\nint_aa\n')
all <- lapply(dir("int_aa", full.names = TRUE), extract_tables, lattice=TRUE)
names(all) <- gsub("TYPE5_", "", gsub(".pdf", "", dir("int_aa")))
is.list(all)
names(all)

comps <- list()

for (i in 1:length(all)) {
  cat(i,'', names(all)[i], '\n')
  
  year <- year <- gsub("_.*", "", names(all)[i])
  type <- "INT"
  competition <- "AA"
  qual <- FALSE
  if (grepl("_", names(all)[i], fixed=TRUE)) {
    qual <- TRUE
  }
  d <- all[[i]]
  for (j in 1:length(d)) {
    page <- d[[j]]
    page[,1] <- gsub("D\rE", "", gsub("^[0-9]*", "", page[,1]))
    for (k in grep("USA$", page[,1])) {
      name <- gsub("Mc ", "MC", gsub("USA$", "", page[k, 1]))
      VT <- strsplit(gsub("[^\\s]\\([0-9].*\\)", " ", page[k, 2]), " ")
      UE <- strsplit(gsub("[^\\s]\\([0-9].*\\)", " ", page[k, 3]), " ")
      BB <- strsplit(gsub("[^\\s]\\([0-9].*\\)", " ", page[k, 4]), " ")
      FX <- strsplit(gsub("[^\\s]\\([0-9].*\\)", " ", page[k, 5]), " ")
      VT1_d <- VT[[1]][1]
      VT1_e <- gsub("-.*", "", gsub("^.*\r", "", VT[[1]][3]))
      VT1_tot <- VT[[1]][2]
      VT2_d <- NA
      VT2_e <- NA
      VT2_tot <- NA
      VT_tot <- NA
      UE_d <- UE[[1]][1]
      UE_e <- gsub("-.*", "", gsub("^.*\r", "", UE[[1]][3]))
      UE_tot <- UE[[1]][2]
      BB_d <- BB[[1]][1]
      BB_e <- gsub("-.*", "", gsub("^.*\r", "", BB[[1]][3]))
      BB_tot <- BB[[1]][2]
      FX_d <- FX[[1]][1]
      FX_e <- gsub("-.*", "", gsub("^.*\r", "", FX[[1]][3]))
      FX_tot <- FX[[1]][2]
      comps[[length(comps)+1]] <- data.frame(name, year, type, competition, qual, VT1_d, VT1_e, 
                                             VT1_tot, VT2_d, VT2_e, VT2_tot, VT_tot, UE_d, UE_e,
                                             UE_tot, BB_d, BB_e, BB_tot, FX_d, FX_e, FX_tot)
    }
  }
}

scores <- rbind(scores, do.call(rbind, comps))

## Domestic

cat('\ndom_events\n')
all <- lapply(dir("dom_events", full.names = TRUE), extract_tables, 
              output='data.frame', guess=FALSE, area=
                list(c(109.38249, 30.26453, 720.42042, 592)))
names(all) <- gsub(".pdf", "", dir("dom_events"))
is.list(all)
names(all)

comps <- list()

for (i in 1:length(all)) {
  cat(i,'', names(all)[i], '\n')
  
  info <- strsplit(names(all)[i], "_")[[1]]
  year <- info[1]
  competition <- info[2]
  qual <- FALSE
  type <- "DOM"
  if (length(info) == 3) {
    type <- "TRIAL"
  }
  
  d <- all[[i]]
  
  for (x in 1:length(d)) {
    page <- d[[x]]
    for (j in seq(1, nrow(page), 4)) {
      gymnast <- page[j,]
      name <- strsplit(gymnast$Name, " ")[[1]]
      name <- paste(toupper(name[2]), name[1], sep=" ")
      for (k in 1:2) {
        VT1_d <- NA
        VT1_e <- NA
        VT1_tot <- NA
        VT2_d <- NA
        VT2_e <- NA
        VT2_tot <- NA
        VT_tot <- NA
        UE_d <- NA
        UE_e <- NA
        UE_tot <- NA
        BB_d <- NA
        BB_e <- NA
        BB_tot <- NA
        FX_d <- NA
        FX_e <- NA
        FX_tot <- NA
        score_col <- 7
        if (k == 2) {
          score_col <- 6
        }
        if (competition == "VT") {
          VT_tot <- gymnast[,score_col]
          VT_diff <- strsplit(page[j+1,][,score_col], "/")[[1]]
          VT_ex <- strsplit(page[j+2,][,score_col], "/")[[1]]
          VT1_d <- as.numeric(VT_diff[1])
          VT1_e <- as.numeric(VT_ex[1])
          VT1_tot <- VT1_d + VT1_e
          VT2_d <- as.numeric(VT_diff[2])
          VT2_e <- as.numeric(VT_ex[2])
          VT2_tot <- VT1_d + VT1_e
          ND <- strsplit(page[j+3,][,score_col], "/")[[1]]
          if (ND[1] != "") {
            VT1_tot <- VT1_tot - abs(as.numeric(ND[1]))
          }
          if (!is.na(ND[2])) {
            VT2_tot <- VT2_tot - abs(as.numeric(ND[2]))
          }
        }
        else if (competition == "UE") {
          UE_tot <- gymnast[,score_col]
          UE_d <- page[j+1,][,score_col]
          UE_e <- page[j+2,][,score_col]
        }
        else if (competition == "BB") {
          BB_tot <- gymnast[,score_col]
          BB_d <- page[j+1,][,score_col]
          BB_e <- page[j+2,][,score_col]
        }
        else if (competition == "FX") {
          FX_tot <- gymnast[,score_col]
          FX_d <- page[j+1,][,score_col]
          FX_e <- page[j+2,][,score_col]
        }
        comps[[length(comps)+1]] <- data.frame(name, year, type, competition, qual, VT1_d, VT1_e, 
                                               VT1_tot, VT2_d, VT2_e, VT2_tot, VT_tot, UE_d, UE_e,
                                               UE_tot, BB_d, BB_e, BB_tot, FX_d, FX_e, FX_tot)
      }
    }
  }
}

scores <- rbind(scores, do.call(rbind, comps))



cat('\ndom_aa\n') 
all <- lapply(dir("dom_aa", full.names = TRUE), extract_tables, output='data.frame', guess=FALSE, area=list(c(106.851163, 9.628874, 763.505728, 613.347834)))
names(all) <- gsub(".pdf", "", dir("dom_aa"))
is.list(all)
names(all)

comps <- list()

for (i in 1:length(all)) {
  cat(i,'', names(all)[i], '\n')
  
  info <- strsplit(names(all)[i], "_")[[1]]
  year <- info[1]
  competition <- "AA"
  qual <- FALSE
  type <- "DOM"
  if (length(info) == 2) {
    type <- "TRIAL"
  }
  
  d <- all[[i]]
  for (j in 1:length(d)) {
    page <- d[[j]]
    for (k in seq(1, nrow(page), 7)) {
      gymnast <- page[k,]
      name <- strsplit(gymnast$Name, " ")[[1]]
      name <- paste(toupper(name[2]), name[1], sep=" ")
      for (l in 1:2) {
        round_offset <- 0
        if (l == 2) {
          round_offset <- 3
        }
        VT1_tot <- as.numeric(page[k+round_offset,]$X.2)
        VT1_vals <- strsplit(page[k+1+round_offset,]$X.2, "/")[[1]]
        VT1_d <- as.numeric(VT1_vals[1])
        VT1_e <- as.numeric(VT1_vals[2])
        VT2_tot <- NA
        VT2_d <- NA
        VT2_e <- NA
        VT_tot <- NA
        UE_tot <- as.numeric(page[k+round_offset,]$X.3)
        UE_vals <- strsplit(page[k+1+round_offset,]$X.3, "/")[[1]]
        UE_d <- as.numeric(UE_vals[1])
        UE_e <- as.numeric(UE_vals[2])
        BB_tot <- as.numeric(page[k+round_offset,]$X.4)
        BB_vals <- strsplit(page[k+1+round_offset,]$X.4, "/")[[1]]
        BB_d <- as.numeric(BB_vals[1])
        BB_e <- as.numeric(BB_vals[2])
        FX_tot <- as.numeric(page[k+round_offset,]$X.5)
        FX_vals <- strsplit(page[k+1+round_offset,]$X.5, "/")[[1]]
        FX_d <- as.numeric(FX_vals[1])
        FX_e <- as.numeric(FX_vals[2])
        comps[[length(comps)+1]] <- data.frame(name, year, type, competition, qual, VT1_d, VT1_e, 
                                               VT1_tot, VT2_d, VT2_e, VT2_tot, VT_tot, UE_d, UE_e,
                                               UE_tot, BB_d, BB_e, BB_tot, FX_d, FX_e, FX_tot)
      }
    }
  }
}

scores <- rbind(scores, do.call(rbind, comps))

cat('\ndom_classic\n')

all <- lapply(dir("dom_classic", full.names = TRUE), extract_tables, output='data.frame', guess=FALSE, area=list(c(106.851163, 9.628874, 763.505728, 613.347834)))
names(all) <- gsub(".pdf", "", dir("dom_classic"))
is.list(all)
names(all)

comps <- list()

for (i in 1:length(all)) {
  cat(i,'', names(all)[i], '\n')
  
  year <- names(all)[i]
  competition <- "AA"
  qual <- FALSE
  type <- "DOM"
  
  d <- all[[i]]
  for (j in 1:length(d)) {
    page <- d[[j]]
    for (k in seq(2, nrow(page), 5)) {
      gymnast <- page[k,]
      name <- strsplit(gymnast$Name, " ")[[1]]
      name <- paste(toupper(name[2]), name[1], sep=" ")
      VT1_d <- as.numeric(gymnast$X.2)
      VT1_e <- as.numeric(page[k+1,]$X.2)
      VT1_tot <- as.numeric(page[k+3,]$X.2)
      VT2_d <- as.numeric(NA)
      VT2_e <- as.numeric(NA)
      VT2_tot <- as.numeric(NA)
      VT_tot <- as.numeric(NA)
      UE_d <- as.numeric(gymnast$X.3)
      UE_e <- as.numeric(page[k+1,]$X.3)
      UE_tot <- as.numeric(page[k+3,]$X.3)
      BB_d <- as.numeric(gymnast$X.4)
      BB_e <- as.numeric(page[k+1,]$X.4)
      BB_tot <- as.numeric(page[k+3,]$X.4)
      FX_d <- as.numeric(gymnast$X.5)
      FX_e <- as.numeric(page[k+1,]$X.5)
      FX_tot <- as.numeric(page[k+3,]$X.5)
      comps[[length(comps)+1]] <- data.frame(name, year, type, competition, qual, VT1_d, VT1_e, 
                                             VT1_tot, VT2_d, VT2_e, VT2_tot, VT_tot, UE_d, UE_e,
                                             UE_tot, BB_d, BB_e, BB_tot, FX_d, FX_e, FX_tot)
    }
  }
}

scores <- rbind(scores, do.call(rbind, comps))

write.csv(scores, "Data.csv", row.names=FALSE)


## Scraping Olympic Data
## Different format than other data

cat('\nextract_aa\n')
extract_aa <- function(filename) {
  d <- extract_tables(filename, lattice=TRUE)
  comps <- list()
  
  
  for (j in 1:length(d)) {
    page <- d[[j]]
    page[,1] <- gsub("D\rE", "", gsub("^[0-9]*", "", page[,1]))
    for (k in 2:nrow(page)) {
      if (str_sub(page[k,1], -3, nchar(page[k,1])) == "USA") {
        next
      }
      name <- str_sub(page[k,1], 1, -4)
      VT <- strsplit(gsub("[^\\s]\\([0-9].*\\)", " ", page[k, 2]), " ")
      UE <- strsplit(gsub("[^\\s]\\([0-9].*\\)", " ", page[k, 3]), " ")
      BB <- strsplit(gsub("[^\\s]\\([0-9].*\\)", " ", page[k, 4]), " ")
      FX <- strsplit(gsub("[^\\s]\\([0-9].*\\)", " ", page[k, 5]), " ")
      VT_d <- as.numeric(VT[[1]][1])
      VT_e <- as.numeric(gsub("-.*", "", gsub("^.*\r", "", VT[[1]][3])))
      VT_tot <- as.numeric(VT[[1]][2])
      UE_d <- as.numeric(UE[[1]][1])
      UE_e <- as.numeric(gsub("-.*", "", gsub("^.*\r", "", UE[[1]][3])))
      UE_tot <- as.numeric(UE[[1]][2])
      BB_d <- as.numeric(BB[[1]][1])
      BB_e <- as.numeric(gsub("-.*", "", gsub("^.*\r", "", BB[[1]][3])))
      BB_tot <- as.numeric(BB[[1]][2])
      FX_d <- as.numeric(FX[[1]][1])
      FX_e <- as.numeric(gsub("-.*", "", gsub("^.*\r", "", FX[[1]][3])))
      FX_tot <- as.numeric(FX[[1]][2])
      comps[[length(comps)+1]] <- data.frame(name, VT_d, VT_e, 
                                             VT_tot, UE_d, UE_e,
                                             UE_tot, BB_d, BB_e, BB_tot,
                                             FX_d, FX_e, FX_tot)
    }
  }
  return(do.call(rbind,comps))
}

aa <- extract_aa('olympics/aa.pdf')
write.csv(aa, 'olympics/aa.csv', row.names=FALSE)

aa_qual <- extract_aa('olympics/aa_qual.pdf')
write.csv(aa_qual, 'olympics/aa_qual.csv', row.names=FALSE)

cat('\nextract_team\n')
extract_team <- function(filename) {
  d <- read_lines(pdf_text(filename))
  
  comps <- list()
  
  for (i in 1:length(d)) {
    row <- str_squish(d[i])
    if (grepl("^[0-9][0-9]? [A-Z]{3} ", row)) {
      row <- gsub("- [A-Za-z ']*", "", row)
      row <- strsplit(row, " ")[[1]]
      if (row[2] == "USA") {
        next
      }
      country <- row[2]
      VT <- row[3]
      UE <- row[5]
      BB <- row[7]
      FX <- row[9]
      tot <- row[11]
      
      comps[[length(comps)+1]] <- data.frame(country, VT, UE, BB, FX, tot)
    }
  }
  return(do.call(rbind, comps))
}

team <- extract_team('olympics/team.pdf')
write.csv(team, 'olympics/team.csv', row.names=FALSE)

team_qual <- extract_team('olympics/team_qual.pdf')
write.csv(team_qual, 'olympics/team_qual.csv', row.names=FALSE)

cat('\nextract_event_qual\n')
extract_event_qual <- function(filename) {
  d <- extract_tables(filename, output='data.frame', guess=FALSE, 
                      area=list(c(204.56297,  90.39865, 774.95535, 506.86739)))
  
  comps <- list()
  
  for (i in 1:length(d)) {
    page <- d[[i]]
    for (j in 1:nrow(page)) {
      gymnast <- page[j,]
      if (gymnast$X == "") {
        next
      }
      name <- gymnast$Name
      info <- strsplit(gymnast$TotalCode.Score, " ")[[1]]
      if (info[1] == "USA") {
        next
      }                  
      event_d <- info[2]
      event_e <- gymnast$Score
      event_tot <- as.numeric(strsplit(as.character(gymnast$X), " ")[[1]][1])
      comps[[length(comps)+1]] <- data.frame(name, event_d, event_e, event_tot)
    }
  }
  
  return(do.call(rbind, comps))
}

BB_qual <- extract_event_qual("olympics/BB_qual.pdf")
write.csv(BB_qual, "olympics/BB_qual.csv", row.names=FALSE)
UE_qual <- extract_event_qual("olympics/UE_qual.pdf")
write.csv(UE_qual, "olympics/UE_qual.csv", row.names=FALSE)
FX_qual <- extract_event_qual("olympics/FX_qual.pdf")
write.csv(FX_qual, "olympics/FX_qual.csv", row.names=FALSE)

cat('\nextract_events\n')
extract_events <- function(filename) {
  d <- extract_tables(filename, output='data.frame', guess=FALSE,
                      area=list(c(209.0899,  99.41411, 
                                  374.32261, 484.26918)))[[1]]
  
  comps <- list()
  
  for (i in 1:nrow(d)) {
    row <- d[i,]
    if (row$Code == "USA") {
      next
    }
    name <- row$X
    event_d <- row$Score
    event_e <- row$Score.1
    print(row)
    event_tot <- row$X.1
    
    comps[[length(comps)+1]] <- data.frame(name, event_d, event_e, event_tot)
  }
  
  return(do.call(rbind, comps))
}

BB <- extract_events("olympics/BB.pdf")
write.csv(BB, "olympics/BB.csv", row.names=FALSE)
FX <- extract_events("olympics/FX.pdf")
write.csv(FX, "olympics/FX.csv", row.names=FALSE)
UE <- extract_events("olympics/UE.pdf")
write.csv(UE, "olympics/UE.csv", row.names=FALSE)

cat('\nVT_qual\n')
vault_qual <- extract_tables("olympics/VT_qual.pdf", output='data.frame', guess=FALSE, 
                             area=list(c(211.1437, 112.9972, 
                                         750.0573, 515.9631)))[[1]]

comps <- list()

for (i in seq(1, nrow(vault_qual), 2)) {
  row <- vault_qual[i,]
  if (row$Code == "USA") {
    next
  }
  name <- row$X
  VT1_d <- row$Score
  VT1_e <- row$Score.1
  VT1_tot <- row$Score.2
  VT_tot <- row$X.3
  VT2_d <- vault_qual[i+1,]$Score
  VT2_e <- vault_qual[i+1,]$Score.1
  VT2_tot <- vault_qual[i+1,]$Score.2
  
  comps[[length(comps)+1]] <- data.frame(name, VT1_d, VT1_e, VT1_tot, VT2_d, 
                                         VT2_e, VT2_tot)
}

VT_qual <- do.call(rbind, comps)
write.csv(VT_qual, "olympics/VT_qual.csv", row.names=FALSE)

cat('\nVT_qual\n')
vault <- extract_tables("olympics/VT.pdf", output='data.frame', guess=FALSE, 
                             area=list(c(204.56297,  83.56714, 704.78803,
                                         592.93414 )))[[1]]
comps <- list()

for (i in seq(2, (nrow(vault)-1), 2)) {
  row <- vault[i,]
  if (row$X == "USA") {
    next
  }
  name <- row$Name
  VT1_d <- row$X.1
  VT1_e <- row$X.2
  VT1_tot <- row$X.3
  VT_tot <- row$Total
  VT2_d <- vault[i+1,]$X.1
  VT2_e <- vault[i+1,]$X.2
  VT2_tot <- vault[i+1,]$X.3
  
  comps[[length(comps)+1]] <- data.frame(name, VT1_d, VT1_e, VT1_tot, VT2_d, 
                                         VT2_e, VT2_tot)
}

VT <- do.call(rbind, comps)
write.csv(VT, "olympics/VT.csv", row.names=FALSE)
