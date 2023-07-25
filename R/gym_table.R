library(pdftools)
library(tabulizer)
library(tidyverse)


## extract country abbreviation
noc <- readLines("R/noc_key.txt") %>% 
  regmatches(. , gregexpr("\\b[A-Z]+\\b", .)) %>% 
  unlist()


## define get_bottom() function
# serves as a building block of later gym_table() function
# remainder means how many unique y coordinates there is between last Noc line and last line of table

get_bottom <- function(page, y_vals, y_noc) {
  unit_chunk <- page %>% # every text between first noc and second noc
    filter(y >= y_noc[1] & y <= y_noc[2])
  y_diff <- diff(unique(unit_chunk$y))
  
  remainder <- which.max(y_diff) - 1 # get the position that has the largest y difference than its prior raw, which is the first raw of a unit chunk
                                     # since we want the last row of a unit chunk, we minus 1
  bottom <- page %>% 
    filter(y == y_vals[match(y_noc[length(y_noc)], y_vals) + remainder]) %>% # the last noc row + remaining row numbers
    slice(1) %>% 
    select(y_end) %>% 
    pull()+1
  return(bottom)
}


## define gym_table() function

gym_table <- function(file_path){
  page_data <- pdf_data(file_path)
  pdf_tables <- list()
  
  for (i in 1:length(page_data)) {
    page <- page_data[[i]] %>% 
      arrange(y,x) %>% 
      mutate(x_right = x + width, y_end = y + height)
    y_vals <- sort(unique(page$y))
    y_noc <- unique(page[which(page$text %in% noc),]$y)
    if (length(y_noc) == 0) {
      break
    }
    noc_first_y <- y_noc[1]
    noc_last_y <- y_noc[length(y_noc)]
    
    chunk <- page %>% # every text between first noc and last noc
      filter(y >= noc_first_y & y<= noc_last_y)
    
    top <- noc_first_y-2 # -2 is to make it compatible for aa results
    left <- chunk %>% 
      arrange(x) %>% 
      pull(x) %>% 
      first()
    right <- chunk %>%  
      arrange(x_right) %>% 
      pull(x_right) %>% 
      last()+1
    
    bottom <- get_bottom(page = page, y_vals = y_vals, y_noc = y_noc)
    
    page_area <- list(c(top, left, bottom, right))
    page_table <- extract_tables(file_path, pages = i, guess = F, area = page_area)
    pdf_tables <- c(pdf_tables, page_table)
  }
  
  # pdf_tables <- lapply(pdf_tables, as.data.frame)
  return(pdf_tables)
}


# example
paths <- "pdfs/int_events/19_qual.pdf"
gym_table(file_path = paths)

path2 <- "pdfs/int_aa/19_aa_qual.pdf"
gym_table(file_path = path2)

path3 <- "pdfs_2023/baku/baku_w_qual_vt.pdf"
gym_table(file_path = path3)

path4 <- "pdfs_2023/cairo/cairo_m_qual_PH.pdf"
gym_table(file_path = path4)

path5 <- "pdfs_2023/liverpool/m_aa_final.pdf"
gym_table(file_path = path5)
