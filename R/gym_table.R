library(pdftools)
library(tabulizer)
library(tidyverse)




# extract country abbreviation

noc_string <- readLines("R/noc_key.txt")
uppercase_parts <- regmatches(noc_string, gregexpr("\\b[A-Z]+\\b", noc_string))
noc <- unlist(uppercase_parts)


# 对于event/aa数据：（中文注释在最终定稿之前会删掉）
# 利用国家名称定位，第一个国家名到最后一个国家名之间的全部内容，
# 取x最小值和最大值（作为左右），y的最小值（作为top）。（默认国家缩写为表格第一行）
# 选择最后一个国家名，取y坐标，在y坐标的unique值中排序+1，
# 选择该序数的y值，作为y最大值（作为坐标bottom）

# 无法正常使用的情况：
# 1.如cottbus的VT数据那样，一个人的数据有三行，跨页的

# 可以正常使用的情况：
# Baku, Doha那种前两列低于后面各列



## define get_bottom() function
# serves as a building block of later gym_table() function
# inputs: page, a dataframe; y_vals, a numeric vector; noc_last_y & remainder, integers.
# remainder means how many unique y coordinates there is between last Noc line and last line of table

get_bottom <- function(page, y_vals, y_noc) {
  remainder <- match(y_noc[2], y_vals) - match(y_noc[1], y_vals) - 1
  bottom <- page %>% 
    filter(y == y_vals[match(y_noc[length(y_noc)], y_vals) + remainder]) %>% 
    slice(1) %>% 
    select(y_end) %>% 
    pull()
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

path2 <- "pdfs/int_aa/19_aa.pdf"
gym_table(file_path = path2)

path3 <- "pdfs_2023/baku/baku_w_qual_vt.pdf"
gym_table(file_path = path3)
