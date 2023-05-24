library(vroom)
library(tictoc)
library(tidyverse)
library(data.table)

col_types <- list(
  id = col_character(),
  type = col_character(),
  number = col_character(),
  country = col_character(),
  date = col_date("%Y-%m-%d"),
  abstract = col_character(),
  title = col_character(),
  kind = col_character(),
  num_claims = col_double(),
  filename = col_character(),
  withdrawn = col_double()
)
patent_tbl <- vroom(
  file       = "Patent_data/patent.tsv", 
  delim      = "\t", 
  col_types  = col_types,
  na         = c("", "NA", "NULL")
)
uspc_tbl <- vroom(
  file       = "Patent_data/uspc.tsv", 
  delim      = "\t", 
  col_types  = col_types,
  na         = c("", "NA", "NULL")
)
patent_assignee_tbl <- vroom(
  file       = "Patent_data/patent_assignee.tsv", 
  delim      = "\t", 
  col_types  = col_types,
  na         = c("", "NA", "NULL")
)
assignee_tbl <- vroom(
  file       = "Patent_data/assignee.tsv", 
  delim      = "\t", 
  col_types  = col_types,
  na         = c("", "NA", "NULL")
)
#---------------------Qustion 1------------------------
class(assignee_tbl)
setDT(assignee_tbl)
class(patent_assignee_tbl)
setDT(patent_assignee_tbl)
class(patent_tbl)
setDT(patent_tbl)
tic()
Q1_patent_data <- merge(x = patent_assignee_tbl, y = assignee_tbl, 
                       by.x="assignee_id", by.y ="id",
                       all.x = TRUE, 
                       all.y = FALSE)
toc()
setkey(Q1_patent_data, "assignee_id")
key(Q1_patent_data)
Q1_patent_data %>% dim()
keep_cols <- c("assignee_id","patent_id","organization")
Q1_patent_data <- Q1_patent_data[, ..keep_cols]
Q1_patent_data %>% dim()
Q1_patent_data %>% glimpse()
#Count_Q1 contains the data of the companies/organisations and the number of patents they hold in decending oreder
tic()
Count_Q1<- Q1_patent_data %>%
  filter(!is.na(organization)) %>%
  count(organization)%>% arrange(desc(n))
toc()
#Print the top 10 companies with max patents
head(Count_Q1,10)
#---------------------Qustion 2------------------------
class(patent_tbl)
setDT(patent_tbl)
#Q2_patent_all has patent,patentdata,asignee data merged
tic()
Q2_patent_all <- merge(x = patent_tbl, y = Q1_patent_data, 
                        by.x="id", by.y ="patent_id",
                        all.x = TRUE, 
                        all.y = FALSE)
toc()
#Selecting a timeframe (Aug 1 2014 to Aug 31 2014) and building a new data frame Q2_Aug_patents
Q2_Aug_patents<- with(Q2_patent_all, Q2_patent_all[(date >= "2014-08-01" & date <= "2014-08-31") ])
#Count_Aug has the data of all the organisations that got a patent in Aug 2014 in desc order 
#with number of patents
tic()
Count_Aug<- Q2_Aug_patents %>%
  filter(!is.na(organization)) %>%
  count(organization)%>% arrange(desc(n))
toc()
#Company with most patents in August 2014
head(Count_Aug,1)
#Top 10 companies with new patents in August 2014
head(Count_Aug,10)
#---------------------Qustion 3------------------------
tic()
Q3_patent_all <- merge(x = uspc_tbl, y = Q1_patent_data, 
                       by.x="patent_id", by.y ="patent_id",
                       all.x = TRUE, 
                       all.y = FALSE)
toc()
#Most innovative class can be calculated by summing the total number of patents for each class
tic()
mostinnovative_class_Q3<- Q3_patent_all %>%
  filter(!is.na(mainclass_id)) %>%
  count(mainclass_id)%>% arrange(desc(n))
toc()

#making a df of top10 companies only to investigate the top 5 uspto
Q3_top10 <- Q3_patent_all[Q3_patent_all$organization %in% c('International Business Machines Corporation','Samsung Electronics Co., Ltd.','Canon Kabushiki Kaisha','Sony Corporation','Microsoft Corporation','Google Inc.','Kabushiki Kaisha Toshiba','QUALCOMM Incorporated','LG Electronics Inc.','Panasonic Corporation'),]
#adding a new column with has Company name & Class togeather titled "Company_Class"
Q3_top10$Company_Class <- paste(Q3_top10$organization, Q3_top10$mainclass_id)
#Counting the Company_class and arranging in desc order
tic()
Q3_top10_CC<- Q3_top10 %>%
  count(Company_Class)%>% arrange(desc(n))
toc()

#displaying the most innovative class and its corresponding number of patents of that class
head(mostinnovative_class_Q3,1)

#Shows the top 5 USPTO Classes from the top 10 companies granted patents
head(Q3_top10_CC,5)
