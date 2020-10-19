library(knitr)
library(tidyverse)
library(magrittr)
library(kableExtra)

## read the data

ag_data <- read_csv("/Users/amelia/Documents/mssp/MA615/Hw_berries/Berries_strawberries/data/berries.csv", col_names = TRUE)

## look at number of unique values in each column
aa <- summarize_all(ag_data, n_distinct)

## make a list of the columns with only one unique value
bb <- which(aa[1,] == 1)

## list the 1-unique valu column names 
colnames(ag_data)[bb]

## remove the 1-unique columns from the dataset
ag_data %<>% select(-all_of(bb))

aa %<>% select(-all_of(bb)) 


## State name and the State ANSI code are (sort of) redundant
## Just keep the name
ag_data %<>% select(-4)
aa %<>% select(-4) 

## Display the head of ag_data 
head(ag_data)

## 3 kinds of berries
berry <- unique(ag_data$Commodity)
nberry <- length(berry)




## Strawberries
sberry <- ag_data %>% filter((Commodity=="STRAWBERRIES") & (Period=="YEAR"))
sberry %<>% select(-c(Period, Commodity))   

#### Does every Data Item begin with "STRAWBERRIES, "
sum(str_detect(sberry$`Data Item`, "^STRAWBERRIES, ")) == length(sberry$`Data Item`)

## Seperate the Data Item
sberry1 <- subset(sberry, str_detect(sberry$`Data Item`, "^STRAWBERRIES, ") == "TRUE")
sberry2 <- subset(sberry, str_detect(sberry$`Data Item`, "^STRAWBERRIES, ") == "FALSE")
sberry1 %<>% separate(`Data Item`, c("B","type", "meas", "what"), sep = ",") 
sberry2 %<>% separate(`Data Item`, c("B","todo"), sep = "-") 
sberry2 %<>% separate(`todo`, c("type","meas", "what"), sep = ",") 
sberry <- rbind(sberry1, sberry2)
sberry %<>% select(-B)


## Seperate the type
sberry1 <- subset(sberry, str_detect(sberry$`type`, " - ") == "TRUE")
sberry1 %<>% separate(type,c("type", "lab1", "lab2"), " - ")
sberry1 %<>% separate(type,c("b1", "type"), " ")

sberry2 <- subset(sberry, type == " FRESH MARKET")
sberry2$type <- "FRESH MARKET"

sberry3 <- subset(sberry, type == " PROCESSING")
sberry3$type <- "PROCESSING"

sberry4 <- subset(sberry, str_detect(sberry$`type`, " - ") == "FALSE" & 
                    type != " FRESH MARKET" & type != " PROCESSING")
sberry4 %<>% separate(type,c("b1", "lab1", "lab2"), " ")

sberry <- plyr::rbind.fill(sberry1, sberry2, sberry3, sberry4)

sberry[is.na(sberry)] <- ""

sberry %<>% select(-b1)
## OK now Data Item has been split into parts



## onto Domain
sberry %<>% separate(Domain, c("D_left", "D_right"), sep = ", ")

sberry[is.na(sberry)] <- ""


## And now Domain Category
sberry %<>% separate(`Domain Category`, c("DC_left", "DC_right"), sep = ", ")

## unique(sberry$DC_left)
## unique(sberry$DC_right)

## looks like DC_left combines labels
sberry$DC_right[which(str_detect(sberry$DC_left,"138831"))]="INSECTICIDE: (CYFLUMETOFEN = 138831)"
sberry$DC_left[which(str_detect(sberry$DC_left,"138831"))]="CHEMICAL"


## work on DC_left first

sberry %<>% separate(DC_left, c("DC_left_l", "DC_left_r"), sep = ": ")

## sberry$DC_left_l %>% unique()
## sberry$DC_left_r %>% unique()

## now work on DC_right

sberry %<>% separate(DC_right, c("DC_right_l", "DC_right_r"), sep = ": ") 


sberry[is.na(sberry)] <- ""

##  OK now we need to eliminate the redundancy


## fine and remove redundant columns

## paste(sberry$D_left,sberry$DC_left_l) %>% unique
## returns -- "CHEMICAL CHEMICAL"     "FERTILIZER FERTILIZER" "TOTAL NOT SPECIFIED" 

## remove column bberry$DC_left_l

sberry %<>%  select(-DC_left_l) 



## test

# sum(sberry$D_right == sberry$DC_right_l)
# [1] 3220
# sberry$DC_left_r %>% unique()
# [1] ""            "(NITROGEN)"  "(PHOSPHATE)" "(POTASH)"    "(SULFUR)"   

## remove column DC_right_l

sberry %<>% select(-DC_right_l)

## Test for lab1, lab2

# paste(sberry$lab1, sberry$lab2) %>% unique()
# [1] "APPLICATIONS "   "TREATED "        "PRODUCTION "     " "              
# [5] "ACRES HARVESTED" "ACRES PLANTED"   "YIELD "  


sberry %<>% mutate(label = paste(lab1,lab2)) 


## test for necessity of "chemical" in col D_left


# paste(sberry$D_left, sberry$D_right) %>% unique()
# [1] "CHEMICAL FUNGICIDE"   "CHEMICAL HERBICIDE"   "CHEMICAL INSECTICIDE"
# [4] "CHEMICAL OTHER"       "FERTILIZER "          "TOTAL "   


## remove "Chemical" and joint the columns

sberry$D_left[which(sberry$D_left == "CHEMICAL")] <- ""

sberry %<>% mutate(Chemical=paste(D_left, D_right)) 

sberry %<>% select(-c(D_left, D_right)) 


sberry %<>% select(Year, State, type, what, meas, label, DC_left_r, DC_right_r, Chemical, Value )


###  Now the problem is that we have entries in both the "what" and "meas" columns
##  that begin  "MEASURED IN"
##  how many are there

## in the column "what"
cnt_1 <- str_detect(sberry$what, "MEASURED IN")
sum(cnt_1)

## in the column "meas"

cnt_2 <- str_detect(sberry$meas, "MEASURED IN")
sum(cnt_2)

## We want to put them all in the same column
## So, we will separate them from their current column and put them into
## two columns -- then we will test to make sure there aren't any overlaps
## and then merge the two columns

## we're going to use PURRR.  We need a simple function that takes a logical
## variable and a second variable.  It returns the second variable if the logical
## variable is true and returns a blank if it is false

f1 <- function(a,b){
  if(a){
    return(b)
  }else{
    return("")
  }
}
#################################################
##  try it

f1(TRUE,"hi")

f1(!TRUE, "hi")

## now try it with map2()

f1_log <- c(FALSE, TRUE, TRUE)
f1_str <- c("one", "two", "three")

map2(f1_log, f1_str, f1)

#########################################################
## now let's separate the "MEASURED IN" entries in the meas column
## form an index of the entries to be separated out

index_meas <- str_detect(sberry$meas, "MEASURED IN")

## verify the first six values against the dats bberry
head(index_meas)
new <- map2(index_meas, sberry$meas, f1)
new <- unlist(new)
head(new, n=20)
######################################################

sberry %<>% mutate(m_in_1 = unlist(map2(index_meas, sberry$meas, f1))) 

# 
#
# b_f <- sberry
# 
#  
#  
#  l1 <- "MEASURED IN LB"
#  l2 <- "howdy"
#  l3 <- "MEASURED IN LB / ACRE"
# 
#  str_replace(c(l1,l2,l3), "MEASURED IN.*$", "")
# 
# tmp <- str_replace(l4, "MEASURED IN.*$", "")
# 
# b_f %<>%  mutate(meas = str_replace(b_f$meas, "MEASURED IN.*$", "" ))
# 
# Check on whether it worked
#
# cnt_l <- str_detect(b_f$meas, "MEASURED IN")
# sum(cnt_l)
#

sberry %<>% mutate(meas = str_replace(sberry$meas, "MEASURED IN.*$", ""))

## Check
cnt_3 <- str_detect(sberry$meas, "MEASURED IN")
sum(cnt_3)


#########################
## Now we will do the same thing with the 
## "what" column  

### index of cells to be isolated
index_what <- str_detect(sberry$what, "MEASURED IN")
sum(index_what)

### create a column of the isolated cells
sberry %<>% mutate(m_in_2 = unlist(map2(index_what, sberry$what, f1))) 

###  eliminate the isolated cells from the original column
sberry %<>% mutate(what = str_replace(sberry$what, "MEASURED IN.*$", ""))

### test that theere are no more "MEASURED IN" cells in the original column
cnt_what <- str_detect(sberry$what, "MEASURED IN")
sum(cnt_what)

### Check for overlaps


sberry %<>% mutate(units = str_trim(paste(m_in_1, m_in_2))) 

sberry$units %>% unique()


## now let's clean it up 

sberry$what %>% unique()  ## rename Avg

sberry$meas %>% unique()  ## rename marketing

sberry$label %>% unique() ## rename harvest 

sberry$DC_left_r %>% unique() # rename chemical_family

tmp <- sberry$DC_right_r %>% unique() # rename materials --213

tmp <- sberry$Value %>% unique() # values

tmp <- sberry$units %>% unique() # Measures


sberry %<>% rename(c(Avg = what, Marketing = meas, Harvest = label, 
                               Chem_family = DC_left_r, Materials = DC_right_r, 
                               Measures = units))

sberry %<>% select(Year, State, type, Marketing, 
                   Measures, Avg, Harvest, Chem_family,
                   Materials, Chemical, Value )

str_trim(paste(sberry$Marketing, sberry$Harvest)) %>% unique

###  these belong in one column

sberry %<>% mutate(production = str_trim(paste(Marketing, Harvest)))

sberry %<>% select(Year, State, type, production, Measures,
                   Avg, Chem_family, Materials, Chemical, Value)


## I missed this one !!

sberry %<>% mutate(Chemical = str_trim(paste(Chem_family, Chemical)))

sberry %<>% select(Year, State, type, production, Avg, Measures, 
                   Materials, Chemical, Value)

kable(head(sberry, n=10)) %>% kable_styling(font_size=12)

sberry$Value <- as.numeric(str_replace_all(sberry$Value,c(','='')))
write.csv(sberry, file = "/Users/amelia/Documents/mssp/MA615/Hw_berries/strawberries.csv", row.names = FALSE)

