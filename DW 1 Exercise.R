library(tidyr)
library(dplyr)

#Reading file from CSV
rawcsv <- read.csv("refine_original.csv")

#Changing CSV to df then to tbl
csvtbl <- tbl_df(rawcsv)

#Bring Company into a Vector
cnames <- as.character(csvtbl$company)


#Cleaning Company

#If it ends with S, then rename entire to phillips
#If it starts with A or a, rename to akzo
#If it ends with R, then rename to unilever
#If it starts with V, then rename to van houten

locations <- grep ("s$|S$",cnames)
cnames[locations] <- "philips"

locations <- grep ("o$|O$|0$",cnames)
cnames[locations] <- "akzo"

locations <- grep ("r$",cnames)
cnames[locations] <- "unilever"

locations <- grep (" ",cnames)
cnames[locations] <- "van houten"

#Assigning Cnames to dataframe
csvtbl$company <-cnames


#Parse product code and number

pccol <- csvtbl$Product.code...number
pcodes <- sub("-[0-9]+", "", pccol)
ncodes <- sub(".*-", "", pccol)

#Adding text version
pcodetxt <- pcodes
pcodetxt <- sub("p", "Smartphone", pcodetxt)
pcodetxt <- sub("v", "TV", pcodetxt)
pcodetxt <- sub("x", "Laptop", pcodetxt)
pcodetxt <- sub("q", "Tablet", pcodetxt)

#Binding columns - product code, number code, product txt
csvtbl <- cbind(csvtbl, pcodes, ncodes, pcodetxt)

#Combining address, city, country into Geocode address
csvtbl <- unite(csvtbl, full_address, address, city, country, sep =",", remove = FALSE)


#Creating Binary Column function

binarycol <- function(item, letter) {
  if (item == letter){
    item=1
  } else {item=0}
}

company_philips <- cnames
company_akzo <- cnames
company_van_houten <-cnames
company_unilever <-cnames

product_smartphone <- pcodes
product_tv <- pcodes
product_laptop <- pcodes
product_tablet <- pcodes

product_smartphone <-vapply(product_smartphone, binarycol, letter="p", numeric(1))
product_tv <-vapply(product_tv, binarycol, letter="v", numeric(1))
product_laptop <-vapply(product_laptop, binarycol, letter="x", numeric(1))
product_tablet <-vapply(product_tablet, binarycol, letter="q", numeric(1))

company_philips <-vapply(company_philips, binarycol, letter="philips", numeric(1))
company_akzo <-vapply(company_akzo, binarycol, letter="akzo", numeric(1))
company_van_houten <-vapply(company_van_houten, binarycol, letter="van houten", numeric(1))
company_unilever <-vapply(company_unilever, binarycol, letter="unilever", numeric(1))

#Binding columns -  binary product codes, then binary company codes
csvtbl <- cbind(csvtbl, product_smartphone, product_tv, product_laptop, product_tablet)
csvtbl <- cbind(csvtbl, company_philips, company_akzo, company_van_houten, company_unilever)
glimpse(csvtbl)

write.csv(csvtbl, file = "refine_completed.csv", row.names=FALSE)
