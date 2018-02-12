library(plyr)
library(dplyr)
library(tidyr)

# Convert to local dataframe

refine_original<- read.csv("C:/springboard_capstone_project/refine_original.csv")
product<- as.data.frame(refine_original)
View(product)
View(refine_original)
class(refine_original)
class(product)


# Rename column names
names(product) <- c("company","product.code.number","address","city","country","name") 

# Cleanup company names

product$company<-gsub("\\w+ps$", "philips", product$company, ignore.case = TRUE)
product$company <-gsub("^ak\\s*\\w+", "akzo", product$company, ignore.case = TRUE)
product$company <-gsub("^van\\s\\w+", "van houten", product$company, ignore.case = TRUE)
product$company <-gsub("^un\\w+", "unilever", product$company, ignore.case = TRUE)

#Unique company name list
product %>% select(company) %>% unique()

product$product.code.number <- as.character(product$product.code.number)

# split the product code no. column to productCode and productNumber
product <- product %>%
separate(product.code.number, c("ProductCode", "ProductNumber"), "-")
product %>% select(ProductCode, ProductNumber) %>% unique()
# Add new column product category. It will be identified by this product code
product <- product %>% 
  mutate(category = case_when(
    .$ProductCode=="p" ~ "Smartphone",
    .$ProductCode=="v" ~ "TV",
    .$ProductCode=="x" ~ "Laptop",
    .$ProductCode=="q" ~ "Tablet"
  ))
product %>% select(category) %>% unique()
# Add full address for geocoding
product <- product %>% 
  mutate(full_address = paste(address, city, country, sep = ', '))
product %>% 
  select(full_address) %>% distinct
# Create dummy variables for company and product category
product <- product %>% 
  mutate(company_philips = if_else(company=="philips", 1, 0)) %>% 
  mutate(company_akzo = if_else(company=="akzo", 1, 0)) %>% 
  mutate(company_van_houten = if_else(company=="van houten", 1, 0)) %>% 
  mutate(company_unilever = if_else(company=="unilever", 1, 0)) %>% 
  mutate(product_smartphone = if_else(ProductCode=="p", 1, 0)) %>% 
  mutate(product_tv = if_else(ProductCode=="v", 1, 0)) %>% 
  mutate(product_laptop = if_else(ProductCode=="x", 1, 0)) %>% 
  mutate(product_tablet = if_else(ProductCode=="q", 1, 0)) %>% 
  mutate_at(vars(matches("company_|product_")),funs(as.logical))

product


  
