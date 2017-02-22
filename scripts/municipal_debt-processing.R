library(dplyr)
library(RCurl)

##################################################################
#
# Processing Script for Municipal Debt
# Created by Jenna Daly
# On 02/15/2017
#
##################################################################

sub_folders <- list.files()
data_location <- grep("raw", sub_folders, value=T)
path <- (paste0(getwd(), "/", data_location))
#grabs all csvs (even not FISCIN data)
all_csvs <- dir(path, recursive=T, pattern = ".csv") 
#isolates FISCIN csvs
only_FISCIN <- all_csvs[grep("FISCIN", all_csvs)] 

#create empty data frame with set columns
all_data <- setNames(data.frame(matrix(ncol = 11, nrow = 0)), 
                     c("Town", 
                       "Year", 
                       "Bonded Regional School District Debt", 
                       "Debt Service",
                       "Long Term Bonded Debt", 
                       "Total Indebtedness", 
                       "Total Revenue", 
                       "ENGL",
                       "Total Expenditures",
                       "Population",
                       "NGL"))

#read in each raw file and get ready for master combine
for (i in 1:length(only_FISCIN)) {
  current_file <- read.csv(paste0(path, "/", only_FISCIN[i]), stringsAsFactors=F, header=T)
  remove_folder <- sub(".*/", "", only_FISCIN[i]) #filename
  get_year <- unique(as.numeric(unlist(gsub("[^0-9]", "", unlist(remove_folder)), "")))
  get_year <- get_year + 2000
  SFY <- paste(get_year - 1, get_year, sep = "-")
  SFY2 <- paste("SFY", SFY)
  current_file$Year <- SFY2
  col_names <- colnames(current_file)
  pop_col <- grep("population", col_names, ignore.case=T, value=T)
  NGL_col <- grep("ACGLFY", col_names, ignore.case=T, value=T)
  final_columns <- current_file[, c("Municipality", 
                                    "Year", 
                                    "RSD_DEBT",
                                    "Debt_Service",         
                                    "TOWN_Bonded_Long_Term_Debt",
                                    "Total_Bonded_Long_Term_Debt_.RSD_._Town.",
                                    "Total_Revenue", 
                                    "EGL",
                                    "Total_Expenditures",
                                    pop_col,
                                    NGL_col)]  
  names(final_columns) <- c("Town", 
                           "Year", 
                           "Bonded Regional School District Debt", 
                           "Debt Service",
                           "Long Term Bonded Debt", 
                           "Total Indebtedness", 
                           "Total Revenue", 
                           "ENGL",
                           "Total Expenditures",
                           "Population",
                           "NGL")

  # take out "Groton (City of)" because it is a political subdivision of groton and not the town.
  final_columns <- final_columns[final_columns$Town != "GROTON (City of)",]
  
  # Add this iteration's data to main container, first removing duplicated year data
  all_data <- all_data[all_data$Year!=SFY2,]
  all_data <- rbind(all_data, final_columns)
}

# Town names to title case
all_data$"Town" <- gsub("\\b([a-z])([a-z]+)", "\\U\\1\\E\\2", tolower(all_data$Town), perl=TRUE)

## Round numeric columns to whole numbers
round_df <- function(x, digits) {
  columns_to_round <- c(  "Total Expenditures",
                          "Total Revenue",
                          "Debt Service",
                          "Bonded Regional School District Debt",
                          "Long Term Bonded Debt",
                          "Total Indebtedness",
                          "NGL",
                          "ENGL",
                          "Population" )
  columns_to_round <- sapply(x, mode) == 'numeric'
  all_data[columns_to_round] <-  round(all_data[columns_to_round], digits)
  all_data
}

all_data <- round_df(all_data, 0)

#Create Calculated Columns (percents will be rounded to 1 decimal)
all_data$"Debt per Capita" <- NA
all_data$"Debt per Capita" <- round((all_data$"Total Indebtedness" / all_data$"Population"), 0)

all_data$"Debt as Percent of Revenue" <- NA
all_data$"Debt as Percent of Revenue" <- round(100 * (all_data$"Total Indebtedness" / all_data$"Total Revenue"), 1)

all_data$"Total Indebtedness as Percent of Total Expenditures" <- NA
all_data$"Total Indebtedness as Percent of Total Expenditures" <- round(100 * (all_data$"Total Indebtedness" / all_data$"Total Expenditures"), 1)

all_data$"Ratio of Debt to Actual Grand List" <- NA
all_data$"Ratio of Debt to Actual Grand List" <- round(100 * (all_data$"Total Indebtedness" / all_data$"NGL"), 1)

all_data$"Ratio of Debt to Equalized Net Grand List" <- NA
all_data$"Ratio of Debt to Equalized Net Grand List" <- round(100 * (all_data$"Total Indebtedness" / all_data$"ENGL"), 1)

#Calculate Debt Per Capita as a Percent of State Average
#Calculated as: 

 #[                             Debt per capita of a given town for a given year                           ]
 #[--------------------------------------------------------------------------------------------------------] X 100
 #[(sum of all towns indebtedness for that given year) / (sum of all states population for that given year)]

sum_indebt <- aggregate(`Total Indebtedness` ~ Year, all_data, sum)
sum_pop <- aggregate(Population ~ Year, all_data, sum)

joined_sums <- merge(all_data, sum_indebt, by="Year")
joined_sums <- merge(joined_sums, sum_pop, by="Year")

joined_sums$"Debt per Capita as Percent of State Average" <- NA
joined_sums$"Debt per Capita as Percent of State Average" <- round(((joined_sums$"Debt per Capita")/((joined_sums$`Total Indebtedness.y`)/(joined_sums$Population.y)))*100, 1) 
        
arranged <- select(joined_sums, Town, Year, `Bonded Regional School District Debt`,
                                `Debt Service`, `Long Term Bonded Debt`, `Total Indebtedness.x`,                            
                                `Total Revenue`, ENGL, `Total Expenditures`,
                                Population.x, NGL, `Debt per Capita`, `Debt as Percent of Revenue`,
                                `Total Indebtedness as Percent of Total Expenditures`,
                                `Ratio of Debt to Actual Grand List`,
                                `Ratio of Debt to Equalized Net Grand List`, 
                                `Debt per Capita as Percent of State Average`) %>% 
            arrange(Town)

arranged$`Total Revenue` = NULL
arranged$`Total Expenditures` = NULL
arranged$`NGL` = NULL
arranged$`ENGL` = NULL
arranged$Population.x = NULL

#Relabel column names
colnames(arranged) <- c( "Town", "Year", 
                         "Bonded Regional School District Debt",
                         "Debt Service", 
                         "Long Term Bonded Debt", 
                         "Total Indebtedness",                            
                         "Debt per Capita", 
                         "Debt as Percent of Revenue",
                         "Total Indebtedness as Percent of Total Expenditures",
                         "Ratio of Debt to Actual Grand List",
                         "Ratio of Debt to Equalized Net Grand List", 
                         "Debt per Capita as Percent of State Average")

#convert to long format
cols_to_stack <- c("Bonded Regional School District Debt",                   
                   "Debt per Capita",                        
                   "Debt Service",                                       
                   "Long Term Bonded Debt",
                   "Total Indebtedness",                                   
                   "Debt as Percent of Revenue",                         
                   "Debt per Capita as Percent of State Average",             
                   "Ratio of Debt to Actual Grand List",                    
                   "Ratio of Debt to Equalized Net Grand List",    
                   "Total Indebtedness as Percent of Total Expenditures")

long_row_count = nrow(arranged) * length(cols_to_stack)

combined_final_long <- reshape(arranged, 
                               varying = cols_to_stack, 
                               v.names = "Value", 
                               timevar = "Variable", 
                               times = cols_to_stack, 
                               new.row.names = 1:long_row_count,
                               direction = "long"
)

combined_final_long$Variable <- factor(combined_final_long$Variable, levels = cols_to_stack)
combined_final_long <- combined_final_long[order(combined_final_long$Town, combined_final_long$Year, combined_final_long$Variable),]
combined_final_long$id <- NULL

#check to see if any duplicates made it into the data frame (this should be empty)
duplicates <- combined_final_long[duplicated(combined_final_long[,1:3]),]

#add Measure Type
combined_final_long$"Measure Type" <- NA
##fill in 'Measure Type' column based on criteria listed below
combined_final_long$"Measure Type"[which(combined_final_long$Variable %in% c("Bonded Regional School District Debt",
                                               "Debt per Capita",                                         
                                               "Debt Service",                                            
                                               "Long Term Bonded Debt",                                                       
                                               "Total Indebtedness"))] <- "Number"

combined_final_long$"Measure Type"[which(combined_final_long$Variable %in% c("Debt as Percent of Revenue",                               
                                               "Debt per Capita as Percent of State Average",              
                                               "Ratio of Debt to Actual Grand List",                        
                                               "Ratio of Debt to Equalized Net Grand List",                         
                                               "Total Indebtedness as Percent of Total Expenditures"))] <- "Percent"

#add FIPS (using raw URL from GitHub)
common <- ("https://raw.githubusercontent.com/CT-Data-Collaborative/ctdata-catalog/master/Common")
fips <- read.csv(file.path(common, "Geography", "town_fips.csv?token=AIOGobc7jL5CPapMO4OqpDM8eBDAmARgks5YteSTwA%3D%3D"))

merge_long_fips <- merge(combined_final_long, fips, all=T)

#remove "Connecticut"
municipal_debt_data <- merge_long_fips[!merge_long_fips$Town == "Connecticut",]

#Reorder columns
municipal_debt_data <- municipal_debt_data[c("Town", "FIPS", "Year", "Measure Type", "Variable", "Value")]

#Sort data
municipal_debt_data <- arrange(municipal_debt_data, Year, Variable, `Measure Type`, Town)

# Write to File
write.table(
  municipal_debt_data,
  file.path(getwd(), "data", "municipal_debt.csv"),
  sep = ",",
  na = "",
  row.names = F
)
