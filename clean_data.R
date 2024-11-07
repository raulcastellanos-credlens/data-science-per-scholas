
packages_list <- c("tidyverse", 
                   "lubridate", 
                   "janitor",
                   "dplyr",
                   "stringr",
                   "readr", 
                   "knitr",
                   "ggplot2", 
                   "readxl", 
                   "tinytex")
#tinytex::install_tinytex(force = TRUE)

sapply(packages_list,
       function(x) {
         if(! x %in% rownames(installed.packages()))
           install.packages(x)
         require(x, character.only=T)
       })

options(scipen=999) # prevent scientific notation

#remove all of the previous work in my environment
rm(list = ls())

setwd("/Users/raul.castellanos/Documents/data/per_scholas/")

per_scholas <- Per_Scholas_Graduate_Data_Since_2012 <- read_excel("../data/Per Scholas Graduate Data Since 2012.xlsx")
per_scholas = per_scholas %>% clean_names()


## Clean data set 

```{r}

#City 
per_scholas <- per_scholas %>%
  mutate(contact_full_name_mailing_city = str_to_upper(contact_full_name_mailing_city)) %>% 
  mutate(
    city_cleaned = if_else(
      str_detect(contact_full_name_mailing_city, "\\d"),  # Check for any digit
      NA_character_,                                      # Replace with NA if a digit is found
      contact_full_name_mailing_city %>%
        str_replace_all("(?i)<\\s*BR\\s*>", "") %>%      # Remove all occurrences of <BR> in a case-insensitive way
        str_replace(",.*", "") %>%                       # Remove comma and everything after it
        str_replace("NEW YORK CITY", "NEW YORK") %>%  
        str_replace("LONG ISLAND CITY", "LONG ISLAND") %>%
        str_replace("MISSOURI CITY", "MISSOURI") %>% 
        str_replace("ABINGTON", "ABINGDON") %>% 
        str_replace("ALLEN TX", "ABINGDON") %>% 
        str_replace("DALLAS TX", "DALLAS") %>% 
        str_replace("HOUSTON TX", "HOUSTON") %>% 
        str_replace("BEAVER FALLS", "BEAVERFALLS")
    ),
    city_changed_flag = if_else(
      contact_full_name_mailing_city == city_cleaned, 0, 1
    )
  )

#SSN 

invalid_ssn_values <- c("9334", "*****1666", "***-**-****", "***-**-4874", "7", 
                        "000-00-0000", "000-00-1418", "000-00-7147", "0", 
                        "0000000000", "00000000000", "00000007896", "000000611")


# Clean and format SSNs
per_scholas <- per_scholas %>%
  mutate(
    # Remove hyphens and # symbols
    ssn_cleaned = str_replace_all(contact_full_name_social_security_number_dvp, "[-#]", ""),
    
    # Set to NA if the SSN matches any pattern with asterisks or if it’s in the list of invalid values
    ssn_cleaned = if_else(
      ssn_cleaned %in% invalid_ssn_values | 
      str_detect(ssn_cleaned, "\\*") |             # Exclude any SSNs containing '*'
      str_detect(ssn_cleaned, "^0+$"),             # Exclude any SSNs with only zeros
      NA_character_,
      ssn_cleaned
    ),
    
    # Set to NA if length is not exactly 9 after cleaning
    ssn_cleaned = if_else(str_length(ssn_cleaned) == 9, ssn_cleaned, NA_character_),
    
    # Format valid 9-digit SSNs as XXX-XX-XXXX
    ssn_cleaned = if_else(!is.na(ssn_cleaned),
                          str_c(str_sub(ssn_cleaned, 1, 3), "-", 
                                str_sub(ssn_cleaned, 4, 5), "-", 
                                str_sub(ssn_cleaned, 6, 9)),
                          NA_character_)
  )

# Ages 

per_scholas <- per_scholas %>%
  mutate(
    contact_full_name_birthdate = as.Date(contact_full_name_birthdate, format = "%Y-%m-%d"), # Ensure date format
    contact_full_name_birthdate = if_else(
      year(contact_full_name_birthdate) > 2008, 
      NA_Date_, 
      contact_full_name_birthdate
    )
  )

###############
#Zip codes 
################


# Zip codes
per_scholas <- per_scholas %>%
  mutate(zipcode_clean = case_when(
    # Keep as is if the length is 5
    str_length(contact_full_name_mailing_zip_postal_code) == 5 ~ contact_full_name_mailing_zip_postal_code,
    
    # Remove the '&amp' part if present
    str_detect(contact_full_name_mailing_zip_postal_code, "&amp") ~ 
      str_remove(contact_full_name_mailing_zip_postal_code, "&amp"),
    
    # Set to NA if the length is less than 5
    str_length(contact_full_name_mailing_zip_postal_code) < 5 ~ NA_character_,
    
    # Leave as is if it already has the format 'XXXXX-YYYY'
    str_detect(contact_full_name_mailing_zip_postal_code, "^\\d{5}-\\d{4}$") ~ contact_full_name_mailing_zip_postal_code,
    
    # Format as 'XXXXX-YYYY' if longer than 5 and missing the '-'
    str_length(contact_full_name_mailing_zip_postal_code) > 5 & 
    !str_detect(contact_full_name_mailing_zip_postal_code, "-") ~ 
      str_c(str_sub(contact_full_name_mailing_zip_postal_code, 1, 5), "-", str_sub(contact_full_name_mailing_zip_postal_code, 6, 9)),
    
    # Default to NA for any other cases
    TRUE ~ NA_character_
  )) %>%
  # Additional step: remove last part if format is XXXXX-X
  mutate(zipcode_clean = if_else(
    str_detect(zipcode_clean, "^\\d{5}-\\d$"),
    str_sub(zipcode_clean, 1, 5),
    zipcode_clean
  ))


###################
#credential flag 
###################
per_scholas <- per_scholas %>%
  mutate(credential_flag = ifelse(
    rowSums(select(., comp_tia_a_certified:contact_full_name_security_cisco_certified) != 0 & 
            !is.na(select(., comp_tia_a_certified:contact_full_name_security_cisco_certified))) > 0,
    1,
    0
  ))


#National column 
per_scholas <- per_scholas %>%
  mutate(national_flag = ifelse(
      program_campus == 'National', 1, 0
    )
  )



# Credential names with their display strings
credential_names <- list(
  comp_tia_a_certified = "CompTIA A+",
  comp_tia_cy_sa_certified = "CompTIA CySA+",
  contact_full_name_network_certified = "CompTIA Network+",
  aws_certified_cloud_practitioner = "AWS CCP",
  google_it_support_certified = "Google IT Support",
  contact_full_name_ccna_certified = "CCNA Certified",
  contact_full_name_security_cisco_certified = "Security+/Cisco Certified"
)

# Process per_scholas data frame
per_scholas <- per_scholas %>%
  # Convert columns to 0 or 1 based on condition
  mutate(across(
    names(credential_names), 
    ~ if_else(is.na(.) | . == 0, 0, 1)
  )) %>%
  rowwise() %>%
  mutate(
    # Create stacked_credential by concatenating non-zero credentials
    stacked_credential = paste(
      unlist(credential_names)[unlist(c_across(names(credential_names))) == 1], 
      collapse = " |"
    ),
    # Replace empty string with NA
    stacked_credential = if_else(stacked_credential == "", NA_character_, stacked_credential)
  ) %>%
  ungroup()


#### Ethnicity 

per_scholas <- per_scholas %>%
  mutate(
    contact_full_name_race_ethnicity = if_else(
      str_detect(contact_full_name_race_ethnicity, ";"), 
      "Two or more Race/Ethnicity categories", 
      contact_full_name_race_ethnicity
    )
  )

#STACKED TABLE 

# first version 

# Step 1: Filter to keep only the earliest credential (by end_date) for each student (con_id)
per_scholas_filtered <- per_scholas %>%
  group_by(con_id) %>%
  filter(end_date == max(end_date, na.rm = TRUE)) %>%  # Keep only the oldest end_date for each credential
  ungroup()

# Step 2: Group by training_track and stacked_credential, then count
stacked_credential_table <- per_scholas_filtered %>%
  group_by(training_track, stacked_credential) %>%
  summarize(count = n(), .groups = "drop")  # Count occurrences of each unique credential set

# Make sure it is correct 
total_credential_count <- stacked_credential_table %>%
  filter(!is.na(stacked_credential)) %>%
  summarize(total_count = sum(count)) %>%  # Sum the count column
  pull(total_count)

# Define the age bucket function
age_bucket <- function(age) {
  case_when(
    age < 18                    ~ "Under 18",
    age >= 18 & age <= 24       ~ "18-24",
    age >= 25 & age <= 34       ~ "25-34",
    age >= 35 & age <= 44       ~ "35-44",
    age >= 45 & age <= 54       ~ "45-54",
    age >= 55 & age <= 64       ~ "55-64",
    age >= 65                   ~ "65 and over"
  )
}

# Apply the function to create age_bucket_at_cred_award column based on end_date
per_scholas <- per_scholas %>%
  mutate(
    # Calculate age based on end_date (award date)
    age_at_cred_award = as.integer(interval(contact_full_name_birthdate, end_date) / years(1)),
    # Assign age bucket based on calculated age
    age_bucket_at_cred_award = age_bucket(age_at_cred_award)
  )

# exam columns 

per_scholas <- per_scholas %>%
  mutate(
    # Create the boolean flag column: 1 if non-zero/non-NA, otherwise 0
    exam_boolean_flag = if_else(!is.na(number_exam_based_certs_attained_in_program) & 
                                number_exam_based_certs_attained_in_program > 0, 1, 0),
    
    # Create the 3-category column based on original values
    exam_category = case_when(
      is.na(number_exam_based_certs_attained_in_program) | number_exam_based_certs_attained_in_program == 0 ~ "0",
      number_exam_based_certs_attained_in_program == 1 ~ "1",
      number_exam_based_certs_attained_in_program > 1 ~ "2+"
    )
  )

# Add transaction date and organization name 
per_scholas$tx_date = '20241107'
per_scholas$organization_name = 'Per Scholas'

# Add the year of the start date 
per_scholas <- per_scholas %>%
  mutate(start_date_year = year(as.Date(start_date)))


#Clean columns to put them in order and name after the golden record 
per_scholas_final <- per_scholas %>%
  select(-city_changed_flag) %>% 
  rename(record_id = admission_decision_id_ad_number_number_number_number_number,
         first_name = contact_full_name_first_name, 
         middle_name = contact_full_name_middle_name_or_initial, 
         last_name = contact_full_name_last_name, 
         ssn = ssn_cleaned, 
         street_line_1 = contact_full_name_mailing_street, 
         city = contact_full_name_mailing_city, 
         state = contact_full_name_mailing_state_province, 
         zipcode = zipcode_clean, 
         dob = contact_full_name_birthdate, 
         gender = contact_full_name_gender_identity, 
         race_ethnicity = contact_full_name_race_ethnicity, 
         credential_name = training_track, 
         educ_highest = educational_level, 
         credential_start_date = start_date,
         credential_award_date = end_date, 
         credential_name = training_track, 
         custom_attribute_1 = learning_environment,
         custom_attribute_2 = national_flag, 
         custom_attribute_3 = client_account_name,
         custom_attribute_4 = special_initiative, 
         custom_attribute_5 = start_date_year, 
         custom_attribute_6 = exam_boolean_flag,
         custom_attribute_7 = exam_category,
         custom_attribute_8 = stacked_credential
         
         ) %>% 
  select(record_id, 
        first_name, 
         middle_name, 
         last_name, 
         age_bucket_at_cred_award, 
         street_line_1, 
         city, 
         state, 
         zipcode, 
         ssn, 
         gender,
         race_ethnicity, 
         credential_name, 
         credential_award_date,
         educ_highest, 
         custom_attribute_1, 
         custom_attribute_2, 
         custom_attribute_3, 
         custom_attribute_4, 
         custom_attribute_5, 
         custom_attribute_6, 
         custom_attribute_7, 
         custom_attribute_8,
         tx_date, 
         organization_name) %>% 
    mutate(across(where(is.character), ~ if_else(is.na(.), "", .)))

```


# Exporting in Equifax format (golden record)

```{r}
write.csv(stacked_credential_table, "stacked_credential_table.csv")
write.table(per_scholas_final, "PerScholas_20241111.csv", 
            row.names = FALSE, sep = "|", quote = TRUE)
```
