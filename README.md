## Column Transformations for Per Scholas

1. **Variable Names**
   - Applied `clean_names()` to standardize all column names to snake_case format

2. **City**
   - Converted all values to uppercase
   - Set entries containing numbers (apartment/zip codes) to NA
   - Removed `<*BR*>` characters

3. **SSN**
   - Standardized format to XXX-XX-XXXX
   - Set invalid entries to NA:
     - Numbers not 9 digits long
     - All zeros
     - All asterisks
   - Added proper formatting to valid 9-digit numbers

4. **Age**
   - Removed invalid birth dates (after 2008)

5. **Zip Codes**
   - Standardized to XXXXX or XXXXX-XXXX format
   - Truncated XXXXX-X format to first 5 digits
   - Removed erroneous strings (e.g., '&amp')

6. **National Flag**
   - Created boolean flag for campus location "National"

7. **Stacked Credentials**
   - Created new column combining all user credentials
   - Counted only final credential achieved per user
   - Created summary table grouping by program and certificate

8. **Age Groups**
   - Created age brackets based on credential award date:
     - Under 18
     - 18-24
     - 25-34
     - 35-44
     - 45-54
     - 55-64
     - 65 and over

9. **Exam Status**
   - Added boolean flag for exam completion
   - Accounts for multiple exam attempts

10. **Final Steps**
    - Renamed columns per golden record standard
    - Exported with '|' delimiter
