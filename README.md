# gym2024data

The goal for gym2024data is to scrap data of Artistic Gymnastics results from PDFs into CSVs.

## Instructions on Output

The ideal data structure would be separate CSV files for Men and Women 
consisting of the scores for all the routines 
performed in the set of competitions in 2022 and 2023.

Each row of data would have the following fields:

First Name  
Last Name  
Country  
Competition Date  
Competition Name  
Competition Round (e.g., Qualifying, Apparatus Final)  
Competition Location (City and Country)  
Apparatus  
Placement on specified Apparatus in a given round  
D-Score (Difficulty)  
E-Score (Execution)  
Penalty (Pen or ND, which is a Neutral Deduction)  
Score (D-Score + E-Score – Penalty)


## Other Challenges

Some other challenges with the data in general:

1. Name may be listed as FirstName LastName or LastName FirstName. If one of the names is capitalized, it is the last (family) name.
2. The scores may be listed as D-Score, then E-Score, or in the opposite order, so the column header must be checked when scraping.
3. The scores may be listed using decimal points (as done in the USA) or commas (as done in some European countries and other parts of the world)
