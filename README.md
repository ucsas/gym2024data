# gym2024data

The goal for ‘gym2024data’ is to scrap data of Artistic Gymnastics results from PDFs into CSVs.

## Instructions on Output

The ideal data structure would be separate CSV files for Men and Women consisting of the scores for all the routines performed in the set of competitions in 2022 and 2023.

Each row of data would have the following fields:

First Name\
Last Name\
Country\
Competition Date\
Competition Name\
Competition Round (e.g., Qualifying, Apparatus Final)\
Competition Location (City and Country)\
Apparatus\
Placement on specified Apparatus in a given round\
D-Score (Difficulty)\
E-Score (Execution)\
Penalty (Pen or ND, which is a Neutral Deduction)\
Score (D-Score + E-Score – Penalty)


## Other Challenges

Some other challenges with the data in general:

1. Name may be listed as FirstName LastName or LastName FirstName. If one of the names is capitalized, it is the last (family) name.
2. The scores may be listed as D-Score, then E-Score, or in the opposite order, so the column header must be checked when scraping.
3. The scores may be listed using decimal points (as done in the USA) or commas (as done in some European countries and other parts of the world)

## Data Source

The International Gymnastics Federation Archive
https://www.gymnastics.sport/site/events/searchresults.php

2023 European Artistic Gymnastics Championship\
https://gymnasticsresults.com

2023 Pan Am Artistic Gymnastics Championships\
https://usagym.org/results/2023/

2020 Summer Olympics\
https://en.wikipedia.org/wiki/Gymnastics_at_the_2020_Summer_Olympics
https://web.archive.org/web/20210811113649/https://olympics.com/tokyo-2020/olympic-games/resOG2020-/pdf/OG2020-/GAR/OG2020-_GAR_B99_GAR-------------------------------.pdf

## Acronym Convention for Naming
BB = balanced beam\
VT = vault\
FX = floor exercise\
UB = uneven bars\
AA = all around\
RG = rings\
PH = pommel horse\
PB = parallel bars\
HB = horizontal bars
