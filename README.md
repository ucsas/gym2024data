# gym2024data

For more information about UCSAS 2024 USOPC DATA CHALLENGE, please visit\
https://statds.org/events/ucsas2024/challenge.html.

This repo `gym2024data` contains the scores for all the routines performed in a set of major domestic and international gymnastics competitions in 2022 and 2023.

The CSV files of clean data are in the folder **cleandata**. We also store the original PDF files from which we scrape the data in the folder **pdf** and the corresponding code files in folders **R** and **python**.



## Variable Definitions

The ideal data structure would be separate CSV files for Men and Women consisting of the scores for all the routines performed in the set of competitions in 2022 and 2023.

Each row of data represents a gymnast's results on a specific apparatus in a given round of a certain competition. There are 14 columns in total:

`FirstName`: the first name of the gymnast\
`LastName`: the last name of the gymnast\
`Gender`: the gender of the gymnast. "m" stands for men, "w" stands for women.\
`Country`: the 3-letter country code of the gymnast (You can find the compilation of country codes corresponding to their respective country names in the following link: https://en.wikipedia.org/wiki/List_of_IOC_country_codes) \
`Date`: the date when the competition happened, usually a range of days.\
`Competition`: the competition name\
`Round`: the round ("qual" stands for Qualifying, "final" for Apparatus Final, "AAfinal" and "AAqual" for All-Around Final/Qualification, and "TeamFinal" stands for Team Final)\
`Location`: Competition Location (City and Country)\
`Apparatus`: a 2-letter all-capital code representing which apparatus(event) the gymnast played in. (Details are shown in the next section)\
`Rank`: the placement on specified apparatus in a given round\
`D_Score`: the difficulty score\
`E_Score`: the execution score\
`Penalty`: a deduction score inflicted by faults or errors the gymnast made in their execution of techniques. If there is no penalty, the value are left with NA.\
`Score`: the total score (D-Score + E-Score – Penalty)


## Acronym Convention for Naming Apparatus
For women: (4 apparatus)\
BB = Balanced Beam\
VT = Vault\
FX = Floor Exercise\
UB = Uneven Bars

For men: (6 apparatus)\
VT = Vault\
SR = (Still) Rings\
PH = Pommel Horse\
PB = Parallel Bars\
HB = Horizontal Bars
FX = Floor Exercise\

## Data Source

The International Gymnastics Federation Archive\
https://www.gymnastics.sport/site/events/searchresults.php

The Gymternet\
https://thegymter.net/

2023 European Artistic Gymnastics Championship\
https://gymnasticsresults.com

2023 Pan Am Artistic Gymnastics Championships\
https://usagym.org/results/2023/

2020 Summer Olympics\
https://en.wikipedia.org/wiki/Gymnastics_at_the_2020_Summer_Olympics
https://web.archive.org/web/20210811113649/https://olympics.com/tokyo-2020/olympic-games/resOG2020-/pdf/OG2020-/GAR/OG2020-_GAR_B99_GAR-------------------------------.pdf

2022 World Cup and World Challenge Cup Results\
https://thegymter.net/2022/03/03/2022-cottbus-world-cup-results/


