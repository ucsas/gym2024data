library(pdftools)
library(tidyverse)


# define a function to extract gym data from pdf files


noc_string <- "Afghanistan AFG
Albania	ALB
Algeria	ALG
American Samoa*	ASA
Andorra	AND
Angola	ANG
Antigua and Barbuda	ANT
Argentina	ARG
Armenia	ARM
Aruba*	ARU
Australia	AUS
Australasia	ANZ
Austria	AUT
Azerbaijan	AZE
The Bahamas	BAH
Bahrain	BRN
Bangladesh	BAN
Barbados	BAR
Belarus	BLR
Belgium	BEL
Belize	BIZ
Bermuda*	BER
Benin	BEN
Bhutan	BHU
Bohemia	BOH
Bolivia	BOL
Bosnia and Herzegovina	BIH
Botswana	BOT
Brazil	BRA
British Virgin Islands*	IVB
British West Indies	BWI
Brunei	BRU
Bulgaria	BUL
Burkina Faso	BUR
Burundi	BDI
Cambodia	CAM
Cameroon	CMR
Canada	CAN
Cape Verde	CPV
Cayman Islands*	CAY
Central African Republic	CAF
Chad	CHA
Chile	CHI
China	CHN
Colombia	COL
Comoros	COM
Congo, Republic of the	CGO
Congo, Democratic	COD
Republic of the Cook Islands*	COK
Costa Rica	CRC
Cote d'Ivoire	CIV
Croatia	CRO
Cuba	CUB
Cyprus	CYP
Czechia	CZE
Czechoslovakia	TCH
Denmark	DEN
Djibouti	DJI
Dominica	DMA
Dominican Republic	DOM
East Germany	GDR
East Timor (Timor-Leste)	TLS
Ecuador	ECU
Egypt	EGY
El Salvador	ESA
Equatorial Guinea	GEQ
Eritrea	ERI
Estonia	EST
Eswatini 	SWZ
Ethiopia	ETH
Fiji	FIJ
Finland	FIN
France	FRA
Gabon	GAB
The Gambia	GAM
Georgia	GEO
Germany	GER
Ghana	GHA
Greece	GRE
Grenada	GRN
Guam*	GUM
Guatemala	GUA
Guinea	GUI
Guinea-Bissau	GBS
Guyana	GUY
Haiti	HAI
Honduras	HON
Hong Kong*	HKG
Hungary	HUN
Iceland	ISL
Individual Olympic athletes	IOA
Independent Olympic Participants	IOP
India	IND
Indonesia	INA
Iran	IRI
Iraq	IRQ
Ireland	IRL
Israel	ISR
Italy	ITA
Jamaica	JAM
Japan	JPN
Jordan	JOR
Kazakhstan	KAZ
Kenya	KEN
Kiribati	KIR
Korea, North (PDR of Korea)	PRK
Korea, South	KOR
Kosovo	KOS
Kuwait	KUW
Kyrgyzstan	KGZ
Laos	LAO
Latvia	LAT
Lebanon	LIB
Lesotho	LES
Liberia	LBR
Libya	LBA
Liechtenstein	LIE
Lithuania	LTU
Luxembourg	LUX
Madagascar	MAD
Malawi	MAW
Malaya	MAL
Malaysia	MAS
Maldives	MDV
Mali	MLI
Malta	MLT
Marshall Islands	MHL
Mauritania	MTN
Mauritius	MRI
Mexico	MEX
Micronesia (Federated States of)	FSM
Mixed Team (1896 to 1904)	ZZX
Moldova	MDA
Monaco	MON
Mongolia	MGL
Montenegro	MNE
Morocco	MAR
Mozambique	MOZ
Myanmar (Burma)	MYA
Namibia	NAM
Nauru	NRU
Nepal	NEP
Netherlands	NED
Netherlands Antilles*	AHO
New Zealand	NZL
Nicaragua	NCA
Niger	NIG
Nigeria	NGR
North Borneo	NBO
North Macedonia	MKD
North Yemen	YAR
Norway	NOR
Oman	OMA
Pakistan	PAK
Palau	PLW
Palestine*	PLE
Panama	PAN
Papua New Guinea	PNG
Paraguay	PAR
Peru	PER
Philippines	PHI
Poland	POL
Portugal	POR
Puerto Rico*	PUR
Qatar	QAT
Refugee Olympic Athletes	ROA
Republic of China	ROC
Romania	ROU
Russia	RUS
Russian Empire	RU1
Rwanda	RWA
Saar	SAA
Saint Kitts and Nevis	SKN
Saint Lucia	LCA
Saint Vincent and the Grenadines	VIN
Samoa	SAM
San Marino	SMR
Sao Tome and Principe	STP
Saudi Arabia	KSA
Senegal	SEN
Serbia	SRB
Serbia and Montenegro	SCG
Seychelles	SEY
Sierra Leone	SLE
Singapore	SIN
Slovakia	SVK
Slovenia	SLO
Solomon Islands	SOL
Somalia	SOM
South Africa	RSA
South Sudan	SSD
South Yemen	YMD
Soviet Union	URS
Spain	ESP
Sri Lanka	SRI
Sudan	SUD
Suriname	SUR
Sweden	SWE
Switzerland	SUI
Syria	SYR
Taiwan (Chinese Taipei)	TPE
Tajikistan	TJK
Tanzania	TAN
Thailand	THA
Togo	TOG
Tonga	TGA
Trinidad and Tobago	TRI
Tunisia	TUN
Türkiye	TUR
Turkmenistan	TKM
Tuvalu	TUV
Uganda	UGA
Ukraine	UKR
Unified Team	EUN
Unified Team of Germany	EUA
United Arab Emirates	UAE
United Arab Republic	RAU
United Kingdom (Great Britain)	GBR
United States	USA
Uruguay	URU
Uzbekistan	UZB
Vanuatu	VAN
Venezuela	VEN
Vietnam	VIE
Virgin Islands*	ISV
West Germany	FRG
West Indies Federation (Antilles)	ANT
Yemen	YEM
Yugoslavia	YUG
Zambia	ZAM
Zimbabwe	ZIM"

# extract country abbreviation
uppercase_parts <- regmatches(noc_string, gregexpr("\\b[A-Z]+\\b", noc_string))
noc <- unlist(uppercase_parts)

# define a pattern for qualification grade
qr <- c("Q", "R", "R1", "R2", "R3")


# special about Baku:
# 1. Rank and Bib are 1 y lower than the main row
# 2. e score comes before d score
# 3. penalty is negative
# 4. y diff between vt1 and vt2 is 20




# define a function onto a single pdf file, outputs a single data frame
# one can easily apply this function onto multiple pdf files in a directory
# when doing so, remember to separate_wider_delim at the end to get gender, round and city info from title


extract_baku <- function(pdf_path){
  table_list <- pdf_data(pdf_path)
  comps <- list()
  
  # for loop on each tibble in the list read from one pdf file
  for (j in 1:length(table_list)) {
    page <- table_list[[j]]
    y_vals <- sort(unique(page$y))
    all_gymnasts <- unique(page[which(page$text %in% noc),]$y)
    if (length(all_gymnasts) == 0) {
      break
    }
    for (k in 1:length(all_gymnasts)) {
      gymnast <- page[which(page$y == all_gymnasts[k]),] %>% arrange(x)
      name <- paste(gymnast$text[1:which(gymnast$text %in% noc)-1], collapse = " ") # combine every text before noc as name
      VT1_d <- NA
      VT1_e <- NA
      VT1_tot <- NA
      VT2_d <- NA
      VT2_e <- NA
      VT2_tot <- NA
      d <- NA
      e <- NA
      tot <- NA
      rank <- NA
      
      if ("Vault" %in% page$text) {
        gymnast_vt2 <- page[which(page$y == all_gymnasts[k]+20),] %>% arrange(x)
        rank <- page[which(page$y == all_gymnasts[k]+1),]$text[1]
        if (any(gymnast$text %in% qr)) {
          scores <- gymnast$text[(which(gymnast$text %in% noc)+2) : (which(gymnast$text %in% qr)-2)] # from E to VaultScore
          VT1_d <- scores[2]
          VT1_e <- scores[1]
          VT1_tot <- scores[length(scores)]
          VT2_d <- gymnast_vt2$text[3]
          VT2_e <- gymnast_vt2$text[2]
          VT2_tot <- gymnast_vt2$text[length(gymnast_vt2$text)]
        } else{
          scores <- gymnast$text[-c(1:(which(gymnast$text %in% noc)+1))] # from E to the end
          VT1_d <- scores[2]
          VT1_e <- scores[1]
          VT1_tot <- scores[length(scores)]
          VT2_d <- gymnast_vt2$text[3]
          VT2_e <- gymnast_vt2$text[2]
          VT2_tot <- gymnast_vt2$text[length(gymnast_vt2$text)]
        }
        
      }
      else {
        rank <- page[which(page$y == all_gymnasts[k]+1),]$text[1]
        if (any(gymnast$text %in% qr)) {
          scores <- gymnast$text[(which(gymnast$text %in% noc)+1) : (which(gymnast$text %in% qr)-1)]
          d <- scores[2]
          e <- scores[1]
          tot <- scores[length(scores)]
        } else{
          scores <- gymnast$text[-c(1:(which(gymnast$text %in% noc)+1))]
          d <- scores[2]
          e <- scores[1]
          tot <- scores[length(scores)]
        }
      }
      title <- basename(pdf_path)
      comps[[length(comps)+1]] <- data.frame(name, rank, title, d, e, tot, VT1_d, VT1_e, VT1_tot, VT2_d, VT2_e, VT2_tot)
    }
  }
  bind_rows(comps)
}


# example
vt_table <- extract_baku("baku/baku_m_qual_VT.pdf")
vt_table



