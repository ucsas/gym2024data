import PyPDF2
import os
import csv

pdf_folder = "C:\\Users\\calvi\\OneDrive\\Documents\\DataChallenge\\gym2024data-main\\pdfs_2023\\olympics_aa"

pdf_files = [f for f in os.listdir(pdf_folder) if f.endswith('.pdf')]
print(len(pdf_files))

list_of_dicts = []
for file in pdf_files:
    pdf_path = os.path.join(pdf_folder, file)
    with open(pdf_path, 'rb') as pdfFileObj:

        pdfReader = PyPDF2.PdfReader(pdfFileObj)

        #print(len(pdfReader.pages))
        
        for i in range(len(pdfReader.pages)):
            pageObj = pdfReader.pages[i]

            
            entire_text = pageObj.extract_text()
            #print(entire_text)
            lines = entire_text.splitlines()

            splitted = []
            temp_list = []
            count = 0
            
            for line in lines:
                count += 1
                
                #print(line)
                if not((line[0].isdigit() and line[2].isalpha()) or (line[0].isdigit() and line[1].isdigit() and line[3].isalpha())):
                    temp_list.append(line)
                else:
                    #print(temp_list)
                    splitted.append(temp_list)
                    #print(splitted)
                    temp_list = []
                    temp_list.append(line)

                if len(lines) == count:
                    splitted.append(temp_list)


            #print(splitted)

            header = splitted[0]
            date = header[3]
            
            #hardcoded competition and location variable
            competition = "Olympic Games"
            location = "Tokyo, Japan"
            gender = header[5].split("'")[0].strip()
            if gender == "Women":
                gender_init = "W"
            else:
                gender_init = "M"
            if "Qualification" in header[7].strip():
                round = "qual"
            else:
                round = "final"

            #print(splitted[24])
            for i in range(1, len(splitted)):
                #print(splitted[i][0])
                name_line = splitted[i][0].split()
                last_name = name_line[1]
                #print(last_name)
                first_name = name_line[2]
                country = name_line[-3]
                vt_score = splitted[i][1].split()[1]
                vt_d_score = splitted[i][2]
                vt_e_score = splitted[i][3]
                adder = 0
                if "(" not in splitted[i][4]:
                    vt_penalty = splitted[i][4]
                    vt_rank = splitted[i][5 + adder]
                    adder += 1
                else:
                    vt_penalty = "NA"
                    vt_rank = splitted[i][4 + adder]
                ue_score = splitted[i][5 + adder]
                ue_d_score = splitted[i][6 + adder]
                ue_e_score = splitted[i][7 + adder]
                if "(" not in splitted[i][8 + adder]:
                    ue_penalty = splitted[i][8 + adder]
                    ue_rank = splitted[i][9 + adder]
                    adder += 1
                else:
                    ue_penalty = "NA"
                    ue_rank = splitted[i][8 + adder]
                bb_score = splitted[i][9 + adder]
                bb_d_score = splitted[i][10 + adder]
                bb_e_score = splitted[i][11 + adder]
                if "(" not in splitted[i][12 + adder]:
                    bb_penalty = splitted[i][12 + adder]
                    bb_rank = splitted[i][13 + adder]
                    adder += 1
                else:
                    bb_penalty = "NA"
                    bb_rank = splitted[i][12 + adder]
                fx_score = splitted[i][13 + adder]
                fx_d_score = splitted[i][14 + adder]
                fx_e_score = splitted[i][15 + adder]
                if "(" not in splitted[i][16 + adder]:
                    fx_penalty = splitted[i][16 + adder]
                    fx_rank = splitted[i][17 + adder]
                    adder += 1
                else:
                    fx_penalty = "NA"
                    fx_rank = splitted[i][16 + adder]
                
                vt_dict = {
                    "FirstName": first_name, 
                    "LastName": last_name, 
                    "Gender": gender, 
                    "Country": country,
                    "Date": date,
                    "Competition": competition,
                    "Round": round,
                    "Location": location,
                    "Apparatus": "VT",
                    "Rank": vt_rank,
                    "D-Score": vt_d_score,
                    "E-Score": vt_e_score,
                    "Penalty": vt_penalty,
                    "Score": vt_score
                    }

                ue_dict = {
                    "FirstName": first_name, 
                    "LastName": last_name, 
                    "Gender": gender, 
                    "Country": country,
                    "Date": date,
                    "Competition": competition,
                    "Round": round,
                    "Location": location,
                    "Apparatus": "UE",
                    "Rank": ue_rank,
                    "D-Score": ue_d_score,
                    "E-Score": ue_e_score,
                    "Penalty": ue_penalty,
                    "Score": ue_score
                    }
                
                bb_dict = {
                    "FirstName": first_name, 
                    "LastName": last_name, 
                    "Gender": gender, 
                    "Country": country,
                    "Date": date,
                    "Competition": competition,
                    "Round": round,
                    "Location": location,
                    "Apparatus": "BB",
                    "Rank": bb_rank,
                    "D-Score": bb_d_score,
                    "E-Score": bb_e_score,
                    "Penalty": bb_penalty,
                    "Score": bb_score
                    }
                
                fx_dict = {
                    "FirstName": first_name, 
                    "LastName": last_name, 
                    "Gender": gender, 
                    "Country": country,
                    "Date": date,
                    "Competition": competition,
                    "Round": round,
                    "Location": location,
                    "Apparatus": "FX",
                    "Rank": fx_rank,
                    "D-Score": fx_d_score,
                    "E-Score": fx_e_score,
                    "Penalty": fx_penalty,
                    "Score": fx_score
                    }

                list_of_dicts.append(vt_dict)
                list_of_dicts.append(ue_dict)
                list_of_dicts.append(bb_dict)
                list_of_dicts.append(fx_dict)
    

with open('output.csv', 'w', newline='') as csvfile:
    # Get the keys in the first dictionary to use as the field names
    fieldnames = list_of_dicts[0].keys()

    writer = csv.DictWriter(csvfile, fieldnames=fieldnames)

    writer.writeheader()  # write the header

    # Write the dictionaries as rows
    for data in list_of_dicts:
        writer.writerow(data)

