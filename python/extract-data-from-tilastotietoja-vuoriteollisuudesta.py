import requests
from fake_useragent import UserAgent
from tabula import read_pdf
import pandas as pd
import numpy as np
import os

year = 2023
filename = f"./source-data/inputs-environment/mining/mining-statistics-fi-{year}.pdf"
os.makedirs(os.path.dirname(filename), exist_ok=True)

ua_str = UserAgent().firefox

mining_stats_url = f"https://tukes.fi/documents/5470659/6373016/Tilastotietoja%20vuoriteollisuudesta%20{year}.pdf"

mining_stats_response = requests.get(mining_stats_url, headers={"User-Agent": ua_str})

with open(filename, "wb") as f:
    f.write(mining_stats_response.content)

df_temp = read_pdf(filename, stream=True)

mining_stats_messy = df_temp[0]

incomplete_colnames = mining_stats_messy.columns.to_list()
missing_part = mining_stats_messy.loc[0, :].fillna("").to_list()
complete_colnames = [x + " " + str(y) if len(y) > 0 else x for x, y in zip(incomplete_colnames, missing_part)]

mining_stats_messy.columns = complete_colnames

mining_stats_messy.drop(labels="Unnamed: 0", axis=1, inplace=True)
mining_stats_messy.drop(labels=0, axis=0, inplace=True)

def find_category_rows(this_series: pd.Series, category):
    row_of_category_statement = np.where(this_series == category)[0]
    rows_of_category_ends = np.where(this_series.str.contains("yhteensä", case=False))[0]
    row_of_category_end = rows_of_category_ends[np.min(np.where(np.greater(rows_of_category_ends, row_of_category_statement)))]
    print("The row of category statement is ", row_of_category_statement, "and it's type is ", type(row_of_category_statement))
    print("The row of category end is ", row_of_category_end, "and it's type is ", type(row_of_category_end))
    res = np.array([row_of_category_statement[0] + 1, row_of_category_end - 1])
    return res



categories = ["Metallimalmit", "Karbonaattikivet", "Muut teollisuusmineraalit", "Teollisuuskivet ja muut"]
delimiter_row_mask_1 = mining_stats_messy.loc[:, "Kaivos/Louhos Kunta"].isin(categories)
delimiter_row_mask_2 = mining_stats_messy.loc[:, "Kaivos/Louhos Kunta"].str.contains("yhteensä", case=False)
# delimiter_row_mask


mining_stats_messy.set_index("Kaivos/Louhos Kunta", inplace=True)

