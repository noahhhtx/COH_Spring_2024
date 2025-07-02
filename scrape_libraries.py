import requests
from bs4 import BeautifulSoup
import re
import pandas as pd

hpl_url = "https://houstonlibrary.org/all-locations"

headers = {"User-Agent": "Mozilla/5.0 (Windows NT 10.0; Win64; x64)" }
page = requests.get(hpl_url, headers=headers)
print(page.status_code)
soup = BeautifulSoup(page.content, "html.parser")

rows_parsed = []

for row in soup.select(".card-body"):
    name="UNKNOWN"
    address="UNKNOWN"
    zip = "UNKNOWN"
    try:
        name = row.find("b").get_text()
    except:
        print("Name not found.")
    try:
        address = row.find("a", alt=re.compile("Navigate to", re.I)).find("b").get_text()
        zip = address[-5:]
    except:
        print("Address not found.")
    rows_parsed.append([name,address,zip,"HPL"])

page.close()

hcpl_url = "https://hcpl.bibliocommons.com/v2/locations"
page = requests.get(hcpl_url, headers=headers)
print(page.status_code)
soup = BeautifulSoup(page.content, "html.parser")

for row in soup.select(".cp-location-item-info"):
    name = "UNKNOWN"
    address = "UNKNOWN"
    zip = "UNKNOWN"
    try:
        name = row.find("h2").find("a").get_text()
    except:
        print("Name not found.")
    if name == "Administrative Offices":
        continue
    try:
        addresses = row.find("div", class_="cp-location-address").findChildren()
        address = addresses[0].get_text() + " " + addresses[1].get_text()
        zip = address[-5:]
    except:
        print("Address not found.")
    rows_parsed.append([name, address, zip,"HCPL"])


df = pd.DataFrame(rows_parsed,columns=["Name","Address","ZIP","System"])
df.to_csv("./geographic_data/houston_libraries.csv", index=False)
