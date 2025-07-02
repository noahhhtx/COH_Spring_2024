import requests
from bs4 import BeautifulSoup
import re
import pandas as pd

headers = {"User-Agent": "Mozilla/5.0 (Windows NT 10.0; Win64; x64)" }
rows_parsed = []

# part 1: scraping HPARD community centers
for direction in ("Northwest", "Northeast", "Southwest", "Southeast"):
    url = ("https://www.houstontx.gov/parks/communitycenters/" + direction + ".html").lower()
    print("Accessing", url)
    page = requests.get(url, headers=headers)
    print("Status Code:", page.status_code)
    if page.status_code != 200:
        continue
    soup = BeautifulSoup(page.content, "html.parser")
    for center in soup.select(".TDOffice"):
        name = "UNKNOWN"
        address = "UNKNOWN"
        zip = "UNKNOWN"
        phone = "UNKNOWN"
        email = "UNKNOWN"
        try:
            name = center.find("strong").get_text()
        except:
            print("ERROR: Name not found.")
        try:
            address = (center.find("a", href=re.compile("maps", re.I)).get_text())
            temp = address.split(",")
            address = temp[0] + ", Houston, TX" + temp[1]
            zip = address[-5:]
        except:
            print("ERROR: Address not found.")
        try:
            phone = (center.find("a", href=re.compile("maps", re.I))).next_sibling.next_sibling
            temp = ""
            for char in phone:
                if char.isdigit():
                    temp += char
            phone = temp
        except:
            print("ERROR: Phone not found.")
        try:
            email = (center.find("a", href=re.compile("mailto", re.I)).get_text())
        except:
            print("ERROR: Email not found.")
        rows_parsed.append([name, address, zip, phone, email, "HPARD Community Center", direction])

# part 2: scraping COH multi-service centers
url = "https://www.houstonhealth.org/services/multi-service-centers"
print("Accessing", url)
page = requests.get(url, headers=headers)
print("Status Code:", page.status_code)
if page.status_code == 200:
    soup = BeautifulSoup(page.content, "html.parser")
    for center in soup.find_all("div", class_=re.compile("coh-container card-content", re.I)):
        name = "UNKNOWN"
        address = "UNKNOWN"
        zip = "UNKNOWN"
        phone = "UNKNOWN"
        email = "UNKNOWN"
        try:
            name = center.find("a").get_text().strip()
        except:
            print("ERROR: Name not found.")
        try:
            address = center.find("p").get_text(separator= ", ", strip=True)
            address = address[0:address.index("-")]
            zip = address[-5:]
            address = address.replace("Texas","TX")
        except:
            print("ERROR: Address not found.")
        try:
            url_2 = "https://www.houstonhealth.org" + center.find("a").get('href')
            print("Accessing", url_2)
            subpage = requests.get(url_2, headers=headers)
            print("Status Code:", subpage.status_code)
            if subpage.status_code == 200:
                soup_2 = BeautifulSoup(subpage.content, "html.parser")
                phone = soup_2.find("a", href=re.compile("tel",re.I)).get_text()
                temp=""
                for char in phone:
                    if char.isdigit():
                        temp += char
                phone = temp
        except:
            print("ERROR: Phone not found.")
        rows_parsed.append([name, address, zip, phone, email, "Multi-Service Center", "UNKNOWN"])

# part 3: scraping Harris County community centers
commissioner_1 = "https://www.hcp1.net/CommunityCenters"
print("Accessing", commissioner_1)
page = requests.get(commissioner_1, headers=headers)
print("Status Code:", page.status_code)
if page.status_code == 200:
    soup = BeautifulSoup(page.content, "html.parser")
    for center in soup.find_all("div", class_="ccItemWrap"):
        name = "UNKNOWN"
        address = "UNKNOWN"
        zip = "UNKNOWN"
        phone = "UNKNOWN"
        email = "UNKNOWN"
        try:
            name = center.find("h4").get_text()
        except:
            print("ERROR: Name not found.")
        subpage_url = "https://www.hcp1.net" + center.find("a").get("href")
        print("Accessing", subpage_url)
        subpage = requests.get(subpage_url, headers=headers)
        print("Status Code:", subpage.status_code)
        if subpage.status_code == 200:
            soup_2 = BeautifulSoup(subpage.content, "html.parser")
            try:
                address = soup_2.find("h3",class_="amLRAdd").get_text()
                zip = address[-5:]
            except:
                print("ERROR: Address not found.")
            try:
                phone = soup_2.find("h4", class_="ccLRPhone").get_text()
                temp = ""
                for char in phone:
                    if char.isdigit():
                        temp += char
                phone = temp
                if len(phone) == 0:
                    phone="UNKNOWN"
            except:
                print("ERROR: Phone not found.")
        rows_parsed.append([name, address, zip, phone, email, "Harris County Community Center", "Precinct 1"])

commissioner_2 = "https://www.hcp2.com/Neighborhood-Services/Community-Centers"
print("Accessing", commissioner_2)
page = requests.get(commissioner_2, headers=headers)
print("Status Code:", page.status_code)
if page.status_code == 200:
    soup = BeautifulSoup(page.content, "html.parser")
    for center in soup.find_all("div",class_="ComCen-Info-Wrap"):
        name = "UNKNOWN"
        address = "UNKNOWN"
        zip = "UNKNOWN"
        phone = "UNKNOWN"
        email = "UNKNOWN"
        try:
            name = center.find("div",class_="ComCen-Title").find("h2").get_text()
        except:
            print("ERROR: Name not found.")
        try:
            address = center.find("div",class_="ComCen-Address").find("a").get_text().strip()
            zip = address[-5:]
        except:
            print("ERROR: Address not found.")
        try:
            phone = center.find("div",class_="ComCen-num").find("span").get_text()
            temp = ""
            for char in phone:
                if char.isdigit():
                    temp += char
            phone = temp
        except:
            print("ERROR: Phone not found.")
        rows_parsed.append([name, address, zip, phone, email, "Harris County Community Center", "Precinct 2"])

commissioner_3 = "https://www.pct3.com/Explore/Community-Centers"
print("Accessing", commissioner_3)
page = requests.get(commissioner_3, headers=headers)
print("Status Code:", page.status_code)
if page.status_code == 200:
    soup = BeautifulSoup(page.content, "html.parser")
    for center in soup.find_all("div",class_="ComCen-Info-Wrap"):
        name = "UNKNOWN"
        address = "UNKNOWN"
        zip = "UNKNOWN"
        phone = "UNKNOWN"
        email = "UNKNOWN"
        try:
            name = center.find("div",class_="ComCen-Title").find("h2").get_text()
        except:
            print("ERROR: Name not found.")
        try:
            address = center.find("div",class_="ComCen-Address").find("a").get_text().strip()
            address = address.replace("Â","")
            zip = address[-5:]
        except:
            print("ERROR: Address not found.")
        try:
            phone = center.find("div",class_="ComCen-num").find("span").get_text()
            temp = ""
            for char in phone:
                if char.isdigit():
                    temp += char
            phone = temp
        except:
            print("ERROR: Phone not found.")
        try:
            email = center.find("div", class_="ComCen-Contact-Info").find("a").get_text().strip()
        except:
            print("ERROR: Email not found.")
        rows_parsed.append([name, address, zip, phone, email, "Harris County Community Center", "Precinct 3"])

commissioner_4 = "https://cp4.harriscountytx.gov/Community-Centers"
print("Accessing", commissioner_4)
page = requests.get(commissioner_4, headers=headers)
print("Status Code:", page.status_code)
if page.status_code == 200:
    soup = BeautifulSoup(page.content, "html.parser")
    for center in soup.find_all("a",class_="hcp4-cc-box"):
        name = "UNKNOWN"
        address = "UNKNOWN"
        zip = "UNKNOWN"
        phone = "UNKNOWN"
        email = "UNKNOWN"
        try:
            name = center.find("h3").get_text()
        except:
            print("ERROR: Name not found.")
        subpage_url = "https://cp4.harriscountytx.gov" + center.get("href")
        print("Accessing", subpage_url)
        subpage = requests.get(subpage_url, headers=headers)
        print("Status Code:", subpage.status_code)
        if subpage.status_code == 200:
            soup_2 = BeautifulSoup(subpage.content, "html.parser")
            try:
                address = soup_2.find("div",class_="hcp4-ccd-left").find("p").get_text().strip()
                zip = address[-5:]
            except:
                print("ERROR: Address not found.")
            try:
                phone = str(soup_2.find("span",class_="hcp4-title-icons").find("a", href=re.compile("tel:", re.I)).get("href"))
                phone = phone[phone.index(":")+1:]
                temp = ""
                for char in phone:
                    if char.isdigit():
                        temp += char
                phone = temp
                if len(phone) == 0:
                    phone = "UNKNOWN"
            except:
                print("ERROR: Phone not found.")
            try:
                e = ((str(soup_2.find("span",class_="hcp4-title-icons").find("a", href=re.compile("email",re.I)).get("href"))).split("#"))[1]
                de = ""
                k = int(e[:2], 16)

                for i in range(2, len(e) - 1, 2):
                    de += chr(int(e[i:i + 2], 16) ^ k)

                email = de
            except:
                print("ERROR: Email not found.")
        rows_parsed.append([name, address, zip, phone, email, "Harris County Community Center", "Precinct 4"])

df = pd.DataFrame(rows_parsed,columns=["Name", "Address", "ZIP", "Phone", "Email", "Type", "Specification"])
df.to_csv("./geographic_data/community_centers.csv", index=False)

