import os
import requests
from bs4 import BeautifulSoup
import datetime

# Open webpage:
url = 'https://edoc.rki.de/discover?rpp=1000&etal=0&query=csv&scope=/&group_by=none&page=1&sort_by=score&order=desc'
content = requests.get(url).content

# Get the html:
soup = BeautifulSoup(content, 'html.parser')
# print(soup.prettify())

# Get all links:
all_links = soup.find_all('a', href=True)

# Loop through to find relevant links/info:
csv_urls = []
for link in all_links:
    # Check that element has title:
    title_temp = link.find('div', {'class': 'artifact-title'})

    if title_temp is not None:
        # Check that contains info about csv data:
        if 'CSV' in title_temp.next_element:
            # print(title_temp.next_element)

            # Get relevant elements and piece together urls:
            folder_temp = link['href']
            date_temp = link.find('div', {'class': 'artifact-subtitle'}).next_element

            if date_temp == '\n':
                date_temp = link.find('span', {'class': 'date'}).next_element

            link_temp = 'https://edoc.rki.de/bitstream' + folder_temp + '/' + date_temp + \
                        '_12-15_teilbare_divi_daten.csv?sequence=1&isAllowed=y'
            csv_urls.append((date_temp, link_temp))

        elif 'csv' in title_temp.next_element:
            print('LOWERCASE DETECTED')

print('Relevant urls collected')

# Order by date:
csv_urls = sorted(csv_urls, key=lambda x: datetime.datetime.strptime(x[0], '%Y-%m-%d'))
print('Urls sorted')

# Ensure relevant directories exist:
if not os.path.isdir('../data/'):
    os.mkdir('../data/')
if not os.path.isdir('../data/raw/'):
    os.mkdir('../data/raw/')

# Write urls to file:
file = open('../data/raw/hosp_dat_urls.txt', 'w')
for csv_url in csv_urls:
    file.write(csv_url[1])
    file.write('\n')
file.close()

print('Done')

# Note: Before April 24, no csv data available; pdfs with choropleth maps only
