import requests
from bs4 import BeautifulSoup
from fake_useragent import UserAgent

ua = UserAgent(verify_ssl=False, use_cache_server=True)
userAgent = ua.random

url = 'https://kr.investing.com/currencies/usd-krw-chart'
selector = '#last_last'

headers = {
    'User-Agent': ua.chrome,
    'Content-Type': 'text/html; charset=utf-8'
}

result.Value = ''
try:
    resp = requests.get(url, headers=headers, timeout=10)
    if resp.status_code == 200:            
        soup = BeautifulSoup(resp.text, "lxml")            
        tags = soup.select(selector)
        data = tags[0]                        
        result.Value = data.text
        
except Exception as e:
    print('except : ', e )

#print( result )
