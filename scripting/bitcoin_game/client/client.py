# python 2.7, ubuntu 17.10

from lxml import html
import requests
import os, binascii
import sys
import hashlib

def get_bitcoin_value(bitcoin_string):
    hexPrice = bitcoin_string[4:8]
    return int(hexPrice, 16) / 100.0

def find_bitcoin(magic_code):
    while True:
        answer = binascii.b2a_hex(os.urandom(32))

        s = answer.decode("hex")
        first  = hashlib.sha256(s)
        second = hashlib.sha256()
        second.update(first.digest())

        if second.hexdigest().startswith(magic_code):
            return answer


url = "http://courses.softlab.ntua.gr/pl2/2017b/exercises/gimmeabitcoin.php"
if(len(sys.argv) == 2):
    url = sys.argv[1]
elif(len(sys.argv) == 3):
    url = sys.argv[1]
    magic = sys.argv[2]
    print find_bitcoin(magic)

s   = requests.Session()
sum = 0
i   = 0
while sum <= 2000:
    page    = s.get(url)
    tree    = html.fromstring(page.content)
    dirty   = tree.xpath('//span[@class="question"]/text()')
    num     = dirty[0].split()[4]
    bitcoin = find_bitcoin(num)
    value   = get_bitcoin_value(bitcoin)
    sum += value
    print 'Round ' + str(i + 1) + ', ' + 'magic code: ' + str(num)
    print bitcoin + ' ' + str(value)
    r = s.post(str(url), data = {'submit': "submit",'answer': bitcoin})
    tree = html.fromstring(r.content)
    right = tree.xpath('//p[@class="right"]/text()')
    wrong = tree.xpath('//p[@class="wrong"]/text()')
    if len(right) > 0:
        print right[0]
    elif len(wrong) > 0:
        print wrong[0]
    else:
        print 'The impossible happenned'
    r = s.post(str(url), data = {'continue': "continue"})

    i += 1

print 'We are even :D'
s.close()
