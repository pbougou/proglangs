#!/usr/bin/env python

from lxml import html
import requests
import sys

url   = "http://courses.softlab.ntua.gr/pl2/2016b/exercises/funge.php"
if(len(sys.argv) == 2):
    url = sys.argv[1]

def getpowers(x):
    powers = []
    i = 1
    while i <= x:
        if i & x:
            powers.append(i)
        i <<= 1
    return powers

def getBefungeNum(num):
    listofpowers = getpowers(num)
    befprog  = '!'
    initial  = len(listofpowers)
    sentinel = max(listofpowers)
    
    cnt = 1
    
    while(cnt < sentinel):
        if(listofpowers[0] == cnt):
            befprog += '::+'
            listofpowers = listofpowers[1:]
        else:
            befprog += ':+'

        cnt += cnt
        
    befprog += '+' * initial + '.@'
    proglen = len(befprog)
    
    if(proglen > 85):
        first, second = befprog[:proglen/2], befprog[proglen/2:]
        if(proglen % 2 == 1):
            befprog = '$' + first + 'v\r\n' + second[::-1] + '<'
        else:
            befprog = first + 'v\r\n' + second[::-1] + '<'


    return befprog

# print getBefungeNum(545434127)

s = requests.Session()

for i in xrange(10):
    page  = s.get(url)
    # print page.content
    tree  = html.fromstring(page.content)
    dirty = tree.xpath('//span[@class="question"]/text()')
    num   = int(dirty[0])
    bfprog = getBefungeNum(num)
    print 'Round ' + str(i+1) + ',' + 'number ' + str(num)  
    print 'Submitted Solution:\n' + bfprog
    r = s.post(str(url), data = {'submit': "Submit", 'program' : bfprog})
    # print r.content
    tree  = html.fromstring(r.content)
    right = tree.xpath('//p[@class="right"]/text()')
    wrong = tree.xpath('//p[@class="wrong"]/text()')
    if(len(right) > 0):
        print right[0]
    elif(len(wrong) > 0):
        print wrong[0] 
    else:
        print "Server must decide if the answer is right or wrong..."
    r = s.post(str(url), data={'reset' : "reset"})

s.close()

