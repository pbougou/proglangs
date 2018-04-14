#!/usr/bin/env python

from signal import alarm
from flask import Flask, request, Response
from threading import Timer
import subprocess, os, sys, time

def bfvmexec(input):
    path = './.inp'
    fd = open(path, 'w')
    fd.write(str(input))
    fd.close()
    p = subprocess.Popen(['./bfvm', path], stdout = subprocess.PIPE, stderr = subprocess.PIPE)

    timeout = False
    kill = lambda process: process.kill()
    timectl = Timer(1, kill, [p])
    try:
	timectl.start()
	stdout, stderr = p.communicate()
    finally:
	timectl.cancel()
    
    os.remove(path)
    return stdout 

def check_shape(input):
    input = input.split('\n')
    input = list(map(lambda x: len(x), input))
    check_val = input[0]
    linenum = 0
    for i in input:
        if i != check_val:
            if input[(linenum+1):] != []:
                return linenum + 1
        linenum = linenum + 1
    return 0

app = Flask(__name__)
@app.route('/befunge93-api/', methods = ['POST'])
def api():
    input = request.data
    linenum = check_shape(input)
    if linenum > 0:
        return Response(response = 'Program is not a rectangle: check line ' + str(linenum) + '.\n', status = '400')

    url = request.full_path
    permitted = True
    if(url.split('?')[1] == 'restrict'):
        forbidden = ['0', '1', '2', '3','4','5','6','7', '8', '9', 'g', 'p', '?', '&', '~', '"']
        for i in input:
            if i in forbidden:
                permitted = False
    
    if(not permitted):
        return Response(response = 'Program contains illegal characters\n', status = '400')
    
    start_time = time.time() 
    res = bfvmexec(input)
    if(time.time() - start_time > 1):
    	return Response(response = 'Timeout' + '\n', status = '400')
    else:
    	return Response(response = str(res) + '\n', status = '200')

if(__name__ == "__main__"):
    app.run()


