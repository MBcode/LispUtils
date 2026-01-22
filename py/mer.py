#!/usr/bin/env python3
# #!/home/bobak/mambaforge/bin/python3
#convenience for https://github.com/lasigeBioTM/MER
#takes file or copy buffer to get entity tagged mike.bobak@gmail
#-need to make the strings safe
import sys
import os
def os_system(cs):
    "run w/o needing ret value"
    os.system(cs)
    #add2log(cs)

def os_system_(cs):
    "system call w/return value"
    s=os.popen(cs).read()
    #add2log(cs)
    return s

import pyperclip as p


def get_ent(n=None): #could send in lex
    "run MER to get the concept tagging of the text"
    lex=os.getenv("lex")
    if lex==None:
        lex="csLex"
    if n==None:
        ne = p.waitForPaste()
    else:
        #ne = p.waitForNewPaste(45)
        ne=n #send in text now
    txt=ne.replace('(',' ').replace(')',' ').replace('&','+')
    cs = f'./get_entities.sh "{txt}" {lex}'
    print(cs)
    r = os_system_(cs)
    print(r)
    return r

def ccwc():
    "wc on cp buffer"
    #cs = f'pbpaste |wc'
    cs = f'xclip -selection clipboard -o |wc'
    n = os_system_(cs)
    return n

def ccc(fn):
    "file to cp buffer"
    #cs = f'cat {fn} |pbcopy'
    cs = f'cat {fn} |xclip -selection clipboard'
    #cs = f'cat {fn} |xclip -i'
    #cs = f'xclip -i {fn}'
    #cs = f'cat {fn} |./pbcopy'
    print(cs)
    #r = os_system_(cs)
    #print(f'{fn} in cp buffer,{r}')
    os_system(cs)
    print(f'{fn} in cp buffer')
    #print(f'{fn} in cp buffer,{r},wc={wc}')
    wc = ccwc()
    print(f'wc={wc}')

def maccp(s):
    cs = f'echo {s} |xclip -selection clipboard'

def say(txt): #not hearing this way/fix
    cs = f' echo "{txt}" | espeak --stdin -s 245 -p 70'
    #print(cs)
    os_system(cs)

def ccsay():
    "read the cp buffer aloud"
    #cs = 'pbpaste | espeak --stdin'
    cs = f'xclip -selection clipboard -o | espeak --stdin -s 245 -p 70'
    os_system(cs)

def cccounts(s):
    "listing of concept counts"
    cs = f"echo '{s}'|cut -d'/' -f5 |sort | uniq -c |sort -rn"
    #print(cs)
    r = os_system_(cs)
    print(r)
    return r


def get_ent_fn(fn):
    "entities tagged from a file"
    ccc(fn)
    return get_ent()

def t():
    return get_ent()

def t2():
    return get_ent(45)

def get_txtfile(fn):
    "ret str from file"
    with open(fn, "r") as f:
        return f.read()

#consider ccli args, like --speak

if __name__ == '__main__':
    import sys
    if(len(sys.argv)>1):
        arg = sys.argv[1] #could send in lex, or fn for txt or txt
        print(f'arg={arg}')
        if "." in arg and len(arg)<11:
            print("open file")
            txt=get_txtfile(arg)
            r=get_ent(txt)
        else:
            print("use text")
            txt=arg
            r=get_ent(txt)

        cccounts(r)
        say(txt)
    else:
        r=get_ent()
        cccounts(r)
        ccsay()
    #print(r)
