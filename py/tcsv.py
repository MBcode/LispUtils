#!/usr/bin/env python3
#mike.bobak@gmail
#alias pdf2csv '~/bin/tcsv.py \!*'
import PDFContentConverter as pc

def file_base(fn):
    "the base part of base.txt"
    import os
    st=os.path.splitext(fn)
    #add2log(f'fb:st={st}')
    return st[0]

if __name__ == '__main__':
    import sys
    if(len(sys.argv)>1):
        fn = sys.argv[1]
        fnb=file_base(fn)
        converter = pc.PDFContentConverter(fn)
        result = converter.pdf2pandas()
        #print(result)
        fc=f"{fnb}.csv"
        result.to_csv(fc, encoding='utf-8', index=False)

