

"""
Seattle Colleges report enrollments via web,
but it's clunky and divided by campus and quarter.
So, I need to make a consistent file name that
separates the information in order to combine it
into a tidy, workable csv, titled like:
"CentralSummer2013Enrollments"
"""

""""
After generating an enrollment report for a given quarter/campus,
do a copy paste into a worksheet. Do this because
the 2nd line of the worksheet will contain the campus,
quarter, year, and date accessed. This can be used to create a file name"""
""" some resources 
http://stackoverflow.com/questions/354883/how-do-you-return-multiple-values-in-python
http://stackoverflow.com/questions/986006/how-do-i-pass-a-variable-by-reference"""

#Identify the folder
#in_path = ""
#outPath = raw_input("enter full path for the outfile:----> ")
needs_work=[]


#import xlrd
#import csv
import os

def identify_files():#=locationInput()[0]): 
    """this searches within a directory and returns 0 or more."""
    #regarding if statement, maybe include campusList?
    in_path = raw_input("enter full path for location of folder with files needing renaming:----> ")    
    file_list = os.listdir(in_path)
    for f in file_list:
        if ".xls" in f:
            needs_work.append(f)
    return (needs_work,in_path) #this returns a list, firs the files (in a list), then the final item is the path


#Open and save .xls as csv using xld
"""file= open('out.csv', 'wb')
wr = csv.writer(file, quoting=csv.QUOTE_ALL)
book=xlrd.open_workbook("F.xls")
sheet=book.sheet_by_index(0)
for sheet in book.sheets():

##for alternative, see Andi's post: http://stackoverflow.com/questions/9884353/xls-to-csv-convertor



#close file
#open new .csv
#read appropriate line (maybe just the first 3 lines)
#pull campus, quarter, year out of appropriate line
#rename
#close file

def make_name(year):
    school = ["Central", "North", "South"]
    quarter = ["Summer", "Fall", "Winter", "Spring"]
    for college in school:
        or term in quarter:
    print college  term  str(year)  "enrollments"
    """