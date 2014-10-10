

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



import xlrd
import csv
import os 
import sys


def identify_files():#=locationInput()[0]): 
    """this searches within a directory and returns 0 or more."""
    #regarding if statement, maybe include campusList?
    in_path = raw_input("enter full path for location of folder with files needing renaming:----> ")    
    file_list = os.listdir(in_path)
    for f in file_list:
        if ".xls" in f:
            needs_work.append(f)
    return (needs_work,in_path) 
    #this returns a list with 2 parts: identify_files()[0] = files in a list; identify_files()[1]:  the path


def xls_to_csv():
    #This should open each xls and print each file name of that excel but with out ".xls"
    fileInfo=identify_files()
    for file in fileInfo[0]:
        book = xlrd.open_workbook(fileInfo[1] + "\\" + file)        
        sh = book.sheet_by_name()
        newCSV = open(str(file)[:-3] + "csv", 'wb')
        wr = csv.writer(newCSV, quoting=csv.QUOTE_ALL)
        for rownum in xrange(sh.nrows):
            wr.writerow(sh.row_values(rownum))
        
        newCSV.close()
        


"""def csv_from_excel():
    for file in identify_files()[0]:
        workbook = xlrd.open_workbook(file)
        all_worksheets = workbook.sheet_names()[0]
        for worksheet_name in all_worksheets:
            #worksheet = workbook.sheet_by_name(worksheet_name)
            your_csv_file = open(''.join([worksheet_name,'.csv']), 'wb')
            wr = csv.writer(your_csv_file, quoting=csv.QUOTE_ALL)
            your_csv_file.close()
"""




            
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