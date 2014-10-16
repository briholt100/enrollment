

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

#outPath = raw_input("enter full path for the outfile:----> ")
needs_work=[]



import xlrd
import csv
import os 
import re


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
    #this opens xls file, identifies first worksheet, and iterates through each line creating a new file, saved in the original folder of xls files.  a bonus would be to then remove those .xls files. 
    fileInfo=identify_files()
    for file in fileInfo[0]:
        book = xlrd.open_workbook(fileInfo[1] + "\\" + file)        
        sheet = book.sheet_names()[0]
        worksheet = book.sheet_by_name(sheet) 
        campusInfo = str(worksheet.row(1)[0])
        campusInfo = campusInfo[7:]   
        print campusInfo
        findColon = re.search(":",campusInfo) #this command creates an object, requiring the next step below using the .group function
        colon_strt= findColon.start() #isolates the first colon
        colon_end= findColon.end() #isolates the first colon
        yr2 = campusInfo[(colon_strt- 3):(colon_end-2)] #returns a 2 digit year code eg, 2012 -> 12
        findHyphen = re.search("-",campusInfo) #this command creates an object, requiring the next step below using the .group function
        hyphen_end= findHyphen.end() #isolates the first colon
        quarter = campusInfo[hyphen_end+1:] #This will read the first or 2nd letter of string to determine whether fall or winter, etc.
        if quarter.lower()[0] =="f":
            quarter = "Fall"      
        elif quarter.lower()[0] =="w":
            quarter = "Winter"      
        elif quarter.lower()[1] =="p":
            quarter = "Spring"
        else:
            quarter = "Summer"      
        #same as above but for campus.
        if campusInfo.lower()[0] =="c":
            campus = "Central"      
        elif campusInfo.lower()[0] =="n":
            campus = "North"      
        elif campusInfo.lower()[1] =="o":
            campus = "South"
        else:
            campus = "SVI"      
        print campus  + " in " + quarter + " in year " + yr2
        #the next line will change str(file) into the new filename CampusTermYearEnrollments.csv
        #This will look like quarter + yr2 + campus +"Enrollments"
        newCSV = open(os.path.join(fileInfo[1],campus+quarter+"20"+yr2+"Enrollments.csv"), 'wb')        
        prefix = quarter + " " + yr2 + " " + campus + ","
        wr = csv.writer(newCSV, quoting=csv.QUOTE_ALL)
        for rownum in xrange(worksheet.nrows):
            wr.writerow(worksheet.row_values(rownum))
        newCSV.close()

        #experimental line  The problem below is that it returns an empty file. Line 90:94 works.  Does match?
        currentFile =os.path.join(fileInfo[1],campus+quarter+"20"+yr2+"Enrollments.csv")
        outfile_store= os.path.join(fileInfo[1],"final" + campus+quarter+"20"+yr2+"Enrollments.csv")
        with open(currentFile,) as infile, open(outfile_store, "a") as outfile: # infile and outfile are temp/local variables
            while True:
                line=infile.readline() #this reads each and every line of file into a variable "line"
                print line
                line=line.replace('\xa0', '').encode('utf-8') #this replaces the unicode character
                print line
                if not line: break
                match = re.findall("^[0-9]{1,4}",line) #the wanted data in these file begins with a 4 digit item code. No other lines have this bx.
                if match:
                    outfile.write(prefix + line) # <<== I must use the fileNameParser to obtain these values. This writes the new string info to the beginning of each wanted line and saves to outfile, which points to the actual output-filename at top of code