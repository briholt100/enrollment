

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

import xlrd
import csv
import os 
import re
needs_work=[]

def cleanUp():
    in_path = raw_input("enter full path for location to clean:----> ")
    file_list = os.listdir(in_path)    
    for f in file_list:
        if ".csv" in f:
            os.remove(os.path.join(in_path,f))

def identify_files():#=locationInput()[0]): 
    """this searches within a directory and returns 0 or more."""
    #regarding if statement, maybe include campusList?
    in_path = raw_input("enter full path for location of folder with files needing renaming:----> ")    
    file_list = os.listdir(in_path)
    os.mkdir(os.path.join(in_path,"processed"),)
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
        prefix = quarter + " " + "20"+yr2 + "," + campus + ","
        wr = csv.writer(newCSV, quoting=csv.QUOTE_ALL)
        for rownum in xrange(worksheet.nrows):
            wr.writerow(worksheet.row_values(rownum))
        newCSV.close()
        currentFile =os.path.join(fileInfo[1],campus+quarter+"20"+yr2+"Enrollments.csv")
        outfile_store= os.path.join(fileInfo[1],"processed","final" + campus+quarter+"20"+yr2+"Enrollments.csv")
        with open(currentFile,) as infile, open(outfile_store, "a") as outfile: # infile and outfile are temp/local variables
            while True:
                line=infile.readline() #this reads each and every line of file into a variable "line"
                line=line.replace('\xa0', '').encode('utf-8') #this replaces the unicode character
                if not line: break
                m = re.match("\"[0-9]{1,4}",line) #the wanted data in these file begins with a 4 digit item code. No other lines have this bx.
                if m is not None:
                    print line[0:8]
                    outfile.write(prefix + line) # This writes the new string info to the beginning of each wanted line and saves to outfile
            infile.close()