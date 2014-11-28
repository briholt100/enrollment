

"""
Seattle Colleges report enrollments via web,
but it's clunky and divided by campus and quarter.
The goal is to make a file name look like:
"CentralSummer2013Enrollments"
"""

""""
First you must download the correct xls files.
After generating an enrollment report for a given quarter/campus,
do a copy paste into a worksheet. Do this because
the 2nd line of the worksheet will contain the campus,
quarter, year, and date accessed. This can be used to create a file name"""

import xlrd
import csv
import os 
import re
import io
needs_work=[]



def identify_files():#=locationInput()[0]): 
    """this searches within a directory and returns 0 or more."""
    #regarding if statement, maybe include campusList?
    in_path = raw_input("enter full path for location of folder with files needing renaming:----> ")    
    file_list = os.listdir(in_path)
    for f in file_list:
        if ".xls" in f:
            needs_work.append(f)
    return (needs_work,in_path) 
    #this returns a list with 2 parts: identify_files()[0] = 
    #files in a list; identify_files()[1]:  the path

def xls_to_csv():
    #this opens xls file, identifies first worksheet, and iterates 
    #through each line creating a new file, saved in the original 
    #folder of xls files.
    print "There are some important content characteristics.\n\n1. the file must have the following information at \nthe first record of the file: 'south - FALL 13 :' This is because\nthis program uses this info to make a file name.\n\n"

    fileInfo=identify_files()    
    os.mkdir(os.path.join(fileInfo[1],"processed"),)    
    for file in fileInfo[0]:
        book = xlrd.open_workbook(os.path.join(fileInfo[1],file))        
        sheet = book.sheet_names()[0]
        worksheet = book.sheet_by_name(sheet) 
        campusInfo = str(worksheet.row(1)[0])
        campusInfo = campusInfo[7:]   
        print campusInfo
        findColon = re.search(":",campusInfo) 
            #this previous command creates an object, requiring the next 
                #step below using the .group function
        colon_strt= findColon.start() #isolates the first colon
        colon_end= findColon.end() #isolates the first colon
        yr2 = campusInfo[(colon_strt- 3):(colon_end-2)] #returns a 2 digit year code eg, 2012 -> 12
        findHyphen = re.search("-",campusInfo) #this command creates an 
                    #object, requiring the next step 
                    #below using the .group function
        hyphen_end= findHyphen.end() #isolates the first colon
        quarter = campusInfo[hyphen_end+1:] #This will read the first or 
                #2nd letter of string to determine whether fall or winter, etc.
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
        elif campusInfo.lower()[1] =="v":
            campus = "SVI"              
        else:
            campus = "UNKNOWN"
        
        print "What follows are the total number of classes from " + campus  + " in " + quarter + " during year " + yr2
        #the next line will change str(file) into the new filename 
            #CampusTermYearEnrollments.csv and will look like quarter + yr2 + campus +"Enrollments"
        newCSV = open(os.path.join(fileInfo[1],campus+quarter+"20"+yr2+"Enrollments.csv"), 'wb')        
        prefix = quarter + " " + "20"+yr2 + "," + campus + ","
        wr = csv.writer(newCSV, quoting=csv.QUOTE_ALL)
        for rownum in xrange(worksheet.nrows):
            wr.writerow(worksheet.row_values(rownum))
        newCSV.close()
        currentFile =os.path.join(fileInfo[1],campus+quarter+"20"+yr2+"Enrollments.csv")
        outfile_store= os.path.join(fileInfo[1],"processed","final" + campus+quarter+"20"+yr2+"Enrollments.csv")
        with io.open(currentFile, encoding='windows-1252', errors='replace') as infile, open(outfile_store, "a") as outfile: 
            i=0            
            while True:
                i+=1                
                line=infile.readline() #this reads each and every line of file into a variable "line"
                #line=line.replace('\xa0', '').encode('utf-8') #this replaces the unicode character
                if not line: break
                m = re.match("\"[0-9]{1,4}",line) #all data is preceded by a number; no other rows have a number at the beginning
                if m is not None:
                   outfile.write(prefix + line) # This writes the new string""" 
            print i
            infile.close()
    print "You may now want to consider using cleanUp() to remove \n CSV files from this directory. The function tidy() could\n also be used to make 1 largefile."



def tidy():  #Creates from many CSV one file called "tidy"
    tidy_path = raw_input("enter full path for location of folder with files to tidy:----> ")
    file_list = os.listdir(tidy_path)
    outfile_store= os.path.join(tidy_path,"tidyEnrollments.csv")
    for file in file_list:
        if "COLUMN" in file:
            with open(os.path.join(tidy_path,file),) as infile, open(outfile_store, "a") as outfile: 
                while True:
                    line=infile.readline() 
                    if not line: break
                    outfile.write(line)
                infile.close()
    
                   
                    
def cleanUp(): #searches for non tidy csv files and deletes them. BE CAREFUL
    in_path = raw_input("enter full path for location to clean:----> ")
    file_list = os.listdir(in_path)
    i=0    
    for f in file_list:
        if "tidy" not in f:
                if ".csv" in f:
                    i+=1            
                    os.remove(os.path.join(in_path,f))
    print "The number of files removed is "+ str(i)


###
###  The following addCol searches the number of columns to add an extra 
###    one after "start time"
###  Some of the older files don't have "end time", while newer ones do


def addCol():
    print "\nBe sure that the columns you are processing \nare of the correct number (15 is the min)"
    in_path = raw_input("enter full path for location of files\n needing an extra column:----> ")
    file_list = os.listdir(in_path)
    i=0
    print "\n\nThese are the files to be analyzed \n for a need of an extra column: \n\n " + str(file_list)
    for f in file_list:
        outfile_store= os.path.join(in_path,"COLUMN"+f)            
        with io.open(os.path.join(in_path,f),"r+",encoding='windows-1252', \
                errors='replace') as infile, open(outfile_store, "a") as outfile:
            while True:
                i+=1            
                line=infile.readline()
                if not line: break            
                columns = line.split('\",\"')
                if len(columns) <= 15:
                    columns.insert(6, " ")
                    outfile.write('\",\"'.join(columns))                
                else:
                    outfile.write(line)
