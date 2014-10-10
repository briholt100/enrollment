"""needs to include a count of how many files were processed in last line of print in tidy()"""



#conceptual flow
""" I have the following code that works:
1. file_tidy(f_loc) which makes a new outfile of data with the correct campus and quarter put up front
2. filename_Parser(fileAddress) which returns the campus and quarter for a given fileAddres (csv)
3. identify_untidy(Parse_Folder) which returns a list of filenames (csv) that need tidying (the adding of campus and quarter)

So, something that takes the return of file_tidy, and then loops through each returned fileAddress and creates a
new outfile with "Tidy" in the file name."""

def tidy(): #calls other methods to tidy up CSV files from Seattlecolleges enrollment
    lines_processed = 0
    for untidy_filename in identify_untidy(Parse_Folder):
       # print untidy_filename
        f_loc=Parse_Folder + "\\" + untidy_filename
        outfile_store =outfile +"\\" +"TidyEnrollments.csv"
        file_tidy(f_loc, outfile_store) #there is no"r" or "w" before the outfile
        lines_processed += file_length(f_loc)
        print "For " + untidy_filename + " there were this many lines processed: " + str(file_length(Parse_Folder + "\\" + untidy_filename))
    print "A total of " + str(lines_processed) + " were processed from this many files"

needs_work=[]
Parse_Folder = raw_input("enter full path for location of folder with files needing tidying:----> ")
outfile = raw_input("enter full path for the outfile:----> ")

def identify_untidy(Parse_Folder):#=locationInput()[0]): ##as of 10/21/13 this definition works just fine
    """this searches within a directory and returns 0 or more .csv files that have no string 'tidy' in the filename and appends these filenames
to a list named 'needs_work'."""
    import os
    #regarding if tidy statement, maybe include campusList from filename_Parser below
    file_list = os.listdir(Parse_Folder)
    for f in file_list:
        if "Tidy" not in f: ###alternatively, this works: if not re.match('^Tidy',f):####
            if ".csv" in f:
                needs_work.append(f)
    return needs_work

def filename_Parser(fileAddress): 
    """takes a string and makes a list.  String comes from file_tidy below with the form "CentralWinter2013Enrollments.csv" in a fileAddress (location) that includes the filename and pulls out 2 variable names from the filename itself, Quarter and campus. These will be fed to the file_tidy"""
    import re
    import ntpath
    campusList= ["North","South","Central","SVI"]
    quarterList = ["Fall","Winter","Spring","Summer"]
    fileName = ntpath.basename(fileAddress)
    yearMatch = re.search("[0-9]{4}",fileName) #this command creates an object, requiring the next step below using the .group function
    year = yearMatch.group(0) #isolates the year
    for c in campusList: #for each item in ["North","South","Central","SVI"]...
        if c in fileName: #then campus will be assigned the value 'c'
            campus = c
    for q in quarterList:
        if q in fileName: #then quarter will be assigned the value 'q'
            quarter = q
    quarterCampus = [quarter, year, campus]
    return quarterCampus
    
                        

def file_tidy(f_loc, outfile_store =outfile +"\\" +"TidyEnrollments.csv") :
    """This definition is longer than necessary, but it works. It creates a new outfile that includes only the raw data, but prefixes these lines with the campus and quarter info from filename_Parser above"""
    import re
    prefix = filename_Parser(f_loc)[0] +" "+ filename_Parser(f_loc)[1]+","+filename_Parser(f_loc)[2] + "," #this takes the output of filename_Parser, which is a list [quarter, year, campus] and front appends to each line of the read-line
    with open(f_loc,) as infile, open(outfile_store, "a") as outfile: # infile and outfile are temp/local variables
        while True:
            line=infile.readline() #this reads each and every line of file into a variable "line"
            line=line.replace('\xa0', '').encode('utf-8') #this replaces the unicode character
            if not line: break
            match = re.findall("^[0-9]{1,4}",line) #the wanted data in these file begins with a 4 digit item code. No other lines have this bx.
            if match:
                outfile.write(prefix + line) # <<== I must use the fileNameParser to obtain these values. This writes the new string info to the beginning of each wanted line and saves to outfile, which points to the actual output-filename at top of code
        infile.close()

def file_length(f_loc): ###as of 10/21/13, this code definition works just fine.
    """counts the number of lines in a file. This is just a quick sanity check"""
    with open(f_loc) as f:
        for i, l in enumerate(f):
            pass
    return i + 1
    

    
