#start by downloading the Uncleaned_DS_jobs.CSV file from Canvas in the Course Project Resources folder
Jobs=read.csv(file.choose()) #read in file

head(Jobs) #view first 6 rows of data to see variable names and data formatting
nrow(Jobs) #view number of rows
ncol(Jobs) #view number of columns

Jobs.sub=Jobs[,c(2,3,4,6,8,9,10,12)] #create data subset with all rows but only the columns of interest
head(Jobs.sub) #confirm correct columns were selected

Jobs2=na.omit(Jobs.sub) #create data subset without rows with NA in any cell
nrow(Jobs2) #view new number of rows
Jobs3=Jobs2[Jobs2$Sector!="" & Jobs2$Size!="Unknown",] #create data subset without rows with empty cells and without unknown sizes
nrow(Jobs3) #view new number of rows
Jobs3$SizeInd=ifelse(Jobs3$Size=="1 to 50 employees",1, #create new column with numerical indicator for size group
              ifelse(Jobs3$Size=="51 to 200 employees",2,
              ifelse(Jobs3$Size=="201 to 500 employees",3,
              ifelse(Jobs3$Size=="501 to 1000 employees",4,
              ifelse(Jobs3$Size=="1001 to 5000 employees",5,
              ifelse(Jobs3$Size=="5001 to 10000 employees",6,7))))))
print(Jobs3) #skim through and confirm the code worked as intended
Jobs3$SizeGrp=ifelse(Jobs3$SizeInd==1 | Jobs3$SizeInd==2,"Small", #create new column with fewer size groups
              ifelse(Jobs3$SizeInd==3 | Jobs3$SizeInd==4,"Medium","Large"))
print(Jobs3) #skim through and confirm the code worked as intended
library(tidyr) #load tidyr library: install package first if needed
Jobs3=separate_wider_delim(Jobs3,cols=Salary, delim = "-", names = c("Low.Salary", "High.Salary")) #separate salary into two columns
print(Jobs3) #confirm previous code worked
Jobs3$Low.Salary=gsub("[[:punct:]]", "", Jobs3$Low.Salary) #remove dollar signs from data in low salary column
Jobs3$High.Salary=gsub("[[:punct:]]", "", Jobs3$High.Salary) #same for high salary column
print(Jobs3) #confirm the previous codes worked
Jobs3$High.Salary=as.numeric(gsub('K', '000', Jobs3$High.Salary)) #replace K with 000 in low salary column and convert to numeric
Jobs3$Low.Salary=as.numeric(gsub('K', '000', Jobs3$Low.Salary)) #same for high salary column
print(Jobs3) #confirm the previous codes worked
Jobs3$Avg.Salary=(Jobs3$Low.Salary+Jobs3$High.Salary)/2 #create new column with average salary for each row
print(Jobs3,n=50) #confirm the previous code worked
fivenum(Jobs3$Avg.Salary) #calculate five number summary for average salary
library(dplyr) #load dplyr library: install package first if needed
Jobs3$SalaryGrp=ifelse(Jobs3$Avg.Salary<=99499,"Low", #create new salary groups based on data quartiles
                ifelse(Jobs3$Avg.Salary<=113999,"Medium Low",
                ifelse(between(Jobs3$Avg.Salary,114000,136499),"Medium High","High")))
print(Jobs3,n=50) #confirm previous code worked as intended
Jobs3$SalaryBinary=ifelse(Jobs3$SalaryGrp=="Low" | Jobs3$SalaryGrp=="Medium Low","Below Median","Above Median") #create new binary salary variable
print(Jobs3,n=50) #confirm previous code worked as intended
Jobs3$SalaryInd=ifelse(Jobs3$SalaryBinary=="Below Median",0,1) #create new indicate variable from binary salary groups
print(Jobs3,n=50) #confirm previous code worked as intended


