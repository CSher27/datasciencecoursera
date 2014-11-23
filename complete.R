complete <- function(dir, id = 1:332) {
        ## dir = character vector, length 1, directory location of CSV files 

        ## id = integer vector, length 1, ID numbers of monitors to read

        ## Returns a data frame of complete observations from monitors 
        ## in the 'id' vector by id and number of complete observations (nobs)
        
## generate vector of file names for specified id
fnames<-c(paste(dir,'/',sprintf("%03d",id),'.csv',sep=''))

##  set column classes
mycols<-c(Date="Date",sulfate="numeric",nitrate="numeric",id="integer")

##  set number of interations
x<-length(id)

##  set up data frame for results
myDF<-data.frame(id=id,nobs=0)

##  read through files and collect count of complete cases
for (i in 1:x) {
        mydata<-read.csv(fnames[i],colClasses=mycols,comment.char="")
        myDF[i,2]<-sum(complete.cases(mydata))
}

##  output results
myDF

}