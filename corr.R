corr <- function(dir, threshold = 0) {
        ## dir = character vector, length 1, directory location of CSV files 

        ## threshold = numeric vector, length 1, threshold of 
        ## complete cases required to compute the correlation
        ## between nitrate and sulfate; the default is 0

        ## Returns a numeric vector of correlations

##  subset monitors meeting the specified threshold
allset <- complete(dir)
threshset <- which(allset$nobs > threshold)

##  return zero if none found
if(length(threshset)==0) return(vector("numeric",length=0))

## generate vector of file names for specified threshold
fnames <- c(paste(dir,'/',sprintf("%03d",threshset),'.csv',sep=''))

mycols <- c('NULL',sulfate="numeric",nitrate="numeric",'NULL')

##  read through files and correlate each set of data
x<-length(fnames)
cores<-c(rep(0,x))
for (i in 1:x) {
        mydata<-read.csv(fnames[i],colClasses=mycols,comment.char="")
        cores[i]<-cor(x=mydata$sulfate,y=mydata$nitrate,use="na.or.complete")
                    
}
cores
}