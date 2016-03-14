
hmpfiles <- list.files(recursive = T,ignore.case = T,pattern = ".txt")

if (file.exists("data.csv") == T) {file.remove("data.csv")}

for (i in 1:979) {
  
  names<-noquote(unlist(strsplit(hmpfiles[i],"[/,.-]")))
  file<-read.table(hmpfiles[i])
  file$v4<-names[10]
  file$v5<-names[11]
  write.table(file,"data.csv",append = T,sep=",",row.names = F,col.names = F) 
  
}

files_sum<-read.csv("data.csv",header = T,sep = ",")
colnames(files_sum)<-c("ax","ay","az","Activity","Volunteer")

#CONVERT THE ACCELEROMETER DATA INTO REAL ACCELERATION VALUES

noise_fil<- function (i) {
  (-14.709) + ((i)/63) * (2*14.709)
}

for (i in 1:979) {
files_sum$ax[i]<-noise_fil(files_sum$ax[i])
files_sum$ay[i]<-noise_fil(files_sum$ay[i])
files_sum$az[i]<-noise_fil(files_sum$az[i])
}

#REDUCE THE NOISE ON THE SIGNALS BY MEDIAN FILTERING

files_sum$ax<-medianFilter(files_sum$ax, windowSize = 3)
files_sum$ay<-medianFilter(files_sum$ay, windowSize = 3)
files_sum$az<-medianFilter(files_sum$az, windowSize = 3)

ggplot(files_sum,aes(x=ax)) + geom_bar() + facet_grid(Activity~.,scale = "free")

#SPLITS DATA INTO TRAIN AND TEST DATA SETS

split<-createDataPartition(y=files_sum$Activity,p=0.8, list = FALSE)
testdata<-files_sum[split,]
traindata<-files_sum[-split,]




