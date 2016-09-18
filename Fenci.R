#调入分词的库

library("rJava")#注意使用的IDE的位数要与系统的位数一致 
library("Rwordseg") #安装可能不成功，可以直接将包下载、解压后放到R的相应文件夹下
#调入绘制词云的库
library("RColorBrewer") 
library("wordcloud") 


#读入数据(特别注意，read.csv 可以读取txt的文本)

myfile<-read.csv(file.choose(), header=FALSE, encoding = "UTF-8")
#注意文本文件的编码是什么，如编码不符，会出现费时且得不到应有效果的情况 

#预处理，这步可以将读入的文本转换为可以分词的字符，没有这步不能分词
myfile.res <- myfile[myfile!=" "] 


#装载分词词典（如果需要的话），比较“经济”的获取方法有搜狗网站上的词库
installDict(file.choose(),dictname = "no1")
#装载自定义词组（单个增加）
insertWords("阿呀阿")


#分词，并将分词结果转换为向量
myfile.words <- unlist(lapply(X = myfile.res,FUN = segmentCN)) 

#剔除URL等各种不需要的字符，还需要删除什么特殊的字符可以依样画葫芦在下面增加gsub的语句
#有两个双引，类似与WORD里的替换，第一个是被替换的内容，第二个是替换成的
myfile.words <- gsub(pattern="http:[a-zA-Z\\/\\.0-9]+","",myfile.words) 
myfile.words <- gsub("\n","",myfile.words) 
myfile.words <- gsub("　","",myfile.words) 

#去掉停用词
data_stw=read.table(file=file.choose(),colClasses="character")#网上可以找到相应的词库 
stopwords_CN=c(NULL) 

for(i in 1:dim(data_stw)[1]){ 
    stopwords_CN=c(stopwords_CN,data_stw[i,1]) 
}

for(j in 1:length(stopwords_CN)){ 
    myfile.words <- subset(myfile.words,myfile.words!=stopwords_CN[j]) 
}


#过滤掉1个字的词
myfile.words <- subset(myfile.words, nchar(as.character(myfile.words))>1) 


#统计词频
myfile.freq <- table(unlist(myfile.words)) 
myfile.freq <- rev(sort(myfile.freq)) 
myfile.freq <- data.frame(word=names(myfile.freq), freq=myfile.freq)


#按词频过滤词，过滤掉只出现过一次的词，这里可以根据需要调整过滤的词频数
myfile.freq2=subset(myfile.freq, myfile.freq$freq>=2) 


#绘制词云
#设置一个颜色系：
mycolors <- brewer.pal(8,"Dark2") 

#设置字体
windowsFonts(myFont=windowsFont("华文彩云")) 

#画图
wordcloud(myfile.freq2$word,myfile.freq2$freq,random.order=FALSE,
          random.color=FALSE,colors=mycolors,family="myFont")

