dataAnalyst <- read.csv("~/Downloads/Datasets/DataAnalyst.csv", comment.char="#")
View(dataAnalyst)

library(DataExplorer)
library(ggplot2)
library(tidyr)
library(dplyr)
library(quanteda)
library(wordcloud)


dataAnalyst[1]<-NULL
dataAnalyst$Job.Title<-tolower(dataAnalyst$Job.Title)

data<-dataAnalyst


introduce(data)
#there is a lot of missing values present

plot_missing(data)
#all the features have more than 50% of missing values

data$Competitors<-NULL
data$Easy.Apply<-NULL



data%>%select(Job.Title)%>%group_by(Job.Title)%>%summarise(total=n())%>%arrange(desc(total))%>%top_n(20)


#top 10 most common data analyst job listed

# Job.Title total
# 1              data analyst   335
# 2       senior data analyst    78
# 3       junior data analyst    28
# 4     business data analyst    23
# 5          sr. data analyst    17
# 6       data analyst junior    15
# 7   data governance analyst    14
# 8         lead data analyst    14
# 9   data collection systems    13
# 10     data quality analyst    13



#cleaning the salary value 

data<-data%>%separate(Salary.Estimate,c("salary","col2"),sep=" ")
data$col2<-NULL

data1<-data%>%separate(salary,c("col1","col2"),sep="-")
data1$col1<-substr(data1$col1,2,6)
data1$col1<-gsub("K","",data1$col1)
data1$col1<-as.numeric(data1$col1)
names(data1)[2]<-"min"


data1$col2<-substr(data1$col2,2,6)
data1$col2<-gsub("K","",data1$col2)
data1$col2<-as.numeric(data1$col2)
names(data1)[3]<-"max"


avg<-function(x,y){
  (x+y)/2
}

data1$mean<-avg(data1$min,data1$max)

data%>%select(salary)%>%group_by(salary)%>%summarise(total=n())%>%arrange(desc(total))

#estimated salary for a data analyst roles
#   salary      total
# 1 "$41K-$78K"    57
# 2 "$42K-$76K"    57
# 3 "$50K-$86K"    41
# 4 "$35K-$67K"    33
# 5 "$43K-$76K"    31
# 6 "$58K-$93K"    31
# 7 "$27K-$52K"    30
# 8 "$35K-$42K"    30

data1$Rating<-as.numeric(data1$Rating)
data2<-subset(data1,Rating<10)
rate<-subset(data2,Rating!=-1)
plot(rate$Rating,rate$mean)
plot(rate$Rating,log(rate$mean))
plot(log(rate$Rating),log(rate$mean))
#we dont see any realationship between salaries and comapnies ratings

#using pearsons correlation to check
cor.test(rate$Rating,rate$mean)

# Pearson's product-moment correlation
# 
# data:  rate$Rating and rate$mean
# t = 0.41755, df = 1646, p-value = 0.6763
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#  -0.03801418  0.05854895
# sample estimates:
#        cor 
# 0.01029138 

#since the p value is greater than 0.05 we cannot reject our null hypothesis
#therefore there is no relation


str_sub(data2$Company.Name,-length(data2$Company.Name),-5)


data2%>%select(Company.Name,Rating)%>%group_by(Company.Name,Rating)%>%summarise(total=n())%>%arrange(desc(total))

#Top 10 comapnies with the most data analytical roles listed

# Company.Name                            Rating total
# 1 "Staffigo Technical Services, LLC\n5.0"    5      50
# 2 "Diverse Lynx\n3.9"                        3.9    18
# 3 "Lorven Technologies Inc\n4.0"             4      14
# 4 "Robert Half\n3.5"                         3.5    13
# 5 "Avacend, Inc.\n2.5"                       2.5    12
# 6 "Kforce\n4.1"                              4.1    12
# 7 "Mondo\n3.9"                               3.9    10
# 8 "Reliable Software Resources\n4.0"         4       9
# 9 "Apex Systems\n3.8"                        3.8     7
# 10 "eTeam Inc.\n3.7"                         3.7     7

#details of the top 5 companies

staffigo_technical_services<-View(subset(data2,Company.Name=="Staffigo Technical Services, LLC\n5.0"))
diverse_lynx<-View(subset(data2,Company.Name=="Diverse Lynx\n3.9"))
lorven_technologies<-View(subset(data2,Company.Name=="Lorven Technologies Inc\n4.0"))
robert_half<-View(subset(data2,Company.Name=="Robert Half\n3.5"))
avacend.inc<-View(subset(data2,Company.Name=="Avacend, Inc.\n2.5"))
                         

#Companies with the largest employees

employ<-subset(data2,Size=="10000+ employees")
employ%>%select(Company.Name,Rating)%>%group_by(Company.Name,Rating)%>%arrange(desc(Rating))

# Company.Name                      Rating
# 1 "NVIDIA\n4.6"                        4.6
# 2 "Fortinet\n4.5"                      4.5
# 3 "Facebook\n4.5"                      4.5
# 4 "Google\n4.4"                        4.4
# 5 "Google\n4.4"                        4.4
# 6 "HCL Global Systems\n4.3"            4.3
# 7 "University of Florida\n4.3"         4.3
# 8 "University of Pennsylvania\n4.3"    4.3
# 9 "University of Pennsylvania\n4.3"    4.3
# 10 "University of Pennsylvania\n4.3"   4.3



#we see that many newer enterprises are being data driven and hence at lot more listing from them  
data2$Founded<-as.numeric(data2$Founded)
data3<-subset(data2,Founded!=-1)
ggplot(data3,aes(Founded))+geom_histogram(bins = 30)+ggtitle("Year wise data role listing")+xlab("Founded")+ylab("count")


data3%>%select(Type.of.ownership)%>%group_by(Type.of.ownership)%>%summarise(total=n())%>%arrange(desc(total))

#mostly recurited by private and public firms

# Type.of.ownership              total
# 1 Company - Private                774
# 2 Company - Public                 331
# 3 Nonprofit Organization            89
# 4 Subsidiary or Business Segment    59
# 5 College / University              24

data3%>%select(Sector)%>%group_by(Sector)%>%summarise(total=n())%>%arrange(desc(total))

#we can we that the IT leads the job market for data analyst roles followed by business services and finance

# Sector                    total
# 1 Information Technology      397
# 2 Business Services           347
# 3 Finance                     138
# 4 Health Care                 119
# 5 Insurance                    42
# 6 Education                    37
# 7 Media                        33
# 8 Retail                       28
# 9 Biotech & Pharmaceuticals    26
# 10 Manufacturing               24


#we can see that ratings have no relation with any of the features
#and that the annual income does not depend on the size or the amount of revenue earned by the company
data2%>%select(Rating,Revenue,mean)%>%group_by(Revenue)%>%summarise(average=mean(Rating),mean=mean(mean))
data2%>%select(Rating,Size,mean)%>%group_by(Size)%>%summarise(average=mean(Rating),mean=mean(mean))


#text analysis of the job description

text<-tokens(data2$Job.Description,what="word",remove_punct = TRUE,remove_numbers = TRUE,remove_symbols = TRUE)

text<-tokens_tolower(text)
text<-tokens_select(text,stopwords(),selection = "remove")
text<-tokens_wordstem(text,language = "english")
text<-tokens_remove(text,"â")

text.dfm<-dfm(text)

freqr<-colSums(as.matrix(text.dfm))
ord<-order(freqr,decreasing = TRUE)

#most used words in the job description
freqr[head(ord,10)]

# words- data  experi    work    busi  requir   manag  report    team develop   skill 
# count- 20110  8105    6758    6130    4997    4738    4540    4259    4154    4135 

#Visualizating the most common words

visual<-data.frame(term=names(freqr),occurrences=freqr)

ggplot(subset(visual,freqr>2000),aes(x=reorder(term,-occurrences),y=occurrences))+geom_bar(stat="identity")+coord_flip()+xlab("count")+ylab("count")+ggtitle("common words based in the job description of various data analyst roles and companies")
#vary the freqr value to see more/less words

#Visualizing using wordcloud

wordcloud(names(freqr),freqr,min.freq =1000,colors = brewer.pal(6,"Dark2"))


#n-grams
#including word ordering i.e joining the two successive words

text.n<-tokens_ngrams(text,2)
text.n<-tokens_replace(text.n,"data_analysi","data_analyst")
text.n<-tokens_replace(text.n,"data_analyt","data_analyst")
text.n<-tokens_replace(text.n,"data_qualiti","data_analyst")
text.n<-dfm(text.n)
n.freq<-colSums(as.matrix(text.n))
n.ord<-order(n.freq,decreasing = TRUE)

n.freq[head(n.ord,10)]

#Highest two succesive word count
# data_analyst    year_experi communic_skill     data_manag equal_opportun 
# 4469           1035            725            679            605 
# skill_abil   job_descript    experi_data  comput_scienc    experi_work 
# 601            601            588            581            560 

visual1<-data.frame(term=names(n.freq),occurrences=n.freq)

ggplot(subset(visual1,n.freq>300),aes(x=reorder(term,-occurrences),y=occurrences))+geom_bar(stat="identity")+coord_flip()+xlab("count")+ylab("count")+ggtitle("common 2 successive words based in the job description of various data analyst roles and companies")

