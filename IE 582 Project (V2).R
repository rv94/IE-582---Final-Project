##Load  required packages ##
install.packages("SnowballC")
install.packages("caTools")
install.packages("ada")
library("ada")
library("RSNNS")
library("xgboost")
library("tm")
library("wordcloud")
library("SnowballC")
library(caTools)
library(ROCR)
library(caret)
library(ggplot2)
library(MASS)
library(rpart)
library(rpart.plot)
library(neuralnet)
library(igraph)
library(Rgraphviz)
library(randomForest)
library(gridExtra)
library(markdown)

## Read data frames ##
setwd("C:/Users/vigne/Desktop/IE Stuff/IE 582 - Engineering Analytics/Project/Dataset")
PSY=read.csv("Youtube 01-comments Psy.csv",stringsAsFactors = FALSE)
KP=read.csv("Youtube 04-comments KatyPerry.csv",stringsAsFactors = FALSE)
LMFAO=read.csv("Youtube 07-comments LMFAO.csv",stringsAsFactors = FALSE)
Eminem=read.csv("Youtube 08-comments Eminem.csv",stringsAsFactors = FALSE)
Shakira=read.csv("Youtube 09-comments Shakira.csv",stringsAsFactors = FALSE)
Total=rbind.data.frame(PSY,KP,LMFAO,PSY,Shakira) ##Incorporate into total data frame
par(mfrow=c(3,2))

View(PSY)

## Preprocess PSY data set by extracting content, making all lowercase, removing punctuation, numbers, stemming words, remvoing stop words and creating a DTM
corpus1=VCorpus(VectorSource(PSY$CONTENT)) 
corpus1[[1]]$content
corpus1=tm_map(corpus1,content_transformer(tolower))
corpus1=tm_map(corpus1,removePunctuation)
corpus1=tm_map(corpus1,removeNumbers)
corpus1=tm_map(corpus1,removeWords,stopwords("english"))
dtm1=DocumentTermMatrix(corpus1)
Sdtm1=removeSparseTerms(dtm1,0.99) ##Remove 99% sparse terms due to indicated sparsity

##Create WordCloud from PSY data set and histogram of word frequency
WPSY=as.data.frame(as.matrix(Sdtm1))
wordcloud(colnames(WPSY),colSums(WPSY),random.order = FALSE, min.freq = 5,rot.per = .1)
PH=data.frame(Terms=colnames(WPSY),Frequency=colSums(WPSY))
PH$Terms=factor(PH$Terms,levels = PH$Terms[order(PH$Frequency,decreasing = T)])
PH=PH[order(PH$Frequency,decreasing = T),]
P1=ggplot(PH,aes(y=PH$Frequency,x=PH$Terms))+geom_histogram(stat="identity",fill="light blue")+ggtitle("Word Frequencies in PSY data set")

## Preprocess Katy Perry data set by extracting content, making all lowercase, removing punctuation, numbers, stemming words, remvoing stop words and creating a DTM
corpus2=VCorpus(VectorSource(KP$CONTENT))
corpus2[[2]]$content
corpus2=tm_map(corpus2,content_transformer(tolower))
corpus2=tm_map(corpus2,removePunctuation)
corpus2=tm_map(corpus2,removeNumbers)
corpus2=tm_map(corpus2,stemDocument)
corpus2=tm_map(corpus2,removeWords,stopwords("english"))
dtm2=DocumentTermMatrix(corpus2)
Sdtm2=removeSparseTerms(dtm2,0.99) ##Remove 99% sparse terms due to indicated sparsity

##Create WordCloud from Katy Perry data  set
WKP=as.data.frame(as.matrix(Sdtm2))
wordcloud(colnames(WKP),colSums(WKP),random.order = FALSE, min.freq = 5,rot.per = .1)
KH=data.frame(Terms=colnames(WKP),Frequency=colSums(WKP))
KH$Terms=factor(KH$Terms,levels = KH$Terms[order(KH$Frequency,decreasing = T)])
KH=KH[order(KH$Frequency,decreasing = T),]
P2=ggplot(KH,aes(y=KH$Frequency,x=KH$Terms))+geom_histogram(stat="identity",fill="light blue")+ggtitle("Word Frequencies in Katy Perry data set")

## Preprocess Eminem data set by extracting content, making all lowercase, removing punctuation, numbers, stemming words, remvoing stop words and creating a DTM
corpus3=VCorpus(VectorSource(Eminem$CONTENT))
corpus3[[3]]$content
corpus3=tm_map(corpus3,content_transformer(tolower))
corpus3=tm_map(corpus3,removePunctuation)
corpus3=tm_map(corpus3,removeNumbers)
corpus3=tm_map(corpus3,removeWords,stopwords("english"))
dtm3=DocumentTermMatrix(corpus3)
Sdtm3=removeSparseTerms(dtm3,0.99) ##Remove 99% sparse terms due to indicated sparsity

##Create WordCloud from Eminem data  set
WEm=as.data.frame(as.matrix(Sdtm3))
wordcloud(colnames(WEm),colSums(WEm),random.order = FALSE, min.freq = 5,rot.per = .1)
EH=data.frame(Terms=colnames(WEm),Frequency=colSums(WEm))
EH$Terms=factor(EH$Terms,levels = EH$Terms[order(EH$Frequency,decreasing = T)])
EH=EH[order(EH$Frequency,decreasing = T),]
P3=ggplot(EH,aes(y=EH$Frequency,x=EH$Terms))+geom_histogram(stat="identity",fill="light blue")+ggtitle("Word Frequencies in Eminem data set")

## Preprocess LMFAO data set by extracting content, making all lowercase, removing punctuation, numbers, stemming words, remvoing stop words and creating a DTM
corpus4=VCorpus(VectorSource(LMFAO$CONTENT))
corpus4[[4]]$content
corpus4=tm_map(corpus4,content_transformer(tolower))
corpus4=tm_map(corpus4,removePunctuation)
corpus4=tm_map(corpus4,removeNumbers)
corpus4=tm_map(corpus4,stemDocument)
corpus4=tm_map(corpus4,removeWords,stopwords("english"))
dtm4=DocumentTermMatrix(corpus4)
Sdtm4=removeSparseTerms(dtm4,0.99) ##Remove 99% sparse terms due to indicated sparsity

##Create WordCloud from LMFAO data  set
WLM=as.data.frame(as.matrix(Sdtm4))
wordcloud(colnames(WLM),colSums(WLM),random.order = FALSE, min.freq = 5,rot.per = .1)
LH=data.frame(Terms=colnames(WEm),Frequency=colSums(WEm))
LH$Terms=factor(LH$Terms,levels = LH$Terms[order(LH$Frequency,decreasing = T)])
LH=LH[order(LH$Frequency,decreasing = T),]
P4=ggplot(LH,aes(y=LH$Frequency,x=LH$Terms))+geom_histogram(stat="identity",fill="light blue")+ggtitle("Word Frequencies in Eminem data set")

## Preprocess Shakira data set by extracting content, making all lowercase, removing punctuation, numbers, stemming words, remvoing stop words and creating a DTM
corpus5=VCorpus(VectorSource(Shakira$CONTENT))
corpus5[[5]]$content
corpus5=tm_map(corpus5,removePunctuation)
corpus5=tm_map(corpus5,content_transformer(tolower))
corpus5=tm_map(corpus5,removeNumbers)
corpus5=tm_map(corpus5,removeWords,stopwords("english"))
corpus5=VCorpus(VectorSource(corpus5))
dtm5=DocumentTermMatrix(corpus5)
Sdtm5=removeSparseTerms(dtm5,0.99) ##Remove 99% sparse terms due to indicated sparsity

##Create WordCloud from Shakira data set
WSha=as.data.frame(as.matrix(Sdtm5))
wordcloud(colnames(WSha),colSums(WSha),random.order = FALSE, min.freq = 5,rot.per = .1)
SH=data.frame(Terms=colnames(WSha),Frequency=colSums(WSha))
SH$Terms=factor(SH$Terms,levels = SH$Terms[order(SH$Frequency,decreasing = T)])
SH=SH[order(SH$Frequency,decreasing = T),]
P5=ggplot(SH,aes(y=SH$Frequency,x=SH$Terms))+geom_histogram(stat="identity",fill="light blue")+ggtitle("Word Frequencies in Shakira data set")

## Preprocess Total data set by extracting content, making all lowercase, removing punctuation, numbers, stemming words, remvoing stop words and creating a DTM
corpusT=VCorpus(VectorSource(Total$CONTENT))
corpusT[[5]]$content
corpusT=tm_map(corpusT,content_transformer(tolower))
corpusT=tm_map(corpusT,removeNumbers)
corpusT=tm_map(corpusT,removeWords,stopwords("english"))
dtmT=DocumentTermMatrix(corpusT)
SdtmT=removeSparseTerms(dtmT,0.99) ##Remove 99% sparse terms due to indicated sparsity

##Create WordCloud from Total data set
WT=as.data.frame(as.matrix(SdtmT))
wordcloud(colnames(WT),colSums(WT),random.order = FALSE, min.freq = 5,rot.per = .1)
TH=data.frame(Terms=colnames(WT),Frequency=colSums(WT))
TH$Terms=factor(TH$Terms,levels = TH$Terms[order(TH$Frequency,decreasing = T)])
TH=TH[order(TH$Frequency,decreasing = T),]
P6=ggplot(TH,aes(y=TH$Frequency,x=TH$Terms))+geom_histogram(stat="identity",fill="light blue")+ggtitle("Word Frequencies in Total data set")
grid.arrange(P1,P2,P3,P4,P5,P6,nrow=3,ncol=2)

##Attach response variables to respective data frames from source and converting to Factor variables as response
WPSY$Class=as.factor(PSY$CLASS)
WEm$Class = as.factor(Eminem$CLASS)
WSha$Class= as.factor(Shakira$CLASS)
WLM$Class= as.factor(LMFAO$CLASS)
WKP$Class=as.factor(KP$CLASS)
WT$Class=as.factor(Total$CLASS)

##Split datasets into 75:25 Training:Testing sets
set.seed(2017)
PSplit=sample.split(WPSY$Class,0.75) ##PSY
PTrain=subset(WPSY,PSplit==T)
PTest=subset(WPSY,PSplit==F)

set.seed(2017)
ESplit=sample.split(WEm$Class,0.75) ##Eminem
ETrain=subset(WEm,ESplit==T)
ETest=subset(WEm,ESplit==F)

set.seed(2017)
SSplit=sample.split(WSha$Class,0.75) ##Shakira
STrain=subset(WSha,SSplit==T)
STest=subset(WSha,SSplit==F)

set.seed(2017)
LSplit=sample.split(WLM$Class,0.75) ##LMFAO
LTrain=subset(WLM,LSplit==T)
LTest=subset(WLM,LSplit==F)

set.seed(2017)
KSplit=sample.split(WKP$Class,0.75) ##Katy Perry
KTrain=subset(WKP,KSplit==T)
KTest=subset(WKP,KSplit==F)

set.seed(2017)
TSplit=sample.split(WT$Class,0.75) ##Total
TTrain=subset(WT,TSplit==T)
TTest=subset(WT,TSplit==F)

##Logistic regression models for the 6 datasets - Build model off training data, measure system time elapsed, predict off test data and generate confusion matrix
PLog=glm(formula=Class~., data = PTrain,family = "binomial")
system.time(glm(formula=Class~., data = PTrain,family = "binomial"))
PLogPred=predict(PLog,newdata=PTest)
table(PTest$Class,PLogPred>0.5) ##PSY

ELog=glm(formula=Class~., data = ETrain,family = "binomial")
system.time(glm(formula=Class~., data = ETrain,family = "binomial"))
ELogPred=predict(ELog,newdata=ETest)
table(ETest$Class,ELogPred>0.5) ##Eminem

SLog=glm(formula=Class~., data = STrain,family = "binomial")
system.time(glm(formula=Class~., data = STrain,family = "binomial"))
SLogPred=predict(SLog,newdata=STest)
table(STest$Class,SLogPred>0.5) ##Shakira

LLog=glm(formula=Class~., data = LTrain,family = "binomial")
system.time(glm(formula=Class~., data = LTrain,family = "binomial"))
LLogPred=predict(LLog,newdata=LTest)
table(LTest$Class,LLogPred>0.5)##LMFAO

KLog=glm(formula=Class~., data = KTrain,family = "binomial")
system.time(glm(formula=Class~., data = KTrain,family = "binomial"))
KLogPred=predict(KLog,newdata=KTest)
table(KTest$Class,KLogPred>0.5) ##Katy Perry

TLog=glm(formula=Class~., data = TTrain,family = "binomial")
system.time(glm(formula=Class~., data = TTrain,family = "binomial"))
TLogPred=predict(TLog,newdata=TTest)
table(TTest$Class,TLogPred>0.5) ##Total dataset

TLogPredR=ROCR::prediction(TLogPred,TTest$Class)
TLogPerf=performance(TLogPredR,"tpr","fpr")
plot(TLogPerf,main = "Total Data Set, Logistic Regression ROC Plot")

##LDA models for the 6 datasets - Build model off training data, measure system time elapsed, predict off test data and generate confusion matrix
PLDA=lda(Class~., data = PTrain)
system.time(lda(Class~., data = PTrain))
PLDAPred=predict(PLDA,newdata=PTest)
table(PTest$Class,PLDAPred$class) ##PSY

ELDA=lda(Class~., data = ETrain)
system.time(lda(Class~., data = ETrain))
ELDAPred=predict(ELDA,newdata=ETest)
table(ETest$Class,ELDAPred$class) ##Eminem

SLDA=lda(Class~., data = STrain)
system.time(lda(Class~., data = STrain))
SLDAPred=predict(SLDA,newdata=STest)
table(STest$Class,SLDAPred$class) ##Shakira

LLDA=lda(Class~., data = LTrain)
system.time(lda(Class~., data = LTrain))
LLDAPred=predict(LLDA,newdata=LTest)
table(LTest$Class,LLDAPred$class) ##LMFAO

KLDA=lda(Class~., data = KTrain)
system.time(lda(Class~., data = KTrain))
KLDAPred=predict(KLDA,newdata=KTest)
table(KTest$Class,KLDAPred$class) ##Katy Perry

TLDA=lda(Class~., data = TTrain)
system.time(lda(Class~., data = TTrain))
TLDAPred=predict(TLDA,newdata=TTest)
table(TTest$Class,TLDAPred$class) ##Total Data set

TLDAPredR=ROCR::prediction(TLDAPred$class,TTest$Class)
TLDAPerf=performance(TLDAPredR,"tpr","fpr")
plot(TLDAPerf,main = "Total Data Set, LDA ROC Plot")


##QDA - Rank deficiency, likely indication that not enough data available

##CART models for the 6 datasets - Build model off training data, measure system time elapsed, plot tree predict off test data and generate confusion matrix
PTree=rpart(Class~., data=PTrain,method = "class")
system.time(rpart(Class~., data=PTrain,method = "class"))
rpart.plot(PTree, main = "PSY")
PTreePred=predict(PTree,newdata = PTest)
table(PTest$Class,PTreePred[,2]>0.5) ##PSY

ETree=rpart(Class~., data=ETrain,method = "class")
system.time(rpart(Class~., data=ETrain,method = "class"))
rpart.plot(ETree,main="Eminem")
ETreePred=predict(ETree,newdata = ETest)
table(ETest$Class,ETreePred[,2]>0.5) ##Eminem

LTree=rpart(Class~., data=LTrain,method = "class")
system.time(rpart(Class~., data=LTrain,method = "class"))
rpart.plot(LTree,main="LMFAO")
LTreePred=predict(LTree,newdata = LTest)
table(LTest$Class,LTreePred[,2]>0.5) ##LMFAO

KTree=rpart(Class~., data=KTrain,method = "class")
system.time(rpart(Class~., data=KTrain,method = "class"))
rpart.plot(KTree,main="Katy Perry")
KTreePred=predict(KTree,newdata = KTest)
table(KTest$Class,KTreePred[,2]>0.5) ##Katy Perry

STree=rpart(Class~., data=STrain,method = "class")
system.time(rpart(Class~., data=STrain,method = "class"))
rpart.plot(STree,main="Shakira")
STreePred=predict(STree,newdata = STest)
table(STest$Class,STreePred[,2]>0.5) ##Shakira

TTree=rpart(Class~., data=TTrain,method = "class")
system.time(rpart(Class~., data=TTrain,method = "class"))
rpart.plot(TTree,main="Total data set")
TTreePred=predict(TTree,newdata = TTest)
table(TTest$Class,TTreePred[,2]>0.5) ##Total data set

TTreePredR=ROCR::prediction(TTreePred[,2],TTest$Class)
TTreePerf=performance(TTreePredR,"tpr","fpr")
plot(TTreePerf,main = "Total Data Set, CART ROC Plot")

##Boosted Tree models for the 6 datasets (Adaboost) - Build model off training data, measure system time elapsed, and generate confusion matrix
PAda=ada(Class~.,data=PTrain)
system.time(ada(Class~.,data=PTrain))
PAdaPred=predict(PAda,newdata = PTest)
table(PTest$Class,PAdaPred)

EAda=ada(Class~.,data=ETrain)
system.time(ada(Class~.,data=ETrain))
EAdaPred=predict(EAda,newdata = ETest)
table(ETest$Class,EAdaPred)

LAda=ada(Class~.,data=LTrain)
system.time(ada(Class~.,data=LTrain))
LAdaPred=predict(LAda,newdata = LTest)
table(LTest$Class,LAdaPred)

KAda=ada(Class~.,data=KTrain)
system.time(ada(Class~.,data=KTrain))
KAdaPred=predict(KAda,newdata = KTest)
table(KTest$Class,KAdaPred)

SAda=ada(Class~.,data=STrain)
system.time(ada(Class~.,data=STrain))
SAdaPred=predict(SAda,newdata = STest)
table(STest$Class,SAdaPred)

TAda=ada(Class~.,data=TTrain)
system.time(ada(Class~.,data=TTrain))
TAdaPred=predict(TAda,newdata = TTest)
table(TTest$Class,TAdaPred)

##Random Forest models for the 6 datasets - Build model off training data, measure system time elapsed, and generate confusion matrix
set.seed(2017)
PRF=randomForest(Class~., data=PTrain)
system.time(randomForest(Class~., data=PTrain))
PRFPred=predict(PRF,newdata = PTest)
table(PTest$Class,PRFPred) ##PSY

set.seed(2017)
ERF=randomForest(Class~.,data=ETrain)
system.time(randomForest(Class~., data=ETrain))
ERFPred=predict(ERF,newdata = ETest)
table(ETest$Class,ERFPred) ##Eminem

set.seed(2017) ##Resolve
SRF=randomForest(Class~., data=STrain)
SRFPred=predict(SRF,newdata = STest)
table(SRFPred,STest$Class) ##Shakira

set.seed(2017)
LRF=randomForest(Class~., data=LTrain)
system.time(randomForest(Class~., data=LTrain))
LRFPred=predict(LRF,newdata = LTest)
table(LTest$Class,LRFPred) ##LMFAO


set.seed(2017)  ##Resolve
KRF=randomForest(Class~., data=KTrain)
KRFPred=predict(KRF,newdata = KTest)
table(KRFPred,KTest$Class)

set.seed(2017) ##Resolve
TRF=randomForest(Class~., data=TTrain)
TRFPred=predict(TRF,newdata = TTest)
table(TRFPred,TTest$Class)

##5-Nearest Neighbour models for the 6 datasets - Build model off training data, measure system time elapsed, and generate confusion matrix
P5NN=knn3(Class~., data=PTrain, k=5)
system.time(knn3(Class~., data=PTrain, k=5))
P5NNPred=predict(P5NN,newdata = PTest)
table(PTest$Class,P5NNPred[,2]>0.5)##PSY

E5NN=knn3(Class~., data=ETrain, k=5)
system.time(knn3(Class~., data=ETrain, k=5))
E5NNPred=predict(E5NN,newdata = ETest)
table(ETest$Class,E5NNPred[,2]>0.5)##Eminem

K5NN=knn3(Class~., data=KTrain, k=5) ##Resolve
system.time(knn3(Class~., data=KTrain, k=5))
K5NNPred=predict(K5NN,newdata = KTest)
table(K5NNPred[,2]>0.5,KTest$Class)

L5NN=knn3(Class~., data=LTrain, k=5)
system.time(knn3(Class~., data=LTrain, k=5))
L5NNPred=predict(L5NN,newdata = LTest)
table(LTest$Class,L5NNPred[,2]>0.5)##LMFAO

S5NN=knn3(Class~., data=STrain, k=5) ##Resolve
system.time(knn3(Class~., data=STrain, k=5))
S5NNPred=predict(S5NN,newdata = STest)
table(S5NNPred[,2]>0.5,STest$Class)##Shakira

T5NN=knn3(Class~., data=TTrain, k=5) ##Resolve
system.time(knn3(Class~., data=TTrain, k=5))
T5NNPred=predict(T5NN,newdata = TTest)
table(T5NNPred[,2]>0.5,TTest$Class)##Total Data Set

##3-Nearest Neighbour models for the 6 datasets - Build model off training data, measure system time elapsed, and generate confusion matrix
P3NN=knn3(Class~., data=PTrain, k=3)
system.time(knn3(Class~., data=PTrain, k=3))
P3NNPred=predict(P3NN,newdata = PTest)
table(PTest$Class,P3NNPred[,2]>0.5)##PSY

E3NN=knn3(Class~., data=ETrain, k=3)
system.time(knn3(Class~., data=ETrain, k=3))
E3NNPred=predict(E3NN,newdata = ETest)
table(ETest$Class,E3NNPred[,2]>0.5)##Eminem

K3NN=knn3(Class~., data=KTrain, k=3) ##Resolve
system.time(knn3(Class~., data=KTrain, k=3))
K3NNPred=predict(K3NN,newdata = KTest)
table(K3NNPred[,2]>0.5,KTest$Class)

L3NN=knn3(Class~., data=LTrain, k=3)
system.time(knn3(Class~., data=LTrain, k=3))
L3NNPred=predict(L3NN,newdata = LTest)
table(LTest$Class,L3NNPred[,2]>0.5)##LMFAO

S3NN=knn3(Class~., data=STrain, k=3) ##Resolve
system.time(knn3(Class~., data=STrain, k=3))
S3NNPred=predict(S3NN,newdata = STest)
table(S3NNPred[,2]>0.5,STest$Class)##Shakira

T3NN=knn3(Class~., data=TTrain, k=3) ##Resolve
system.time(knn3(Class~., data=TTrain, k=3))
T3NNPred=predict(T3NN,newdata = TTest)
table(T3NNPred[,2]>0.3,TTest$Class)##Total Data Set

##Neural network models for the 6 datasets - Build model off training data, measure system time elapsed, plot tree network off test data and generate confusion matrix
  
WPSY2=WPSY[sample(1:nrow(WPSY),length(1:nrow(WPSY))),]
WPSYValues=WPSY2[,1:107]
WPSYTargets=decodeClassLabels(WPSY2[,108])
WPSY2=splitForTrainingAndTest(WPSYValues,WPSYTargets,ratio = 0.25)
PNNModel=mlp(WPSY2$inputsTrain,WPSY2$targetsTrain,size=c(60,40,2),learnFuncParams = 0.3,maxit = 100,inputsTest = WPSY2$inputsTest, targetsTest = WPSY2$targetsTest)
system.time(mlp(WPSY2$inputsTrain,WPSY2$targetsTrain,size=c(60,40,2),learnFuncParams = 0.3,maxit = 100,inputsTest = WPSY2$inputsTest, targetsTest = WPSY2$targetsTest))
PNNPred=predict(PNNModel,WPSY2$inputsTest)
PTarget=WPSY2$targetsTest[,2]
table(PTarget,PNNPred[,2]>0.5) ##PSY

WLM2=WLM[sample(1:nrow(WLM),length(1:nrow(WLM))),]
WLMValues=WLM2[,1:67]
WLMTargets=decodeClassLabels(WLM2[,68])
WLM2=splitForTrainingAndTest(WLMValues,WLMTargets,ratio = 0.25)
LNNModel=mlp(WLM2$inputsTrain,WLM2$targetsTrain,size=c(40,30,2),learnFuncParams = 0.3,maxit = 100,inputsTest = WLM2$inputsTest, targetsTest = WLM2$targetsTest)
system.time(mlp(WLM2$inputsTrain,WLM2$targetsTrain,size=c(40,30,2),learnFuncParams = 0.3,maxit = 100,inputsTest = WLM2$inputsTest, targetsTest = WLM2$targetsTest))
LNNPred=predict(LNNModel,WLM2$inputsTest)
LTarget=WLM2$targetsTest[,2]
table(LTarget,LNNPred[,2]>0.5) ##LMFAO

WSha2=WSha[sample(1:nrow(WSha),length(1:nrow(WSha))),]
WShaValues=WSha2[,1:199]
WShaTargets=decodeClassLabels(WSha2[,200])
WSha2=splitForTrainingAndTest(WShaValues,WShaTargets,ratio = 0.25)
SNNModel=mlp(WSha2$inputsTrain,WSha2$targetsTrain,size=c(80,50,2),learnFuncParams = 0.3,maxit = 100,inputsTest = WSha2$inputsTest, targetsTest = WSha2$targetsTest)
system.time(mlp(WSha2$inputsTrain,WSha2$targetsTrain,size=c(80,50,2),learnFuncParams = 0.3,maxit = 100,inputsTest = WSha2$inputsTest, targetsTest = WSha2$targetsTest))
SNNPred=predict(SNNModel,WSha2$inputsTest)
STarget=WSha2$targetsTest[,2]
table(STarget,SNNPred[,2]>0.5) ##Shakira

WKP2=WKP[sample(1:nrow(WKP),length(1:nrow(WKP))),]
WKPValues=WKP2[,1:149]
WKPTargets=decodeClassLabels(WKP2[,150])
WKP2=splitForTrainingAndTest(WKPValues,WKPTargets,ratio = 0.25)
KNNModel=mlp(WKP2$inputsTrain,WKP2$targetsTrain,size=c(100,50,2),learnFuncParams = 0.3,maxit = 100,inputsTest = WKP2$inputsTest, targetsTest = WKP2$targetsTest)
system.time(mlp(WKP2$inputsTrain,WKP2$targetsTrain,size=c(100,50,2),learnFuncParams = 0.3,maxit = 100,inputsTest = WKP2$inputsTest, targetsTest = WKP2$targetsTest))
KNNPred=predict(KNNModel,WKP2$inputsTest)
KTarget=WKP2$targetsTest[,2]
table(KTarget,KNNPred[,2]>0.5) ##Katy Perry

WEm2=WEm[sample(1:nrow(WEm),length(1:nrow(WEm))),]
WEmValues=WEm2[,1:178]
WEmTargets=decodeClassLabels(WEm2[,179])
WEm2=splitForTrainingAndTest(WEmValues,WEmTargets,ratio = 0.25)
ENNModel=mlp(WEm2$inputsTrain,WEm2$targetsTrain,size=c(150,100,2),learnFuncParams = 0.3,maxit = 100,inputsTest = WEm2$inputsTest, targetsTest = WEm2$targetsTest)
system.time(mlp(WEm2$inputsTrain,WEm2$targetsTrain,size=c(150,100,2),learnFuncParams = 0.3,maxit = 100,inputsTest = WEm2$inputsTest, targetsTest = WEm2$targetsTest))
ENNPred=predict(ENNModel,WEm2$inputsTest)
ETarget=WEm2$targetsTest[,2]
table(ETarget,ENNPred[,2]>0.5) ##Eminem

WT2=WT[sample(1:nrow(WT),length(1:nrow(WT))),]
WTValues=WT2[,1:91]
WTTargets=decodeClassLabels(WT2[,92])
WT2=splitForTrainingAndTest(WTValues,WTTargets,ratio = 0.25)
TNNModel=mlp(WT2$inputsTrain,WT2$targetsTrain,size=c(60,40,2),learnFuncParams = 0.3,maxit = 100,inputsTest = WT2$inputsTest, targetsTest = WT2$targetsTest)
system.time(mlp(WT2$inputsTrain,WT2$targetsTrain,size=c(60,40,2),learnFuncParams = 0.3,maxit = 100,inputsTest = WT2$inputsTest, targetsTest = WT2$targetsTest))
TNNPred=predict(TNNModel,WT2$inputsTest)
TTarget=WT2$targetsTest[,2]
table(TTarget,TNNPred[,2]>0.5) ##Total Data Set

