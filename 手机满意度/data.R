# 载入数据
dat<-read.csv("E:/1 研一规划/课程/2 统计基础/3 关蓉的课/第三次作业/作业/mobilephone.csv")
fix(dat)
# 剔除缺失值
data<-dat[,-c(1,31:34)] # 数据只包含q1~q28和brand的信息
data<-na.omit(data)
# 检测是否有小数
sum(!round(data)==data)
# 查看异常点
summary(data)
boxplot(data$q8,data$q22,data$q28,data$q24)
i1<-which.max(data$q8)
i2<-which.max(data$q28)
i3<-which.max(data$q22)
i4<-which.max(data$q24)
data<-data[-c(i1,i2,i3,i4),]
# 载入需要用的包
install.packages("psych")
library(car)
library(psych)
library(corrplot)
# 画相关系数图检验是否可以做因子分析
data1=data[,c(1:23)] #自变量数据
data2=data[,c(1,24:29)] # 因变量数据
cormatrix<-cor(data1[,-1]) # 因变量数据相关系数矩阵
corrplot(cormatrix,type="lower",method="circle")
#corrplot(cormatrix,type="lower",add=T,method="number")

## 因子分析
# 去掉q6,q11,q16,q15,q13,q20,q21,q22
fa3=fa(data1[,-c(1,7,12,17,16,14,21,22,23)],nfactors=3,rotate="varimax",fm="pa");fa3
fa3$loadings
write.csv(fa3$loadings,file="E:/1 研一规划/课程/2 统计基础/3 关蓉的课/第三次作业/作业/loadings.csv")
head(fa3$scores)
# 将得分并入原先的数据框
data1[,c(24:26)]=round(fa3$scores,2)
names(data1)[24:26]=c("用户体验","象征价值","性价比")
# 修改brand对应到中文品牌
data1[,1]=as.character(data1[,1])
brandmap=c("2"='三星',"4"="苹果iphone","6"="HTC","7"="华为")
data1$brand=unname(brandmap[data1$brand])
data1$brand=as.factor(data1$brand)
# 箱线图
Boxplot(data1[,24]~data1$brand, id=list(method="y",cex=0.8),ylab="用户体验因子得分",xlab="",col="lightblue")
Boxplot(data1[,25]~data1$brand, id=list(method="y",cex=0.8),ylab="象征价值因子得分",xlab="",col="yellowgreen")
Boxplot(data1[,26]~data1$brand, id=list(method="y",cex=0.8),ylab="性价比因子得分",xlab="",col="lightpink")
## 得分排名
#
score1=data1[order(data1$用户体验,decreasing=TRUE)[1:10],c(1,24)]
cbind(score1,datar[order(data1$用户体验,decreasing=TRUE)[1:10],])
write.csv(score1,file="E:/1 研一规划/课程/2 统计基础/3 关蓉的课/第三次作业/作业/score1.csv")
#
score2=data1[order(data1$象征价值,decreasing=TRUE)[1:10],c(1,25)]
cbind(score2,datar[order(data1$象征价值,decreasing=TRUE)[1:10],])
write.csv(score2,file="E:/1 研一规划/课程/2 统计基础/3 关蓉的课/第三次作业/作业/score2.csv")
#
score3=data1[order(data1$性价比,decreasing=TRUE)[1:10],c(1,26)]
cbind(score3,datar[order(data1$性价比,decreasing=TRUE)[1:10],])
write.csv(score3,file="E:/1 研一规划/课程/2 统计基础/3 关蓉的课/第三次作业/作业/score3.csv")
# 雷达图
ladar1<-tapply(data1[,24],data1$brand,mean)
ladar2<-tapply(data1[,25],data1$brand,mean)
ladar3<-tapply(data1[,26],data1$brand,mean)
ladar<-data.frame("用户体验"=ladar1,"象征价值"=ladar2,"性价比"=ladar3)
write.csv(ladar,file="E:/1 研一规划/课程/2 统计基础/3 关蓉的课/第三次作业/作业/ladar.csv")
# 回归
# 构建回归数据集
datar=data.frame(experience=data1[,24],value=data1[,25],quality=data1[,26],satisfy=apply(data2[,2:4],1,mean),loyalty=apply(data2[,5:7],1,mean))
# 回归结果
lm1=lm(datar$satisfy~datar$experience+datar$value+datar$quality)
lm2=lm(datar$loyalty~datar$experience+datar$value+datar$quality)
lm1$coefficients;lm2$coefficients
par(mfrow=c(2,2));plot(lm1,which=c(1,2,3,4))
par(mfrow=c(2,2));plot(lm2,which=c(1,2,3,4))
hist(datar$satisfy,main="",ylab="频数",xlab="",col="salmon",border="white")
hist(datar$loyalty,main="",ylab="频数",,col="steelblue",border="white")
hist(log(datar$satisfy))
