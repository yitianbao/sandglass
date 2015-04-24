

setwd('C:/Users/Yitian Bao/Desktop/0424/data_test')

hebing

setwd('C:/Users/Lu Wei/Desktop/candle/data_test')



list.files=dir()
answer=0
for (i in 1:500)
{answer=c(answer,0)}
for(i in 1:12)
{answer=rbind(answer,answer)}
answerday=answer
return=0
day=0
for (index in 1:(length(list.files)))
{
	#cleaning
	mark=0
	num=1
	data=read.table(list.files[index],header=T)
	data=cbind(data,cbind(1:dim(data)[1]),cbind(1:dim(data)[1]))
	data[1,8:9]=data[1,5]
	if (dim(data)[1]>100)
	{data=cbind(data,cbind(1:dim(data)[1]),cbind(1:dim(data)[1]))
	data[1,8:9]=data[1,5]
	for (i in 2:dim(data)[1])
	{data[i,8]=data[i-1,8]*11/13+data[i,5]*2/13
	data[i,9]=data[i-1,9]*25/27+data[i,5]*2/27
	}
	data[,8]=data[,8]-data[,9]
	data[1,9]=data[1,8]
	for(i in 2:dim(data)[1])
	{data[i,9]=data[i-1,9]*8/10+data[i,8]*2/10}
	data[,9]=2*(data[,8]-data[,9])
	

#trading
	for (i in 30:dim(data)[1])
	{
		if ((mark==0) & (data[i,9]>0) &(data[(i-1),9]<=0))
		{mark=1
			card=i+1}
		if((mark==1) & (data[i,9]<0) & (data[(i-1),9]>0))
		{mark=0
			answer[index,num]=data[i+1,5]/data[card,5]
			answerday[index,num]=(-(card-i-1))
					num=num+1}
	}
	if( i==dim(data)[1] & mark==1)
	{answer[index,num]=data[i,5]/data[card,5]
		answerday[index,num]=(-(card-i))}
}
}


test=(answer-1)
meananswer=0
sdanswer=0
for (i in 1:dim(test)[1])
{meananswer=c(meananswer,sum(test[i,(which(test[i,]!=(-1)))])/sum(answerday[i,]))
sdanswer=c(sdanswer,sd(test[i,(which(test[i,]!=(-1)))]))}
meananswer=meananswer[-1]
sdanswer=sdanswer[-1]
meananswer=meananswer[1:2461]
sdanswer=sdanswer[1:2461]
q=sort(meananswer,index.return=TRUE)



record=0
for (i in 1:dim(test)[1])
{record=c(record,(sum(sign(test[i,])==(1))/sum(sign(test[i,]!=(-1)))))}
record=record[-1]



record2=0
for (i in 1:dim(test)[1])
{record2=c(record2,sum(sign(test[i,]!=(-1))))}

record2=record2[-1]



p=q$ix

mscd=cbind(p,q$x,record[p],record2[p],sdanswer[p])










0.9603960 0.9207921 0.8514851 0.8910891 0.8800000 0.8811881 0.9100000 0.8316832
 0.9900990 0.8712871 0.8811881 0.8217822 0.9009901 0.8712871 0.8514851 0.8400000 0.9702970
 0.9108911 0.9175258

-0.009519767 -0.004923351 -0.004113162 -0.003652567 -0.003586565 -0.003452143 -0.003102989
 -0.002857477 -0.002827135 -0.002754742 -0.002672440 -0.002666328 -0.002578053 -0.002456272
 -0.002433479 -0.002423755 -0.002374386 -0.002315708 -0.002292114 -0.002286419

  [1] "SZ#300377.txt" "SH#603555.txt" "SH#600705.txt" "SZ#002174.txt" "SH#600862.txt" "SH#600999.txt"
 [7] "SH#600228.txt" "SH#600469.txt" "SZ#002229.txt" "SH#603308.txt" "SH#600782.txt" "SZ#300283.txt"
[13] "SZ#300017.txt" "SZ#002699.txt" "SZ#002303.txt" "SZ#002271.txt" "SH#600208.txt" "SZ#300380.txt"
[19] "SZ#300351.txt" "SH#600282.txt"


record=0
for (i in 1:dim(test)[1])
{record=c(record,sum(sign(test[i,])!=(0)))}



#AD
for (i in 1:dim(data)[1])
{ad[i]=(2*data[i,5]-data[i,4]-data[i,3])/(data[i,3]-data[i,4])*data[i,6]}


#adtm

if (data[i,2]<=data[i-1,2])
{DTM=0}else{dtm=max(c((data[i,3]-data[i,2]),(data[i,])))}








hebing

setwd('C:/Users/Lu Wei/Desktop/data/daycandle')
list.files=dir()
answer=0
for (i in 1:100)
{answer=c(answer,0)}
for(i in 1:12)
{answer=rbind(answer,answer)}
answerday=answer
return=0
day=0
for (index in 1:(length(list.files)))
{
	#cleaning
	mark=0
	num=1
	data=read.table(list.files[index],header=T)


	data=read.table('600000.txt')
	for (i in 2:dim(data)[1])
	{data[i,8]=data[i-1,8]*11/13+data[i,5]*2/13
	data[i,9]=data[i-1,9]*25/27+data[i,5]*2/27
	}
	data[,8]=data[,8]-data[,9]
	data[1,9]=data[1,8]
	for(i in 2:dim(data)[1])
	{data[i,9]=data[i-1,9]*8/10+data[i,8]*2/10}
	data[,9]=2*(data[,8]-data[,9])
	

	data=read.table('600000.txt')
	CCI(data[,3:5])




	for (i in 2:dim(data)[1])
	{data[i,8]=data[i-1,8]*11/13+data[i,5]*2/13
	data[i,9]=data[i-1,9]*25/27+data[i,5]*2/27
	}
	data[,8]=data[,8]-data[,9]
	data[1,9]=data[1,8]
	for(i in 2:dim(data)[1])
	{data[i,9]=data[i-1,9]*8/10+data[i,8]*2/10}
	data[,9]=2*(data[,8]-data[,9])

library(RMySQL)



answer=0
for (i in 1:500)
{answer=c(answer,0)}
for(i in 1:12)
{answer=rbind(answer,answer)}
answerday=answer
return=0
day=0

conn <- dbConnect(MySQL(), user='yitian', password="SyncHedge", dbname='stock', host="192.168.1.2")


m = dbGetQuery(conn, "select distinct(ticker) from bar_day")
for (index in 1:dim(m)[1])
{	mark=0
	num=1
	data = dbGetQuery(conn, paste("select * from bar_day where ticker='", m[index,1],  "' order by dt", sep=""))
	if (dim(data)[1]>100)
	{data=cbind(data,cbind(1:dim(data)[1]),cbind(1:dim(data)[1]))
	data[1,8:9]=data[1,5]
	for (i in 2:dim(data)[1])
	{data[i,8]=data[i-1,8]*11/13+data[i,5]*2/13
	data[i,9]=data[i-1,9]*25/27+data[i,5]*2/27
	}
	data[,8]=data[,8]-data[,9]
	data[1,9]=data[1,8]
	for(i in 2:dim(data)[1])
	{data[i,9]=data[i-1,9]*8/10+data[i,8]*2/10}
	data[,9]=2*(data[,8]-data[,9])
	

#trading
	for (i in 30:dim(data)[1])
	{
		if ((mark==0) & (data[i,9]>0) &(data[(i-1),9]<=0))
		{mark=1
			card=i+1}
		if((mark==1) & (data[i,9]<0) & (data[(i-1),9]>0))
		{mark=0
			answer[index,num]=data[i+1,5]/data[card,5]
			answerday[index,num]=(card-i-1)
					num=num+1}
	}
	if( i==dim(data)[1] & mark==1)
	{answer[index,num]=data[i,5]/data[card,5]
		answerday[index,num]=(-(card-i))}
}
}


test=(answer-1)
meananswer=0
for (i in 1:dim(test)[1])
{meananswer=c(meananswer,sum(test[i,(which(test[i,]!=(-1)))])/sum(answerday[i,]))}



record=0
for (i in 1:dim(test)[1])
{record=c(record,(sum(sign(test[i,])==(1))/sum(sign(test[i,])!=(-1))))}


record=0
for (i in 1:dim(test)[1])
{record=c(record,(sum(sign(test[i,])!=(-1))))}









library('TTR')

list.files=dir()
answer=0
for (i in 1:500)
{answer=c(answer,0)}
for(i in 1:12)
{answer=rbind(answer,answer)}
answerday=answer
return=0
day=0
for (index in 1:(length(list.files)))
{
	#cleaning
	mark=0
	num=1
	data=read.table(list.files[index],header=T)
	data[,7]=CCI(n=14,data[,3:5])
	

#trading
	for (i in 30:dim(data)[1])
	{
		if (((data[i,7]>100) & (data[(i-1),7]<100)) |((data[i,7]>(-100))&(data[(i-1),7]<(-100))))
		{mark=1
			card=i+1}
		if((mark==1) & (((data[i,7]<100) & (data[(i-1),7]>100)) |((data[i,7]<(-100))&(data[(i-1),7]>(-100)))))
		{mark=0
			answer[index,num]=data[i+1,5]/data[card,5]
			answerday[index,num]=(-(card-i-1))
					num=num+1}
	}
	if( i==dim(data)[1] & mark==1)
	{answer[index,num]=data[i,5]/data[card,5]
		answerday[index,num]=(-(card-i))}
}




test=(answer-1)
meananswer=0
sdanswer=0
for (i in 1:dim(test)[1])
{meananswer=c(meananswer,sum(test[i,(which(test[i,]!=(-1)))])/sum(answerday[i,]))
sdanswer=c(sdanswer,sd(test[i,(which(test[i,]!=(-1)))]))}
meananswer=meananswer[-1]
sdanswer=sdanswer[-1]
meananswer=meananswer[1:2461]
sdanswer=sdanswer[1:2461]
q=sort(meananswer,index.return=TRUE)

record=0
for (i in 1:dim(test)[1])
{record=c(record,(sum(sign(test[i,])==(1))/sum(sign(test[i,]!=(-1)))))}
record=record[-1]
record=record[1:2461]


record2=0
for (i in 1:dim(test)[1])
{record2=c(record2,sum(sign(test[i,]!=(-1))))}
record2=record2[-1]
record2=record2[1:2461]



p=q$ix

cci=cbind(p,q$x,record[p],record2[p],sdanswer[p])



RSI


library('TTR')

list.files=dir()
answer=0
for (i in 1:500)
{answer=c(answer,0)}
for(i in 1:12)
{answer=rbind(answer,answer)}
answerday=answer
return=0
day=0
for (index in 1:(length(list.files)))
{
	#cleaning
	mark=0
	num=1
	data=read.table(list.files[index],header=T)
	data[,6]=RSI(n=6,data[,5])
	data[,7]=RSI(n=12,data[,5])

#trading
	for (i in 30:dim(data)[1])
	{
		if (((data[i,6]>data[i,7]) & (data[(i-1),6]<data[(i-1),7])))
		{mark=1
			card=i+1}
		if((mark==1) & ((data[i,6]<data[i,7]) & (data[(i-1),6]>data[(i-1),7])))
		{mark=0
			answer[index,num]=data[i+1,5]/data[card,5]
			answerday[index,num]=(-(card-i-1))
					num=num+1}
	}
	if( i==dim(data)[1] & mark==1)
	{answer[index,num]=data[i,5]/data[card,5]
		answerday[index,num]=(-(card-i))}
}




test=(answer-1)
meananswer=0
sdanswer=0
for (i in 1:dim(test)[1])
{meananswer=c(meananswer,sum(test[i,(which(test[i,]!=(-1)))])/sum(answerday[i,]))
sdanswer=c(sdanswer,sd(test[i,(which(test[i,]!=(-1)))]))}
meananswer=meananswer[-1]
sdanswer=sdanswer[-1]
meananswer=meananswer[1:2461]
sdanswer=sdanswer[1:2461]
q=sort(meananswer,index.return=TRUE)

record=0
for (i in 1:dim(test)[1])
{record=c(record,(sum(sign(test[i,])==(1))/sum(sign(test[i,]!=(-1)))))}
record=record[-1]
record=record[1:2461]


record2=0
for (i in 1:dim(test)[1])
{record2=c(record2,sum(sign(test[i,]!=(-1))))}
record2=record2[-1]
record2=record2[1:2461]



p=q$ix

rsi=cbind(p,q$x,record[p],record2[p],sdanswer[p])

