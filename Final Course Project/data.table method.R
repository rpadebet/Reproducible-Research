X<-StormData_sub
Y<-as.data.table(mapping)



dfunc<-function(){
Z<-merge(X,Y,by.x = "CROPDMGEXP",by.y = "EXP",all.x = TRUE )
setnames(Z,"VAL","CROPEXP")
Z<-merge(Z,Y,by.x = "PROPDMGEXP",by.y = "EXP",all.x = TRUE )
setnames(Z,"VAL","PROPEXP")
Z[is.na(CROPEXP),CROPEXP:=1]
Z[is.na(PROPEXP),PROPEXP:=1]
Z[,Tot_damage := (PROPDMG*PROPEXP)+(CROPDMG*CROPEXP)]
#summary(Z)
}

Rprof()
dfunc()
Rprof(NULL)
summaryRprof()


Rprof()
    Data_df <- Data_tbl%>%
        left_join(y = mapping,by= c("CROPDMGEXP"="EXP"))%>%
        rename(CROPEXP = VAL)%>%
        mutate(CROPEXP = replace(CROPEXP,which(is.na(CROPEXP)),1))%>%
        left_join(y = mapping,by= c("PROPDMGEXP"="EXP"))%>%
        rename(PROPEXP = VAL)%>%
        mutate(PROPEXP = replace(PROPEXP,which(is.na(PROPEXP)),1))%>%
        mutate(Tot_damage = (PROPDMG*PROPEXP)+(CROPDMG*CROPEXP))
Rprof(NULL)
summaryRprof()
