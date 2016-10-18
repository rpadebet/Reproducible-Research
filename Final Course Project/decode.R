


## Function to compute total Dollar Damage

damage<-function(v){
    total = 0
    
    PROPDMG <- ifelse(length(v['PROPDMG'])==0,0,as.numeric(v['PROPDMG']))
    CROPDMG <- ifelse(length(v['CROPDMG'])==0,0,as.numeric(v['CROPDMG']))
    
    PEXP <- decode(v['PROPDMGEXP'])
    CEXP <- decode(v['CROPDMGEXP'])
    
    total = total + (PROPDMG*PEXP)+(CROPDMG*CEXP)
    
    return(total)
}

# Decode function used in Damage function
decode<-function(inp){
    if(length(inp)==0){
        return (1)
    }
    else{
        key <- c("K","M","B","0","")
        value<- c(1000,1000000,1000000000,1,1)
        df<-data.frame(key,value)
        names(df)<-c("EXP","CODE")
        res = df[df$EXP==inp,]$CODE
        return(res)
    }
    
}