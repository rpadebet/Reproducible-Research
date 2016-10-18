
# Function used to decode the EXP in the dataset
decoder<-function(v){
    
    if(length(v[2])==0){return (1)}
    else if(v[2]%in% c("H","h")) {return(100)}
    else if(v[2]%in% c("K","k")) {return(1000)}
    else if(v[2]%in% c("M","m")) {return(1000000)}
    else if(v[2]%in% c("B","b")) {return(1000000000)}
    else if(v[2]%in% c("0","1","2","3","4","5","6","7","8")) {10^as.numeric(v[2])}
    else if(v[2]=="") {return(1)} 
    else return(1)
}

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