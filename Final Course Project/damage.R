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


