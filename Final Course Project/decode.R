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

decoder<-function(v){
    
    if(length(v[2])==0){return (1)}
    else if(v[2]=="K") {return(1000)}
    else if(v[2]=="M") {return(1000000)}
    else if(v[2]=="B") {return(1000000000)}
    else if(v[2]=="0") {return(1)}
    else if(v[2]=="") {return(1)} 
    else return(1)
    }
    
