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