# Function to plot interactions of two categorical variables and a numeric response
plot_categorical_interactions <- function(df, response, grp, x2){
    mysummary<-function(x){
    result<-c(length(x),mean(x),sd(x),sd(x)/length(x),min(x),max(x),IQR(x))
    names(result)<-c("N","Mean","SD","SE","Min","Max","IQR")
    return(result)
  }
  sumstats<-aggregate(response~grp*x2,data=df,mysummary)
  sumstats<-cbind(sumstats[,1:2],sumstats[,-(1:2)])
  
  ggplot(sumstats,aes(x=x2,y=Mean,group=grp,colour=grp))+
    ylab("Iridium")+
    geom_line()+
    geom_point()+
    geom_errorbar(aes(ymin=Mean-SE,ymax=Mean+SE),width=.1)
}

## USAGE ##
plot_categorical_interactions(df = df, response = df$age, grp = df$sex, x = df$occupation)
