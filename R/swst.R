swstNP <- function(x,...)
{
  swst(x,parantheses=FALSE)
}

swst <- function(x,...)
{

### CLASS "htest" ###
 if ("htest"%in%class(x))
 {
   

### chisq.test() ###
   if (any(grepl("X-squared|chi-square",names(x$statistic))))
   {
     # Extract Statistics:
     stat <- x$statistic['X-squared']
     df <- x$parameter['df']
     pval <- x$p.value
     
     return(swp("\\\\chi^2",stat,pval,df,...))
   }   
   
   
# If all else fails:
     stat <- x$statistic
     df <- x$parameter
     pval <- x$p.value
     
     return(swp(names(x$statistic),stat,pval,df,...))
 }
 


# aov() 
 if ("aov"%in%class(x))
 {
   return(swst(anova(x),...))
 } 

 ### lm()
 if ("lm"%in%class(x))
 {
    sum <- summary(x)
    stat <- sum$fstatistic['value']
    df1 <- sum$fstatistic['numdf']
    df2 <- sum$fstatistic['dendf']
    pval <- pf(stat,df1,df2,lower.tail=FALSE)
   
    return(swp("F",stat,pval,c(df1,df2),...))
 }
 
 ### anova ###
  if ("anova"%in%class(x))
  {
    n <- nrow(x)-1
    res <- character(n)
    names(res) <- rownames(x)[1:n]
    for (i in 1:n)
    {
      if ("num Df"%in%names(x) & "den Df"%in%names(x))
      {
        res[i] <- swp("F",x[i,grepl('approx F|F value',names(x))],x[i,'Pr(>F)'],c(x[i,'num Df'],x[i,'den Df']),...)
      } else
      {
        res[i] <- swp("F",x[i,grepl('approx F|F value',names(x))],x[i,'Pr(>F)'],c(x[i,'Df'],x[n+1,'Df']),...) 
      }
    }
    return(res)
  }
 
 stop("The class of your object is not yet supported by swst.\n\nPlease contact me (sacha.epskamp@gmail.com) with information on your object.")

}