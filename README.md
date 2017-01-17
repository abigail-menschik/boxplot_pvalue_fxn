#BOXPLOT_P-VALUE_FXN VERSION 1.1: nonparametric data

pbf <- function(dat) {
  library(ggplot2)
  bp <- ggplot(dat, aes(Group, Value)) + stat_boxplot(geom = "errorbar", width = 0.25) + geom_boxplot() #generate boxplot
  # We'll be using the Mann-Whitney U test to determine significance
  
  #running the test btwn each group
  test1_2 <- wilcox.test(which(dat$Group==1),which(dat$Group==2))$p.val
  test1_3 <- wilcox.test(which(dat$Group==1),which(dat$Group==3))$p.val
  test2_3 <- wilcox.test(which(dat$Group==2),which(dat$Group==3))$p.val
  
  # defining data frames for significance bars
  df1 <- data.frame(a = c(1:5), b = c(1:5))
  df2 <- data.frame(a = c(1:4), b = c(1:4))
  df3 <- data.frame(a = c(1:4), b = c(1:4))
  
  #now to add the significance bars
  bp + geom_line(data=df1,aes(x = c(1,1,2,3,3), y = c(max(dat$Value)+25,max(dat$Value)+26,max(dat$Value)+26,max(dat$Value)+26,max(dat$Value)+25))) +
    annotate("text", x = 2, y = max(dat$Value)+30, label = if(test1_3<=0.05)"*"else"n.s.", size = 5) + 
    geom_line(data=df2,aes(x = c(1,1,2,2), y = c(max(dat$Value)+15,max(dat$Value)+16,max(dat$Value)+16,max(dat$Value)+15))) +
    annotate("text", x = 1.5, y = max(dat$Value)+20, label = if(test1_2<=0.05)"**"else"n.s.", size = 5) +
    geom_line(data=df3,aes(x = c(2,2,3,3), y = c(max(dat$Value)+5,max(dat$Value)+6,max(dat$Value)+6,max(dat$Value)+5))) +
    annotate("text", x = 2.5, y = max(dat$Value)+10, label = if(test2_3<=0.05)"***"else"n.s.", size = 5)
}

#BOXPLOT_P-VALUE_FXN VERSION 1.2: independent parametric data

pbfII <- function(dat) {
  library(ggplot2)
  dat$Group <- as.factor(dat$Group) #formatting data
  bp <- ggplot(dat, aes(Group, Value)) + stat_boxplot(geom = "errorbar", width = 0.25) + geom_boxplot() #generate boxplot
  # We'll be using a 2 sample t-test to determine significance

  #running the test btwn each group
  test1_2 <- t.test(which(dat$Group==1),which(dat$Group==2), paired=FALSE)$p.val
  test1_3 <- t.test(which(dat$Group==1),which(dat$Group==3), paired=FALSE)$p.val
  test2_3 <- t.test(which(dat$Group==2),which(dat$Group==3), paired=FALSE)$p.val
  
  # defining data frames for significance bars
  df1 <- data.frame(a = c(1:5), b = c(1:5))
  df2 <- data.frame(a = c(1:4), b = c(1:4))
  df3 <- data.frame(a = c(1:4), b = c(1:4))
  
  #now to add the significance bars
  bp + geom_line(data=df1,aes(x = c(1,1,2,3,3), y = c(max(dat$Value)+25,max(dat$Value)+26,max(dat$Value)+26,max(dat$Value)+26,max(dat$Value)+25))) +
    annotate("text", x = 2, y = max(dat$Value)+30, label = if(test1_3<=0.05)"*"else"n.s.", size = 5) + 
    geom_line(data=df2,aes(x = c(1,1,2,2), y = c(max(dat$Value)+15,max(dat$Value)+16,max(dat$Value)+16,max(dat$Value)+15))) +
    annotate("text", x = 1.5, y = max(dat$Value)+20, label = if(test1_2<=0.05)"**"else"n.s.", size = 5) +
    geom_line(data=df3,aes(x = c(2,2,3,3), y = c(max(dat$Value)+5,max(dat$Value)+6,max(dat$Value)+6,max(dat$Value)+5))) +
    annotate("text", x = 2.5, y = max(dat$Value)+10, label = if(test2_3<=0.05)"***"else"n.s.", size = 5)
}
