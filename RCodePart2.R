library(datasets); library(ggplot2); library(dplyr); library(xtable)
library(RColorBrewer)

data(ToothGrowth)
options(xtable.comment = FALSE)

df <- as.tbl(ToothGrowth)
df$Dose <- as.factor(a$Dose)

plot(df)
ggplot(df, aes(sample = len)) + 
        stat_qq() + 
        labs(title="QQ-Plot of Tooth Length") + 
        labs(x="Theoretical Quantiles", y="Sample Quantiles")
?par
par(mfrow=c(1,2))
hist(df$len[df$Supplement=="VC"])
hist(df$len[df$Supplement=="OJ"])
par(mfrow=c(1,3))
hist(df$len[df$Dose==0.5],breaks=10)
hist(df$len[df$Dose==1],breaks=5)
hist(df$len[df$Dose==2],breaks=10)


######################################################################################
df.mean <- df %>% group_by(Dose) %>% summarise(mean = mean(len))

DensCurv <- 
  ggplot(df, aes(len, fill = Dose)) +
    geom_density(alpha=.3) + 
    scale_fill_brewer(palette="Set1") +
    labs(title="Density Curves by Dosage") + 
    labs(x="Length", y="Density") +
    geom_vline(data=df.mean, aes(xintercept=mean,  colour=Dose),
        linetype="dashed", size=1) +
    scale_colour_brewer(palette="Set1") +
    annotate("text", label = "Mean", x = 13, y = 0.13, size = 5, color="darkgrey") +
    annotate("text", label = "Mean", x = 22, y = 0.13, size = 5, color="darkgrey") +
    annotate("text", label = "Mean", x = 28, y = 0.13, size = 5, color="darkgrey") 
######################################################################################
my.cols <- brewer.pal(5, "Blues")
qq <- 
  ggplot(df, aes(sample = len, color = Dose)) + 
    stat_qq() + 
    scale_colour_brewer(type = "seq", palette = 3) + 
    labs(title="QQ-Plot of Tooth Length") + 
    labs(x="Theoretical Quantiles", y="Sample Quantiles")
######################################################################################



group0.5 <- df %>% filter(Dose == 0.5) 
group1   <- df %>% filter(Dose == 1) 
group2    <- df %>% filter(Dose == 2) 

p <- rbind(
        t.test(gr2$len ~ gr0.5$len)$p.value,
        t.test(gr2$len ~ gr1$len)$p.value,
        t.test(gr1$len ~ gr0.5$len)$p.value)

ci <- rbind(
        t.test(gr2$len ~ gr0.5$len)$conf.int,
        t.test(gr2$len ~ gr1$len)$conf.int,
        t.test(gr1$len ~ gr0.5$len)$conf.int)
ma <- cbind(ci,round(p,9))
rownames(ma) <- c("2mg vs 0.5mg","2mg vs 1mg","1mg vs 0.5mg")
colnames(ma) <- c("CI upper","CI lower","p-value")

print(xtable(ma,align="cccc"), include.rownames=TRUE)


box <- 
  ggplot(df, aes(x=Dose, y=len, fill=Dose)) + 
    geom_boxplot() +
    scale_fill_brewer(palette="Purples") + 
    labs(title="Boxplot grouped by Dosage") + 
    labs(x="Dosis", y="Length") +
    guides(fill=FALSE) +
    stat_summary(fun.y=mean, geom="point", shape=5, size=4)


ggplot(df, aes(Dose, len)) +
        geom_boxplot(aes(fill = Dose)) + 
        facet_grid(. ~ Supplement) +
        scale_fill_brewer(palette="Set1") + 
        labs(title="Boxplot of Length grouped by Dosage and Supplement") + 
        labs(x="Dosis", y="Length") +
        guides(fill=FALSE) +
        stat_summary(fun.y=mean, geom="point", shape=5, size=4)

   

t1 <- subset(df,Supplement == "OJ")$len
t2 <- subset(df,Supplement == "VC")$len
t.test(t1,t2,paired=F,var.equal=F)

#---------------------------------------------------------------------------------#
#


p <- rbind(
        with(df, t.test(len[Dose == 2], len[Dose == 0.5])$p.value),
        with(df, t.test(len[Dose == 2], len[Dose == 1])$p.value),
        with(df, t.test(len[Dose == 1], len[Dose == 0.5])$p.value))

ci <- round(rbind(
        with(df, t.test(len[Dose == 2], len[Dose == 0.5])$conf.int),
        with(df, t.test(len[Dose == 2], len[Dose == 1])$conf.int),
        with(df, t.test(len[Dose == 1], len[Dose == 0.5])$conf.int)),2)

ma <- cbind(ci,round(p,9))
for(i in 1:3) {
        ifelse (as.numeric(ma[i,3]) < 0.001,ma[i,3] <- "< 0.001",ma[i,3])
}
rownames(ma) <- c("2 mg vs 0.5 mg","2 mg vs 1 mg","1 mg vs 0.5 mg")
colnames(ma) <- c("95% CI lower","95% CI upper","p-value")




#----------------------------------------------------------------------------


p2  <- round(with(df, t.test(len ~ Supplement)$p.value),3)        
ci2 <- round(with(df, t.test(len ~ Supplement)$conf.int),3)
ma2 <- matrix(c(ci2,p2), ,nrow =1)
colnames(ma2) <- c("95% CI lower","95% CI upper","p-value")
rownames(ma2) <- "Orange Juice vs Ascorbic Acid"

# 4fach test ----------------------------------------------------------------
oj <- df %>% filter(Supplement == "OJ")
vc <- df %>% filter(Supplement == "VC")

testmatrix(oj)


testmatrix <- function(x) {
        p <- rbind(
                with(x, t.test(len[Dose == 2], len[Dose == 0.5])$p.value),
                with(x, t.test(len[Dose == 2], len[Dose == 1])$p.value),
                with(x, t.test(len[Dose == 1], len[Dose == 0.5])$p.value))
        
        ci <- round(rbind(
                with(x, t.test(len[Dose == 2], len[Dose == 0.5])$conf.int),
                with(x, t.test(len[Dose == 2], len[Dose == 1])$conf.int),
                with(x, t.test(len[Dose == 1], len[Dose == 0.5])$conf.int)),2)
        
        ma <- cbind(ci,round(p,9))
        for(i in 1:3) {
                ifelse (as.numeric(ma[i,3]) < 0.001,ma[i,3] <- "< 0.001",round(ma[i,3],3)
        }
        rownames(ma) <- c("2 mg vs 0.5 mg","2 mg vs 1 mg","1 mg vs 0.5 mg")
        colnames(ma) <- c("95% CI lower","95% CI upper","p-value")
        
        return(ma)
}

#------------------------------------------------------------------------

df.sum <- df %>% group_by(Supplement, Dose) %>% 
        summarise(n       = length(len), 
                  meanlen = mean(len),
                  sd      = sd(len),
                  ci      = qt(0.975, length(len)-1)*sd(len)/sqrt(length(len)))

bar <- 
        ggplot(df.sum, aes(x=Dose, y=meanlen, fill=Supplement)) + 
        geom_bar(position=position_dodge(), stat="identity", size=.3, color = "black") +  
        scale_fill_brewer(palette="Paired", name="Supplement", 
                          breaks=c("OJ", "VC"),
                          labels=c("Orange juice", "Ascorbic acid")) +
        geom_errorbar(aes(ymin=meanlen-df.sum$ci, ymax=meanlen+df.sum$ci),
                     width=.2,                    
                     position=position_dodge(.9)) + 
        xlab("Dose (mg)") +
        ylab("Average Length") +
        ggtitle(expression(atop("Dose and Supplement affect Odontoblast length", 
                   atop(italic("Average Length and Confidence Intervals"), "")))) +
        theme(plot.title = element_text(size = 22), axis.title = element_text(size = 18)) +
        theme_bw()


ggplot(df, aes(sample = len, color = Dose)) + 
        stat_qq(geom="point",shape = 20, size=4) + 
        scale_color_brewer(palette="BuGn") + 
        labs(title="QQ-Plot of Length") + 
        labs(x="Theoretical Quantiles", y="Sample Quantiles") +
        theme(plot.title = element_text(size = 22), axis.title = element_text(size = 18)) +
        theme_bw()
