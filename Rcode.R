hist(runif(1000))

mns = NULL
for (i in 1 : 1000) mns = c(mns, mean(runif(40)))
hist(mns)




set.seed(1984)
avg <- NULL
nosim <- 1000
for(i in 1:nosim) avg[i] <- mean(rexp(40, rate = 0.2))
 
a <- rexp(1000, rate=.2)
b <- avg

a <- cbind(a,0)
b <- cbind(b,1)
d <- rbind(a,b)
d <- as.data.frame(d)
tail(d)

names(d) <- c("Number","exp")
d$exp <- as.factor(d$exp)
library(dplyr)
e <- summarise(group_by(d,exp), means = mean(Number))
e
library(ggplot2)

x <- 
        ggplot(d, aes(d$Number, fill=d$exp)) +
        geom_histogram(binwidth=.6, alpha=.5, position="dodge") +
        scale_fill_manual(values=c("#009966", "#CC0000"), 
                          name="Distribution",
                          breaks=c(0, 1),
                          labels=c("Exponential", "Mean of sample means")) +
        annotate("text", label = "Mean", x = 7, y = 320, size = 4, color="grey") + 
        labs(title = "Histogramm", x = "Number", y = "Count") +
        geom_vline(data=e, aes(xintercept=e$means,  colour=e$exp), linetype="dashed", size=1) +
        xlim(0,25) 
        

ggplot(v, aes(x=avg)) +
        geom_histogram(binwidth=.5, colour="black", fill="white")


v <- as.data.frame(cbind(avg,0))
class(d$Number)


hist(avg)
mean(avg)
1/0.2
sd(avg)
1/(.2)

theoretical_mean <- 1/0.2

ggplot(hist_data1, aes(x=avg)) +
        geom_histogram(binwidth=.4, colour="black", fill="white") +
        labs(title = "Histogramm of sample means (n = 40)", x = "Sample mean", y = "Count") +
        geom_vline(data=hist_data1, aes(xintercept=mean(avg),  
                                        colour="red"), linetype="dashed", size=1) +
        geom_vline(aes(xintercept=theoretical_mean,  colour="cyan"), linetype="dashed", size=1) +
        annotate("text", label = "Mean of sample means", x = 6, y = 220, size = 4, color="red") +
        annotate("text", label = "Theoretical mean", x = 4.2, y = 220, size = 4, color="#009966") 


a <- rexp(1000, rate=.2)
b <- avg

a <- cbind(a,0)
b <- cbind(b,1)
d <- rbind(a,b)
d <- as.data.frame(d)
names(d) <- c("Number","exp")
d$exp <- as.factor(d$exp)
e <- summarise(group_by(d,exp), means = mean(Number))

plot3 <- ggplot(d, aes(d$Number, fill=d$exp)) +
                geom_density(alpha=.3) +
                scale_fill_manual(values=c("#009966", "#CC0000"), 
                          name= "Distribution",
                          breaks=c(0, 1),
                          labels=c("Exponential", "Mean of sample means")) +
                annotate("text", label = "Mean", x = 7, y = .45, size = 4, color="black") + 
                labs(title = "Density plot", x = "Number", y = "Count") +
                geom_vline(data=e, aes(xintercept=e$means,  colour=e$exp), linetype="dashed", size=1) +
                xlim(0,30)
        
