if(!file.exists("summarySCC_PM25.rds")) {
    download.file("https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip","data.zip")
    unzip("data.zip")
}
data<-readRDS("summarySCC_PM25.rds")

if(!("dplyr" %in% installed.packages())) install.packages("dplyr")
library(dplyr)
summ<-group_by(data, year) %>% summarize(total=sum(Emissions))
summ_type<-group_by(data, type, year) %>% summarize(total=sum(Emissions))

if(dev.cur()!=1) dev.off() #to reset par()
with(summ,plot(year, total, type="b", pch=19, lwd=3,
ylim=c(0, max(total)), xlab = "Year", ylab="PM2.5 emitted (tons)",
main="Total US PM2.5 emissions"))

tapply(summ_type$total, as.factor(summ_type$type),
       function(x) {lines(c(summ$year), x, type="b", pch=23, col=x[3], bg=x[3])})

legcols<-tapply(summ_type$total, as.factor(summ_type$type), function(x) {col=x[3]})
legend("topright",inset=c(.05,0), lwd=c(3, rep(1,5)), col=c(1, "transparent",legcols),
       legend=c("All sources", "by type:", names(legcols)),cex=0.8, bty="n")

dev.copy(png,"plot1.png")
dev.off()