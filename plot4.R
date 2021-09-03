if(!file.exists("summarySCC_PM25.rds")| !file.exists("Source_Classification_Code.rds")) {
    download.file("https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip","data.zip")
    unzip("data.zip")
}
data<-readRDS("summarySCC_PM25.rds")
code<-readRDS("Source_Classification_Code.rds")

if(!("dplyr" %in% installed.packages())) install.packages("dplyr")
library(dplyr)
coal<-as.character(code[grepl("coal", code$Short.Name, ignore.case = T),1])
coal_data<-filter(data, SCC %in% coal)

coal_code<-filter(code, SCC %in% coal_data$SCC)
coal_data<-merge(coal_data, coal_code[,c(1,7)], by="SCC")

summ<-group_by(coal_data, year) %>% summarize(total=sum(Emissions))
summ_type<-group_by(coal_data, SCC.Level.One, year) %>% summarize(total=sum(Emissions))
summ_type<-rbind(summ_type,data.frame(SCC.Level.One=c("Internal Combustion Engines", "Waste Disposal"), year=1999, total=0))


if(dev.cur()!=1) dev.off() #to reset par()
par(mfrow=c(1,2), oma=c(0,0,1,0))

with(coal_data, boxplot(log10(Emissions +1)~ year, pch=19, main="Individual sources"))
with(summ,plot(year, total, type="l",lwd=3, ylim=c(0, max(total)), xlab = "year",
               ylab="PM2.5 emitted (tons)", main="Total emissions"))
tapply(summ_type$total, as.factor(summ_type$SCC.Level.One),
       function(x) {lines(c(summ$year), x, type="l", col=x)})

legend("center", lwd=c(3, rep(1,5)), col=c(1, "transparent",5,2,1,8),
       legend=c("All sources", "by type:", unique(summ_type$SCC.Level.One)[c(1,5,2)],
                "Other"),cex=0.8, bty="n")

mtext("US PM2.5 emissions from coal combustion-related sources",
      outer=T, side=3, line=-1, font=2, cex=1.3)

dev.copy(png,"plot4.png", width=600)
dev.off()