if(!file.exists("summarySCC_PM25.rds")| !file.exists("Source_Classification_Code.rds")) {
    download.file("https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip","data.zip")
    unzip("data.zip")
}
data<-readRDS("summarySCC_PM25.rds")
code<-readRDS("Source_Classification_Code.rds")

if(!("dplyr" %in% installed.packages())) install.packages("dplyr")
library(dplyr)
motor<-as.character(code[c(grepl("motor", code$SCC.Level.Three, ignore.case = T),
                           grepl("motorcycles", code$SCC.Level.Four, ignore.case = T)),1])
bal_mot<-filter(data, fips == "24510", SCC %in% motor)
summ<-group_by(bal_mot, year) %>% summarize(total=sum(Emissions))

onroad<-filter(bal_mot, type=="ON-ROAD")
summ_onroad<-group_by(onroad, year) %>% summarize(total=sum(Emissions))


if(!("ggplot2" %in% installed.packages())) install.packages("ggplot2")
library(ggplot2)
library(gridExtra)
f<-qplot(year,Emissions, data=bal_mot, colour=type,
         ylab="PM2.5 emitted (tons)")+ geom_point(alpha=.5, cex=2) +ggtitle("PM2.5 Emissions in Baltimore City \nfrom motor vehicle sources",
                                                                            subtitle="Individual sources")
g<-qplot(year, total, data=summ,
         ylab="PM2.5 emitted (tons)") + geom_line()+ ggtitle(" \n ", subtitle="Total emissions")

h<-qplot(year,Emissions, data=onroad,
         ylab="PM2.5 emitted (tons)")+ geom_point(alpha=.5, cex=3, colour="skyblue")+ geom_smooth() +ggtitle("Zoom on ON-ROAD sources",
                                                                                                             subtitle="Individual ON-ROAD sources")
i<-qplot(year, total, data=summ_onroad,
ylab="PM2.5 emitted (tons)") + geom_line() +ggtitle(" ", subtitle="Total emissions from ON-ROAD sources")

a<-grid.arrange(f,g,h,i, ncol=2, nrow=2)

ggsave("plot5.png",a)

