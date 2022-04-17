library(readxl)

table2 <- read_excel("table2.xlsx")
table2 = table2[-1,]

table2$`Density[a]`<-gsub("km2","",as.character(table2$`Density[a]`))
table2$`Density[a]`<-gsub("/","",as.character(table2$`Density[a]`))
table2$`Density[a]`<-gsub("sq","",as.character(table2$`Density[a]`))
table2$`Density[a]`<-gsub("mi","",as.character(table2$`Density[a]`))

table2$`Area[14]`<-gsub("km2","",as.character(table2$`Area[14]`))
table2$`Area[14]`<-gsub("sq","",as.character(table2$`Area[14]`))
table2$`Area[14]`<-gsub("mi","",as.character(table2$`Area[14]`))
class(table2$`Percent rural`)
table2$`Percent rural` <- as.numeric(table2$`Percent rural`)
table2$`Percent rural`<−table2$`Percent rural`*100
class(table2$`National Share (%)`)
table2$`National Share (%)`<−table2$`National Share (%)`*100
class(table2$`Decadal growth`)
table2$`Decadal growth` <- as.numeric(iconv(table2$`Decadal growth`, 'utf-8', 'ascii', sub=''))
table2$`Decadal growth`<−table2$`Decadal growth`*100
class(table2$`Percent urban`)
table2$`Percent urban`<−table2$`Percent urban`*100
table2$`Area[14]`<- gsub("\\s*\\([^\\)]+\\)","",as.character(table2$`Area[14]`))
table2$Population<- gsub("\\s*\\([^\\)]+\\)","",as.character(table2$Population))

table2$Rank<- gsub("\\s*\\([^\\)]+\\)","",as.character(table2$Rank))
class(table2$`Density[a]`)
table2$`Density[a]`<- gsub("\\s*\\([^\\)]+\\)","",as.character(table2$`Density[a]`))
table2$`Density[a]`<- gsub(",","",table2$`Density[a]`)
table2$`Density[a]` <- as.numeric(iconv(table2$`Density[a]`, 'utf-8', 'ascii', sub=''))

nm <- table2$`State or union territory`
barplot(table2$`Density[a]`,
        names=nm,
        
        
        col="red", cex.names = 0.7,las = 2,
        ylab = "Density",
        main = "Plot"
)

install.packages('ggplot2')
library(ggplot2)
table3 <-as.data.frame(table2)
ab<-as.data.frame(table2$`Density[a]`)
nm <- table2$`State or union territory`

ggplot(table3, aes(x=`State or union territory`,y=`Density[a]`)) +
  geom_bar(stat="identity")+ 
  scale_x_discrete(guide = guide_axis(angle = 90)) 



