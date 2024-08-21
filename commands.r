#Bibliotecas
library(reshape2)

#Diretório de trabalho e leitura de dados
path = "./AULA2/"
gdata<-subset(read.csv2(sprintf("%sGALTONDATA.csv",path), header = TRUE,sep=","),select=-c(X))
#Conversão de dados
gdata$Parents<-as.numeric(gdata$Parents)
gdata$Children<-as.numeric(gdata$Children)
gdata$Parents<-gdata$Parents*2.54
gdata$Children<-gdata$Children*2.54
#Análise exploratória
summary(gdata)
sd(gdata$Children)
sd(gdata$Parents)
##Moda
Mode(gdata$Children)
Mode(gdata$Parents)
##Amplitde total
range(gdata$Children)[2]-range(gdata$Children)[1]
range(gdata$Parents)[2]-range(gdata$Parents)[1]
##Variância
desvpadc <- (gdata$Children - mean(gdata$Children))
varic <- sum((desvpadc)^2)/(length(gdata$Children)-1)
desvpadp <- (gdata$Parents - mean(gdata$Parents))
varip <- sum((desvpadp)^2)/(length(gdata$Parents)-1)
modelo<-aov(Parents~Children,gdata)
anova(modelo)
##Tukey
gdata$Parentsf<-as.factor(gdata$Parents)
gdata$Childrenf<-as.factor(gdata$Children)
modelof<-aov(Parentsf~Childrenf,gdata)
gdatatk<-TukeyHSD(modelof)
gdatatkdf<-data.frame(gdatatk$Childrenf)
gdatatkdft<-filter(gdatatkdf, format(gdatatkdf$p.adj, scientific = F) < "0.05")
gdatatkdft<-format(gdatatkdft, scientific = F)
##Normalidade
shapiro.test(gdata$Children)
shapiro.test(gdata$Parents)
shapiro.test(resid(modelo))
gdatatk<-with(gdata,dic(Parents,Children,quali=TRUE,mcomp="tukey",sigF=0.05,sigT=0.05))
##Homogeneidade
bartlett.test(gdata$Children,gdata$Parents)
##Erro Padrão
erpadc<-sd(gdata$Children)/sqrt(length(gdata$Children))
erpadp<-sd(gdata$Parents)/sqrt(length(gdata$Parents))
##Coeficiente de Variação
cvc<-sd(gdata$Children)/mean(gdata$Children)*100
cvp<-sd(gdata$Parents)/mean(gdata$Parents)*100
##Gráfico de pontos
#ggplot(gdata, aes(x=Parents, y=Children,group=1))+geom_point(color="blue", size=1) + theme(axis.text.x = element_text(angle = 60, hjust=1))+geom_line(color="blue")
##Vioplot
vio<-ggplot(gdata, aes(x=`Parents`, y=`Children`)) + geom_violin()+ theme(legend.position="top",legend.title=element_blank(),text = element_text(size = 40))+geom_jitter(color="black", alpha=0.7)
ggsave(vio, file=sprintf("%s/OUTPUT/VIOP.png",path), width=10, height=10,limitsize=FALSE)
##Boxplot
gdatam <- melt(gdata)
boxp<-ggplot(gdatam, aes(x=`variable`, y=value,fill=variable)) + geom_boxplot()+ theme(axis.text.x = element_blank(),axis.ticks.x=element_blank(),axis.title.y = element_blank(),axis.ticks.y=element_blank(),legend.position="top",axis.title.x=element_blank(),legend.title=element_blank(),legend.text=element_text(size=40),text = element_text(size = 40))+geom_jitter(color="black", alpha=0.7)
ggsave(boxp, file=sprintf("%s/OUTPUT/BOXP.png",path), width=10, height=10,limitsize=FALSE)
##Curva ajustada
dic<-dic(gdata$Parents,gdata$Children,quali=F)
###Os valores numéricos representam os valores obtidos para o modelo quadrático (b0,b1 e b2 respectivamente)
fun.2<-function(x){469.3208-4.0681*x+0.0136*x^2}
png(sprintf("%s/OUTPUT/adjf.png",path),res = 100)
ggplot(gdata,aes(x=Parents,y=Children))+geom_point(size=0.5)+stat_summary(fun.y=mean,geom="point",color="red",size=2)+stat_function(fun=fun.2)
dev.off()
png(sprintf("%s/OUTPUT/adjc.png",path),res = 100)
ggline(gdata, x = "Parents", y = "Children",add = c("mean_se", "dotplot"))+ theme(legend.position="none")
dev.off()
png(sprintf("%s/OUTPUT/adjlm.png",path),res = 100)
ggplot(mapping = aes(x = Parents, y = Children), data = gdata) +geom_point() +geom_smooth(method = "lm", se = TRUE)
dev.off()
##Modelo Linear
gdatalm<-lm(Parents~Childrenf,gdata)
anova(gdatalm)
png(sprintf("%s/OUTPUT/cont_res.png",path),res = 100)
plot.new()
qqnorm(rstandard(gdatalm))
qqline(rstandard(gdatalm),col=2)
dev.off()
png(sprintf("%s/OUTPUT/qqnorm.png",path),res = 100)
plot(modelo, 2)
dev.off()
res_Stud<-rstandard(modelo)
png(sprintf("%s/OUTPUT/qqnormteo.png",path),res = 100)
ggqqplot(res_Stud)
dev.off()
###Contrastes
contctrs<-glht(gdatalm,linfct=mcp(Childrenf="Tukey"))
contlinfct<-as.data.frame(contctrs$linfct)