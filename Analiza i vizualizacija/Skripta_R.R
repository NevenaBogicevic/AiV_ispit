

# Analiza faktora koji uticu na zadovoljstvo zivotom u jednoj zemlji

#The World Happiness Report-je svetski izvestaj koji rangira 156 zemalja po tome koliko njihovi gradjani 
#smatraju sebe srecnim. Kao potencijalni faktori koji tome doprinose uzimaju se u obzir vrednost GDP, 
#očekivano trajanje života, velikodušnost (ispitanici odgovaruju na pitanje da li doniraju novac u dobrotvorne svrhe),
#drustvena podrška (da li imaju podrsku od strane porodice ili prijatelja), osecaj slobode i 
#prisustvo korupcije u zemlji (percepcija ispitanika o postojanju korupcije).
#Istrazivanje obuhvata godišnji uzorak od 1.000 ljudi, a ukoliko je neka zemlja imala istraživanja svake 
#godine, tada je velicina uzorka 3.000 ljudi.
#Podaci se odnose na izvestaj iz 2020. godine

#Potrebni paketi
library(dplyr)
library(tidyr)
library(ggplot2)


# Ucitavanje baze

WHR_data<-read.csv("Data/WorldHappinessReport2020.csv", stringsAsFactors = FALSE) 

glimpse(WHR_data)
# Baze sadrzi podatke za 153 zemlje

#Priprema podataka za analizu

# Provera nedostajucih vrednosti

sum(is.na(WHR_data))
# Nema nedostajucih vrednosti

# Odabir varijabli koje cemo ukljuciti u istrazivanje i preimenovanje kolona radi lakseg snalazenja
 
WHR_data20 <- WHR_data %>%
               select(Country.name,Regional.indicator,Ladder.score,Logged.GDP.per.capita,Social.support,
                       Healthy.life.expectancy,Generosity,Perceptions.of.corruption) %>%
               rename(Country=Country.name,Happ_Score=Ladder.score,GDP_perCapita=Logged.GDP.per.capita,
                       Life_exp=Healthy.life.expectancy,Corruption=Perceptions.of.corruption)

 glimpse(WHR_data20)

###Eksplorativna analiza

 # Deset 'najsrecnijih' zemalja 

 Top_10<- WHR_data20 %>% select (Country,Happ_Score) %>%
                    arrange(desc(Happ_Score)) %>%
                     head(n=10)
 Top_10

# Graficki prikaz

  ggplot(Top_10, aes(x = reorder(Country, Happ_Score), y = Happ_Score)) +
               geom_bar(stat = "identity",fill="Coral",width =0.3) + coord_flip()+ 
               labs(x = "Country", y = "Score", title = "The top 10 happiest countries") 

# Deset najnize rangiranih zemalja

Top_10 <- WHR_data20 %>% select (Country,Happ_Score) %>%
  arrange(Happ_Score) %>%
  head(n=10)

ggplot(Top_10, aes(x = reorder(Country, -Happ_Score), y = Happ_Score)) +
  geom_bar(stat = "identity",fill="burlywood1",width =0.3) + coord_flip()+ 
  labs(x = "Country", y = "Score", title = "The top ten saddest countries") 


# 'Najsrecnija' zemlja

WHR_data20[(which.max(WHR_data20$ Happ_Score)), c(1,3) ]


# Kako je Srbija rangirana i kako su gradjani Sr zadovoljni zivotom ? 

WHR_data20[WHR_data20$Country == "Serbia",c(1,3) ]


# Srbija u odnosu na zemlje u okruzenju

WHR_data20[WHR_data20$Country %in%
            c('Serbia','Hungary','Romania','Bulgaria','Albania','Montenegro',
                'Bosnia and Herzegovina','Croatia','Slovenia'), c(1,3)]


# Ocekivani zivotni vek u Srbiji

WHR_data20[WHR_data20$Country == "Serbia",c(1,6) ]



# Najkorumiranije zemlje na osnovu ocene njihovih gradjana

Top_10<- WHR_data20 %>% select (Country,Corruption ) %>%
  arrange(desc(Corruption )) %>%
  head(n=10)

 ggplot(Top_10, aes(x = reorder(Country, Corruption), y = Corruption)) +
        geom_bar(stat = "identity",fill=rainbow(n=length(Top_10$Country)),width =0.3) + coord_flip()+ 
        labs(x = "Country", y = "Corruption_Score")
 

# Najmanje korumiranije zemlje na osnovu ocene njihovih gradjana

Top_10 <- WHR_data20 %>% 
           select (Country,Corruption ) %>%
            arrange(Corruption ) %>%
            head(n=10)

ggplot(Top_10, aes(x =reorder(Country, -Corruption), y=Corruption)) +
        geom_bar(stat = "identity",fill=rainbow(n=length(Top_10$Country)),width =0.3) + coord_flip()+ 
        labs(x = "Country", y = "Corruption_Score")

# Istrazivacka pitanja

# Da li postoji statistički značajna veza izmedju srece i 

# vrednosti GdP-a po glavi stanovnika (GDP per capita)

# podrske porodice i prijatelja (Social.support)

# ocekivanog trajanja zivota

# velikodusnosti

# prisustva korupcije u drustvu


# Odgovore na ova pitanja cemo dobiti primenom korelacije, a primenom metoda linerne regresije utvrdicemo koliko
# su ove varijable pogodne za predvinjanje zadovoljstva kvalitetom zivota

#Pre sprovodjenja ovih tehnika, potrebno je utvrditi da li varijable podlezu normalnoj distribuciji ili ne
#ukoloko je to slucaj koristimo Pearsonov koeficijent korelacije , u suprotnom Spirmanov koef.


#Pomocu Shapiro–Wilk testa utvrdjujemo da li varijabe podelezu normalnoj distribuciji ili ne
#Ho..podleze normalnoj distribuciji, ukoliko je p>0.01, prihvatamo H0, u sprotnom odbacujemo u korist H1
#H1..ne podleze......,

shapiro.test(WHR_data20$Happ_Score)
# podleze normalnoj distribuciji

shapiro.test(WHR_data20$GDP)
#ne podleze normalnoj distribuciji

shapiro.test(WHR_data20$Social.support)
#ne podleze normalnoj distribuciji

shapiro.test(WHR_data20$Life_exp)
#ne podleze normalnoj distribuciji

shapiro.test(WHR_data20$Generosity)
#ne podleze normalnoj distribuciji

shapiro.test(WHR_data20$Corruption)
#ne podleze normalnoj distribuciji

# Samo jedna varijabla podleze normalnoj distribuciji (ocena srece), a sve ostale odstupaju od normalne raspodele
#koristimo Spirmanov koef. korelacije.

# Korelacija
library("Hmisc")
WHR_corr <- data.frame(WHR_data20$Happ_Score,WHR_data20$GDP_perCapita,
                       WHR_data20$Social.support,WHR_data20$Life_exp,WHR_data20$Generosity,WHR_data20$Corruption)

rcorr(as.matrix(WHR_corr), type = c("spearman"))

#REZEULTATI ANALIZE

# 1) Da li postoji statisticki znacajna veza izmedju vrednosti GDP per capita i srece (kvaliteta zivota)?

#Da,postoji pozitivna i jaka veza izmedju ove dve varijable, koef.korelacijije je 0.79, (r=0.79),
#p=0,sto je  < 0,01,odnosno veza je statisticki znacajna
#Sa rastom GDP per capita, raste i zadovoljstvo zivotom, odnosno nivo srece i obrnuto


qplot(data=WHR_data20, x=GDP_perCapita,y=Happ_Score,colour=Regional.indicator,size=I(4))+ 
      labs(title ='GDP per capita vs Happiness', x='GDP per capita', y='Happiness score')

#na grafiku mozemo uociti da zemlje Zapadne Evrope imaju najvece vrednosti za GDP I medju najsrecnijima su 
#*zemlje Zapadne Evrope (*WEOG(Western European and Other States Group) prema klasifikaciji UN
# u ovu grupu ubraju se i zemlje Severne Evrope kao i odredjene zemlje Okeanije(Australija i Nz ) i Zapadne Azije (Turska i Izrael))


 # 2) Da li postoji statisticki znacajna veza izmedju postojanja drustvene podrske i srece?

#Da,postoji pozitivna i jaka veza izmedju ove dve varijable, koef.korelacijije je 0.80, (r=0.80),
#p=0,sto je  < 0,01,odnosno veza je statisticki znacajna
#Sa rastom drustvene podrske, raste i zadovoljstvo zivotom, odnosno nivo srece i obrnuto

qplot(data=WHR_data20, x=Social.support,y=Happ_Score,colour=Regional.indicator,size=I(4))+ 
  labs(title ='Social.support vs Happiness', x='Social.support', y='Happiness score')


# 3) Da li postoji statisticki znacajna veza izmedju duzine ocekivanog zivotnog veka i srece?
#Da,postoji pozitivna i jaka veza izmedju ove dve varijable, koef.korelacijije je 0.78, (r=0.78),
#p=0,sto je  < 0,01,odnosno veza je statisticki znacajna

qplot(data=WHR_data20, x=Life_exp,y=Happ_Score,colour=Regional.indicator,size=I(4))+ 
  labs(title ='Life expectancy vs Happiness', x='Life expectancy', y='Happiness score')

#4)  Da li postoji statisticki znacajna veza izmedju velikodusnosti i srece?

##Postoji pozitivna i slaba veza izmedju ove dve varijable (r=0.07),
#(p=0.36, p> 0,01, sto znaci da ova veza statisticki nije znacajna!

# 5)  Da li postoji statisticki znacajna veza izmedju srece i prisustva korupcije u drustvu (prema percepciji gradjana)?

##Postoji negativna i slaba veza izmedju ove dve varijable (r=-0.28),n = 153,(p=0.005), p < 0,01
# Ako nivo korupcije raste, kvalitet zivota u zemlji se smanjuje i obrnuto

qplot(data=WHR_data20, x=Corruption,y=Happ_Score,colour=Regional.indicator,size=I(4))+ 
  labs(title ='Corruption vs Happiness', x='Corruption', y='Happiness score')


# Vizualizacija korelacije

colnames(WHR_corr)<-c('HappinessScore','GDPperC','Support','LifeExp','Generosity','Corruption')

WHR_corrplot <-cor(WHR_corr, method = "spearman")

library(ggcorrplot)

ggcorrplot(WHR_corrplot, method = "square",colors = c("#6D9EC1", "white", "#E46726"))



#Regresioni modeli:

# Prvi model

Model_1 <- lm(WHR_data20$Happ_Score ~ WHR_data20$GDP_perCapita + WHR_data20$Social.support + 
                   WHR_data20$Life_exp+WHR_data20$Corruption)
summary(Model_1)

# Model je prilicno dobar, objasnjava 72 % varijabiliteta
#r^2=0.72

# Drugi model

Model_2 <- lm(WHR_data20$Happ_Score ~ WHR_data20$GDP_perCapita + WHR_data20$Social.support + 
                WHR_data20$Life_exp) 

summary(Model_2)
 # Drugi model je takodje dobar objašnjava 69 % varjabiliteta zadovoljstva kvalitom života (uk. ocene sreće)
# r2= 0.69, p < 0.01


# Treci model


Model_3 <- lm(WHR_data20$Happ_Score ~ WHR_data20$GDP_perCapita) 
                
summary(Model_3)

# Cetvrti model

Model_4 <- lm(WHR_data20$Happ_Score ~ WHR_data20$Social.support) 

summary(Model_4)


# Peti model

Model_5 <- lm(WHR_data20$Happ_Score ~ WHR_data20$Life_exp) 

summary(Model_5)




