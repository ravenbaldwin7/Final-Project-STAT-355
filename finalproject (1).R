data=read.csv("C:/Users/rabre/Downloads/songsforanalysis.csv")#read in song data
#from csv
summary(data)#summary statistics of song
attach(data)#attaching data

data$duration_ms=data$duration_ms*.001#conversion to seconds

#histograms of the variables
hist(duration_ms)
hist(speechiness)
hist(tempo)
hist(danceability)
hist(instrumentalness)
hist(valence)
hist(energy)
hist(speechiness)
hist(acousticness)
plot(data)

#normality check for all variables
qqnorm(speechiness, main="speechiness")
qqnorm(energy,main="energy")
qqnorm(acousticness,main="acousticness")
qqnorm(instrumentalness,main="instrumentalness")
qqnorm(valence, main = "valence")
qqnorm(tempo, main="tempo")
qqnorm(duration_ms, main = "duration")
qqnorm(loudness, main= "loudness")



#relationship between danceability, energy, and valence

mod1=lm(danceability~energy+valence)#regression model without interaction
mod2=lm(danceability~energy*valence)#regression model with interaction

#checking assumptions for the models
plot(mod1)
plot(mod2)

#regression model summaries
summary(mod1)$coef
summary(mod1)$coef

#partial f tests
anova(mod1, mod2)

#checking r squared
summary(mod1)$r.squared
summary(mod2)$r.squared



#checking relationship between speechiness and instrumenatalness

mod3=lm(speechiness~instrumentalness)#regression model

#checking assumptions
plot(mod3)

#regression model summary
summary(mod3)$coef

#plotting data with regression line
plot(speechiness~instrumentalness)
abline(mod3)

#checking correlation
cor.test(speechiness, instrumentalness)

#ttest
t.test(speechiness,instrumentalness)

#checking r squared
summary(mod3)$r.squared

#partial f test
anova(mod3)



#checking relationship between loudness and acousticness

mod4=lm(loudness~acousticness)#regression model

#checking assumtions
plot(mod4)

#regression model summary
summary(mod4)$coef

#plotting data with regression line
plot(loudness~acousticness)
abline(mod4)

#checking correlation
cor.test(loudness, acousticness)

#ttest
t.test(loudness, acousticness)

#checkinhg r squared
summary(mod4)$r.squared

#partial f test
anova(mod4)

#thats all :)

