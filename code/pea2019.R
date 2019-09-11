library(dplyr)

pea <- read.csv('./data/nflowersvisits_pea2019.csv')

pea$block <- as.factor(pea$block)

#take out date without visitation information

pea <- dplyr::filter(pea, date != '8/27/2019')
pea$date <- pea$date[drop=T]
summary(pea)

fertility <- read.csv('./data/pea_fertility.csv') %>%
             dplyr::mutate(block = as.factor(block))

pea <- dplyr::full_join(pea, fertility) #%>%
       #dplyr::mutate(leaf_greenness = as.factor(leaf_greenness),
       #              stem_redness = as.factor(stem_redness))

plot(leaf_greenness~nflowers, data=pea)
plot(stem_redness~nflowers, data=pea)

#what determines the number of flowers a pea plant has?
library(emmeans); library(lme4)
flowers <- glmer.nb(nflowers~ treatment + (1|date) + (1|observer) +
                    stem_redness, 
                    data=pea,
                    control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
summary(flowers)
AIC(flowers)

emm2 <- emmeans(flowers, specs = pairwise ~ treatment) #, type='response')
emm2

plot(emm2)

library(Hmisc)

#number of plant visits and flower visits is strongly correlated (yay!)
plot(pea$Bombus_nflowervisits~pea$Bombus_nplantvisits, pch=20, cex=1.5)
rcorr(pea$Bombus_nflowervisits,pea$Bombus_nplantvisits)

mod <- lm(pea$Bombus_nflowervisits~pea$Bombus_nplantvisits)
summary(mod)

library(lme4); library(AICcmodavg)
hist(pea$Bombus_nplantvisits, breaks=30)

#pea <- dplyr::filter(pea, nflowers > 5)
pois <- glmer(Bombus_nplantvisits~ treatment + nflowers + (1|date) + (1|observer)
              + (1|block), 
              data=pea, family=poisson,
              control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

nb0 <- glmer.nb(Bombus_nplantvisits~ treatment + nflowers + (1|date),
               data=pea,
               control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

nb <- glmer.nb(Bombus_nplantvisits~ treatment*nflowers + date +
               (1|block) + (1|observer), 
               data=pea,
               control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
nb1 <- glmer.nb(Bombus_nplantvisits~ treatment + nflowers + date +
                 (1|block) + (1|observer), 
               data=pea,
               control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

AIC(nb0, nb, nb1)
summary(nb1)

highflower <- dplyr::filter(pea, treatment %in% c('mix high', 'fert low', 'fert high'))
lowflower <- dplyr::filter(pea, !treatment %in% c('mix high', 'fert low', 'fert high'))

hf_nb <- glmer.nb(Bombus_nplantvisits~ treatment+ nflowers + date +
                 (1|block) + (1|observer), 
               data=highflower,
               control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

lf_nb <- glmer.nb(Bombus_nplantvisits~ treatment+ nflowers + date +
                    (1|block) + (1|observer), 
                  data=lowflower,
                  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

summary(lf_nb)
emm <- emmeans(hf_nb, specs = pairwise ~ treatment) #, type='response')
emm; plot(emm)

summary(nb1)
#try number of flower visits instead of plant
nb_flower <- glmer.nb(Bombus_nflowervisits~ treatment + nflowers + (1|obv_order)
               + (1|block), 
               data=pea,
               control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

AIC(pois, nb)
summary(pois)

summary(nb)


library(emmeans)
emm1 <- emmeans(nb, specs = pairwise ~ treatment) #, type='response')
emm1

plot(emm1)

emm0 <- emmeans(nb0, specs = pairwise ~ date) #, type='response')
emm0

plot(emm0)


flowers <- glmer.nb(nflowers~ treatment + (1|observer), 
                data=pea,
                control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

emm2 <- emmeans(flowers, specs = pairwise ~ treatment) #, type='response')
emm2

plot(emm2)
