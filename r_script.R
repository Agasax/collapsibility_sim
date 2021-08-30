library(janitor)
library(tidyverse)
d <- expand.grid(y=0:1, tx=c('A','B'),sex=c('male', 'female'))
# Order treatment levels to get OR > 1
d$tx <- factor(d$tx, c('B','A'))
d$freq <- c(500, 500, 900, 100, 100, 900, 500, 500)
d <- as_tibble(d)
d
inv_logit  <- function(x) {1/(1 + exp(-x))

}
d_sim <- d %>% modelr::data_grid(tx,sex) %>%
  mutate(n=1000,
         linpred=-2.197225+(as.numeric(tx)-1)*log(9)+(as.numeric(sex)-1)*log(9)) %>%
  mutate(`1`=map2_dbl(n,linpred,~rbinom(1,.x,inv_logit_scaled(.y))),
         `0`=n-`1`)
as.numeric( d$tx)-1
d_sim %>% tabyl()
d_sim <- d %>% modelr::data_grid(tx,sex) %>%
  mutate(n=4000,
         linpred=-4+(as.numeric(tx)-1)*log(9)+(as.numeric(sex)-1)*log(9)) %>%
  mutate(`1`=round(n*inv_logit_scaled(linpred)),
         `0`=n-`1`) %>%
  pivot_longer(cols=c(`0`,`1`),names_to = "y",values_to = "freq",names_transform = list(y=as.integer)) %>%
  select(y,everything(),-linpred)
(coef(glm(y~tx*sex,weights = freq,data = d_sim,family = binomial)))
d <- d_sim
rbinom(4,1000,inv_logit(d_sim$linpred))
(125/(1-125))
(coef(glm(y~tx*sex,weights = freq,data = d,family = binomial)))

library(tidyverse)

d %>% group_by(sex) %>%
  summarise(n=sum(freq))

d %>% pivot_wider(names_from = y,values_from = freq) %>%
  mutate(risk=`1`/(`0`+`1`),
         odds=risk/(1-risk)) %>%
  select(-c(`0`,`1`,n)) %>%
  pivot_wider(names_from = tx,values_from = c("risk","odds") ) %>%
  mutate(rr=risk_A/risk_B,or=odds_A/odds_B) %>%
  inner_join(d %>% group_by(sex) %>%
               summarise(n=sum(freq))) %>%
  add_row(summarise(.,sex="all",rr=weighted.mean(rr,n)))

d%>% pivot_wider(names_from = y,values_from = freq) %>% group_by(tx) %>%
  summarise(sum_0=sum(`0`),sum_1=sum(`1`)) %>%
  mutate(risk=sum_1/(sum_0+sum_1),
         odds=risk/(1-risk)) %>%
  select(-c(sum_0,sum_1)) %>%
  pivot_wider(names_from = tx,values_from = c("risk","odds") ) %>%
  mutate(rr=risk_A/risk_B,or=odds_A/odds_B)


