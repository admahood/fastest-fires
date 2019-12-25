libs <- c("tidyverse", "lme4", "lmerTest", "mblm", "lubridate",
          "broom", "nlme","ggpubr", "gridExtra")
iini <-function(x){
  #stands for install if not installed
  if (!x %in% rownames(installed.packages())) install.packages(x)
}
lapply(libs, iini)
lapply(libs, library, character.only = TRUE)

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# read in daily data -----------------------------------------------------------
d <- read_csv("data/daily_stats_02-18_asof_20180612.csv") %>% 
  mutate(duration = duration,
         year = as.numeric(substr(date,1,4))) %>%
  group_by(id) %>%
  summarise(duration = getmode(duration),
            year = getmode(year),
            ecoregion = getmode(l1_ecoregion),
            lc = getmode(lc_name)) %>%
  ungroup()

dd <- d %>%
  dplyr::select(year, ecoregion, duration) %>%
  group_by(year, ecoregion) %>%
  summarise(duration_mean = mean(duration),
            duration_sd = sd(duration)) %>%
  ungroup()%>%
  nest(-ecoregion) %>%
  mutate(fit = map(data, ~ mblm(duration_mean ~ year, 
                                data=., repeated=F)),
         results = map(fit, augment)) %>%
  unnest(results)

dd_lm <- d %>%
  dplyr::select(year, ecoregion, duration) %>%
  nest(-ecoregion) %>%
  mutate(fit = map(data, ~ lm(duration ~ year,
                               data = .)),
         results = map(fit, augment)) %>%
  unnest(results)

write_csv(dd,"data/yearly_duration_eco.csv")

dd_lc <- d %>%
  dplyr::select(year, lc, duration) %>%
  group_by(year, lc) %>%
  summarise(duration_mean = mean(duration),
            duration_sd = sd(duration)) %>%
  ungroup()%>%
  filter(lc != "Barren" & 
           lc != "Permanent Snow and Ice" &
           lc != "Water Bodies"&
           lc != "Permanent Wetlands") %>%
  nest(-lc) %>%
  mutate(fit = map(data, ~ mblm(duration_mean ~ year, data=., repeated=F)),
         results = map(fit, augment)) %>%
  unnest(results)

dd_lc_lme <- d %>%
  dplyr::select(year, lc, duration) %>%
  nest(-lc) %>%
  mutate(fit = map(data, ~ lm(duration ~ year,
                              data = .)),
         results = map(fit, augment)) %>%
  unnest(results) %>%
  filter(lc != "Barren" & 
           lc != "Permanent Snow and Ice" &
           lc != "Water Bodies" &
           lc != "Permanent Wetlands") 

write_csv(dd_lc, "data/yearly_duration_lc.csv")

# model it properly - ecoregion ------------------------------------------------
dd$mod3 <- dd_lme$.fitted

lmer(duration ~ year + (year|ecoregion), d) -> mod3
dd$mod3 = predict(mod3, newdata = dd)

lm(duration ~ year*ecoregion, d) -> mod3
dd$mod3 = predict(mod3, newdata = dd)


ts_list <- list()
table_ <- tibble(ecoregion = NA, slope = NA, p = NA, min_duration= NA, max_duration = NA)
for(i in 1:length(unique(d$ecoregion))){
  print(i)
  ddd <- filter(dd, ecoregion == unique(d$ecoregion)[i])
  ts_list[[i]] <- mblm(duration_mean ~ year, ddd, repeated =F)
  summary(ts_list[[i]]) -> ss
  
  table_[i,1] <- unique(d$ecoregion)[i]
  table_[i,2] <- round(ts_list[[i]]$coefficients[2] %>% as.numeric,3)
  table_[i,3] <- round(ss$coefficients[2,4],5)
  table_[i,4] <- min(ddd$duration_mean)
  table_[i,5] <- max(ddd$duration_mean)
}
write_csv(table_, "images/ts_table.csv")

pp <- ggplot(dd, aes(x=year, y=duration_mean))+
  geom_point() +
  geom_line(aes(y=.fitted))+
  geom_line(aes(y=.fitted + (.se.fit*1.96)), lty=2)+
  geom_line(aes(y=.fitted - (.se.fit*1.96)), lty=2)+
  geom_line(data = dd_lme, aes(y = .fitted, color = ecoregion))+
  geom_line(data = dd_lme, aes(y = .fitted + (.se.fit*1.96), color = ecoregion), lty=3)+
  geom_line(data = dd_lme, aes(y = .fitted - (.se.fit*1.96), color = ecoregion), lty=3)+
  facet_wrap(~ecoregion, scales = "free")+
  geom_label(data = table_,
             aes(x=2002, y=max_duration, group = ecoregion, label = paste("p =",round(p,3))),
             hjust = "left", alpha = 0.5, label.size = 0.25)+
  theme_pubr() +
  theme(legend.position = "none")+
  ggsave("images/ts_lmm.png", width=12, height = 9)


ggplot(d, aes(x = year, y=duration, color = ecoregion)) +
  #geom_point() +
  #geom_smooth()
  geom_line(aes(y=predict(mod2)))

# by landcover --------------------------------------------------------
d<-d %>% filter(lc != "Barren" & 
                    lc != "Permanent Snow and Ice" &
                    lc != "Water Bodies"&
                  lc != "Permanent Wetlands")

lmer(duration ~ year + (year|lc), d) -> mod3
dd_lc$mod3 = predict(mod3, newdata = dd_lc)

lm(duration ~ year*lc, d ) -> mod3
dd_lc$mod3 = predict(mod3, newdata = dd_lc)


ts_list <- list()
table_ <- tibble(lc = NA, slope = NA, p = NA, min_duration=NA, max_duration = NA)
for(i in 1:(length(unique(d$lc)))){
  print(i)
  ddd <- filter(dd_lc, lc == unique(d$lc)[i])
  ts_list[[i]] <- mblm(duration_mean ~ year, ddd, repeated =F)
  summary(ts_list[[i]]) -> ss
  
  table_[i,1] <- unique(d$lc)[i]
  table_[i,2] <- round(ts_list[[i]]$coefficients[2] %>% as.numeric,3)
  table_[i,3] <- round(ss$coefficients[2,4],5)
  table_[i,4] <- min(ddd$duration_mean)
  table_[i,5] <- max(ddd$duration_mean)
}


write_csv(table_, "images/ts_lc_table.csv")



pp <- ggplot(dd_lc, aes(x=year, y=duration_mean))+
  geom_point() +
  geom_line(aes(y=.fitted))+
  geom_line(aes(y=.fitted + (.se.fit*1.96)), lty=2)+
  geom_line(aes(y=.fitted - (.se.fit*1.96)), lty=2)+
  geom_line(data = dd_lc_lme, aes(y = .fitted, color = lc))+
  geom_line(data = dd_lc_lme, aes(y = .fitted + (.se.fit*1.96), color = lc), lty=3)+
  geom_line(data = dd_lc_lme, aes(y = .fitted - (.se.fit*1.96), color = lc), lty=3)+
  facet_wrap(~lc, scales = "free")+
  geom_label(data = table_,
             aes(x=2002, y=max_duration, group = lc, label = paste("p =",round(p,3))),
             hjust = "left", alpha = 0.5, label.size = 0.25)+
  theme_pubr() +
  theme(legend.position = "none")+
  ggsave("images/ts_lm_lc.png", width=10, height = 10);pp

# gmod<-gls(duration ~ year, d[d$lc == "Barren",], correlation = corAR1(form = ~1|year))
# 
# ggplot(d[d$lc == "Barren",], aes(x = year, y=duration)) +
#   geom_line(aes(y=predict(gmod)))
