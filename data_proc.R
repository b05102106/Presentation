library(tidyverse)
library(fs)

f_path <- "D://Desktop//Stat_final"

# files <- list.files(pattern = "\\.csv$", full.names = T)

# dir_create(f_path)

setwd(f_path)

# for (i in files){
#   file_copy(i, f_path)
# }


# for (f_i in seq_along(files)){
#   to <- str_replace(files[f_i], "(\\w+)_Identification Task_2019_Jun_\\w+_\\w+", "Participant_\\1")
#   file.rename(files[f_i], to)
# }


files <- list.files(f_path, pattern = "\\.csv$") %>% 
  str_sort(numeric = T)

data <- tibble()

pretest <- tibble()

for(i in files){
  read <- read_csv(i) 
  
  test <- read %>% 
    select(participant, sound = Sound, resp = key_resp_4.keys, rt = key_resp_4.rt) %>% 
    filter(!is.na(rt)) %>% 
    mutate(sound = str_replace(sound, "sound/(\\w+)\\.wav", "\\1Hz")) %>% 
    arrange(sound)
  
  pret <- read %>% 
    select(participant, sound = Sound, resp = trial1__key_resps.keys) %>% 
    filter(!is.na(resp)) %>% 
    mutate(sound = str_replace(sound, "sound/(\\w+)\\.wav", "\\1Hz"))
  
  data <- rbind(data, test)
  pretest <- rbind(pretest, pret)
}



data %>% 
  group_by(sound, resp) %>% 
  summarize(count = n(), percent = n() / 40) %>% 
  select(-count) %>% 
  spread(resp, percent) %>% 
  mutate(n = abs(1 - g)) %>% 
  ggplot()+
  geom_line(aes(x = sound, y = n, color = "/sa/", group = 1), size = 1)+
  geom_line(aes(x = sound, y = g, color = "/ʂa/", group = 1), size = 1)+
  xlab("Hz")+
  ylab("Percentage") +
  ggtitle("/ʂa/-/sa/ Identification") + 
  theme(plot.title = element_text(hjust = 0.5, size = 14),
        axis.text.x = element_text(angle = 90, vjust = 0.4)) +
  scale_color_discrete(name="") 


data %>% 
  group_by(sound, resp) %>% 
  summarize(count = n(), percent = n() / 40) %>% 
  select(-count) %>% 
  spread(resp, percent) %>% 
  mutate(n = abs(1 - g)) %>% 
  ggplot()+
  geom_point(aes(x = sound, y = n),
             size = 2)+
  geom_smooth(mapping = aes(x = sound, y = n, group = 1), 
              size = 1, 
              method = "lm")+
  geom_point(mapping = aes(x = sound, y = g),
             shape = 18,
             size = 2.5)+
  geom_smooth(aes(x = sound, y = g, group = 1), 
              size = 1,
              method = "lm")+
  xlab("Hz")+
  ylab("Percentage") +
  ylim(0, 1) +
  ggtitle("/ʂa/-/sa/ Identification") + 
  theme(plot.title = element_text(hjust = 0.5, size = 14),
        axis.text.x = element_text(angle = 90, vjust = 0.4)) +
  scale_color_discrete(name="") 


corr <- vector("numeric", length = nrow(pretest))

for ( i in 1:nrow(pretest)){ 
  if (pretest[i,][["sound"]] == "1500Hz" & pretest[i,][["resp"]] == "g"){
    corr[i] <- 1
  } else if (pretest[i,][["sound"]] == "5000Hz" & pretest[i,][["resp"]] == "n"){
    corr[i] <- 1
  } else {
    corr[i] <- 0
  }
}

pretest <- pretest %>% 
  mutate(corr = corr)

pretest %>% 
  select(sound, corr) %>% 
  group_by(sound, corr) %>% 
  summarise(percentage = n()/120) %>% 
  filter(corr == 1) %>%
  ggplot()+
  geom_bar(mapping = aes(x = sound, y = percentage), 
           stat = "identity") +
  geom_text(mapping = aes(x = sound, y = percentage, label = round(percentage, 2)))


data %>% 
  select(-resp) %>% 
  group_by(participant) %>% 
  ggplot() + 
  geom_line(aes(x = sound, y = rt, group = 1)) + 
  theme(plot.title = element_text(hjust = 0.5, size = 14),
        axis.text.x = element_text(angle = 90, vjust = 0.4))+
  facet_wrap(~participant)
  
data %>% 
  select(-resp) %>% 
  spread(participant, rt) %>% 
  mutate(mean = rowMeans(.[,2:length(.)])) %>% 
  select(sound, mean) %>% 
  ggplot(mapping = aes(x = sound, y = mean, group = 1)) + 
  geom_point()+
  geom_line()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.4))+
  xlab("Reaction Time(s)")+
  ylab("Sound")+
  ggtitle("Mean Reaction Time")+
  theme(plot.title = element_text(hjust = 0.5, size = 14))
  geom_line(data = movavg(mov$mean, 3, type = "s"))

t_t <- data %>% 
  select(-resp) %>% 
  spread(participant, rt) 

a <- t_t %>% 
  filter(sound == "3600Hz") %>% 
  gather(sound, rt) %>% 
  rename("participant" = sound) %>% 
  .[["rt"]] %>%
  as.vector() 
b <- t_t %>% 
  filter(sound == "3400Hz") %>% 
  gather(sound, rt) %>% 
  rename("participant" = sound) %>% 
  .[["rt"]] %>%
  as.vector() 
data %>% 
  select(-resp) %>% 
  spread(participant, rt) %>% 
  mutate(mean = rowMeans(.[,2:length(.)])) %>% 
  select(sound, mean) %>% 
  mutate(mova = movavg(.$mean, 3, type = "s"))%>% 
  ggplot() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.4),
        plot.title = element_text(hjust = 0.5, size = 14)) +
  geom_line(aes(x = sound, y = mova, group = 1)) +
  ggtitle("Smoothed Mean Reaction Time") +
  xlab("Smoothed Reaction Time(s)") +
  ylab("Sound")


 
mov %>% ggplot() + 
  #geom_point(mapping = aes(x = sound, y = mean, group = 1))+
  geom_line(mapping = aes(x = sound, y = mean, group = 1))+
  #geom_smooth(mapping = aes(x = sound, y = mean, group = 1)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.4)) +
  geom_line(aes(x = sound, y = mova, group = 1))


geom_point() +
  geom_smooth(method = "lm")

  
gather(key, value, g, n) %>% 
  

rename("/ʂa/" = g, "/sa/" = n)
View(n)  
  

test <- read_csv("Participant_32.csv") %>% 
  select(participant, sound = Sound, resp = key_resp_4.keys, rt = key_resp_4.rt)



t2 <- test %>% 
  filter(!is.na(rt)) %>% 
  mutate_at(vars(sound), 
            function(x){
              str_replace(x, 
                          pattern = "sound/(\\w+)\\.wav", 
                          replacement = "\\1HZ")}) 

t2 %>% 
  arrange(sound) %>% 
  filter(rt > boxplot(t2$rt)[["stats"]][5,])



boxplot(t2$rt)












