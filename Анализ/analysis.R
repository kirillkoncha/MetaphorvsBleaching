library(tidyverse)

# Чтение результатов
df <- read_csv("results.csv")

# Чтение частотностей каждой фразы
df2 <- read_csv("freq_for_data.csv")

# Чтение участников
participants <- 
  read_csv("participants.csv")

# Оставляем участников, у кого процент правильности в филлерах 85 и выше
participants <- participants[participants$Errors < 15, ]

#Выкидываем участников с не тем возрастом (см. пояснения в github папке)
participants <- participants[!(participants$Participant %in% c(36, 257, 313)), ]
participants <- c(participants$Participant)

#Склеиваем результаты и частотности
df <- merge(df, df2, by.df = "NP")

# Оставляем время реакции только в промежутке между 0 и 15 000 мс
# Это результаты для высокочастотных фраз
# Чтобы получить для низкочастотных, просто замените freq на < 500
df %>% 
  filter(RT < 15000,
         RT > 0,
         Correct >= 0,
         freq > 500) %>% 
  mutate(log_RT = log(RT),
         log_freq = log(freq)) ->
  filtered

# Отсеиваем результаты ненужных участников
filtered <- filtered[ filtered$Participant %in% participants, ]

# Разбиваем данные на 4 группы: буквальное в парах с метафорой,
# метафора, буквальное в парах с бличингом, бличинг
# Для всех групп выкидываем +- 2.5 средних отклонения по времени
literal_m <- filtered[ filtered$Type == 'literal_m', ]
literal_m <- 
  literal_m[literal_m$RT > (mean(literal_m$RT) - 2.5*sd(literal_m$RT)) & 
              literal_m$RT < (mean(literal_m$RT) + 2.5*sd(literal_m$RT)), ]

literal_b <- filtered[filtered$Type == 'literal_b', ]
literal_b <- 
  literal_b[literal_b$RT > (mean(literal_b$RT) - 2.5*sd(literal_b$RT)) & 
              literal_b$RT < (mean(literal_b$RT) + 2.5*sd(literal_b$RT)), ]

metaphor <- filtered[ filtered$Type == 'metaphor', ]
metaphor <- 
  metaphor[metaphor$RT > (mean(metaphor$RT) - 2.5*sd(metaphor$RT)) & 
             metaphor$RT < (mean(metaphor$RT) + 2.5*sd(metaphor$RT)), ]

bleaching <- filtered[ filtered$Type == 'bleaching', ]
bleaching <- 
  bleaching[bleaching$RT > (mean(bleaching$RT) - 2.5*sd(bleaching$RT)) & 
              bleaching$RT < (mean(bleaching$RT) + 2.5*sd(bleaching$RT)), ]

# Склеиваем обратно все 4 группы в одну таблицу
filtered <- rbind(literal_m, literal_b, metaphor, bleaching)

# Теперь внутри таблицы среди всех групп выкидываем +- 2.5 средних отклонения
filtered <- 
  filtered[filtered$RT > (mean(filtered$RT) - 2.5*sd(filtered$RT)) & 
             filtered$RT < (mean(filtered$RT) + 2.5*sd(filtered$RT)), ]

# Визуализация полученных данных
filtered %>% 
  ggplot(aes(freq, log_RT, color = Type))+
  geom_point()+
  facet_wrap(~Type, scales = "free")

library(lme4)
library(lmerTest)

# Пары буквальное значение и метафора
metaphor <- filtered
metaphor$Type[filtered$Type == 'literal_m'] <- 0
metaphor$Type[filtered$Type == 'metaphor'] <- 1

filter <- metaphor[metaphor$Type == 0 | metaphor$Type == 1, ]

# Модель для зависимости времени реакции от типа сдвига и частотности
filter %>% 
  filter(Type %in% c(0,1)) %>% 
  lmer(log_RT ~ Type + log_freq + (1|Participant) + (1|Item), data = .) ->
  m1_RT

summary(m1_RT)

# Модель для зависимости ответов об осмысленности от типа сдвига и частотности
filter %>% 
  filter(Type %in% c(0,1)) %>% 
  glmer(Correct ~ Type + log_freq + (1|Participant) + (1|Item), data = ., 
        family = binomial) ->
  m1_Correct

summary(m1_Correct)

library(ggeffects)

#Вывод с точностью до одного знака после запятой (для визуализации осмысленности)
scaleFUN <- function(x) sprintf("%.1f", x)

ggpredict(m1_RT, terms = c("log_freq", "Type")) %>% 
  plot()

ggpredict(m1_Correct, terms = c("log_freq", "Type")) %>% 
  plot() +
  scale_y_continuous(labels=scaleFUN)


# Пары буквальное значение и бличинг
bleaching <- filtered
filtered$Type[filtered$Type == 'literal_b'] <- 0
filtered$Type[filtered$Type == 'bleaching'] <- 1

filter <- filtered[bleaching$Type == 0 | bleaching$Type == 1, ]

# Модель для зависимости времени реакции от типа сдвига и частотности
filtered %>% 
  filter(Type %in% c(0,1)) %>% 
  lmer(log_RT ~ Type + log_freq + (1|Participant) + (1|Item), data = .) ->
  m2_RT

summary(m2_RT)

# Модель для зависимости ответов об осмысленности от типа сдвига и частотности
filtered %>% 
  filter(Type %in% c(0,1)) %>% 
  glmer(Correct ~ Type + log_freq + (1|Participant) + (1|Item), data = ., 
        family = binomial) ->
  m2_Correct

summary(m2_Correct)

#Вывод с точностью до трех знаков после запятой (для визуализации осмысленности)
scaleFUN <- function(x) sprintf("%.3f", x)

ggpredict(m2_RT, terms = c("log_freq", "Type")) %>% 
  plot()

ggpredict(m2_Correct, terms = c("log_freq", "Type")) %>% 
  plot()+
  scale_y_continuous(labels=scaleFUN)
