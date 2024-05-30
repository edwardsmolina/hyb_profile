df <- tibble::tribble(
  ~date, ~score, ~weight,
  2017,    2,     5,
  2017,    6,     5,
  2018,    2,     10,
  2018,    6,     1,
  2019,    2,     1,
  2019,    6,     10
)

df %>% 
  group_by(date) %>% 
  summarise(w=weighted.mean(score, weight), 
            mean_score=mean(score))

# 2017 - media normal (mismos pesos)
# 2018 - media con mas peso hacia 2
# 2019 - media con mas peso hacia 6

