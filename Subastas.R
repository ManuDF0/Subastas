library(tidyverse)

datos <- read.csv('FPAData_2024.csv')

datos <- datos %>%
  select(-X) %>%
  rename(auction = Auction..) %>%
  pivot_longer(cols = starts_with('Bidder.'),
               names_to = 'bidder',
               values_to = 'bid') %>%
  mutate(bidder = case_when(bidder == "Bidder.1" ~ 1,
                            bidder == "Bidder.2" ~ 2,
                            bidder == "Bidder.3" ~ 3))

G <- ecdf(datos$bid)
datos %>% 
  ggplot(aes(x = bid)) +
  stat_ecdf(geom = 'line') +
  labs(x = 'Bid', y = 'Probabilidad acumulada') + 
  theme_minimal()

datos <- datos %>% 
  mutate(G = G(bid))

# Calculo el bandwidth optimo para epanechnikov
h_optimo <- sd(datos$bid)*2.34*nrow(datos)^(-1/5)

densidad <- density(datos$bid, bw = h_optimo, kernel = 'epanechnikov')
datos %>% 
  ggplot(aes(x = bid)) +
  geom_density(bw = h_optimo, kernel = 'epanechnikov') +
  labs(x = 'Bid', y = 'Densidad') +
  theme_minimal()

# density me devuelve una lista de length 512, uso las densidades estimadas para aproximar las densidades para todo my df 
densidad <- approx(densidad$x, densidad$y, xout = datos$bid)

datos <- datos %>% 
  mutate(g = densidad[["y"]])

# Calculo los límites del soporte
b_low <- round(min(datos$bid), digits = 6)
b_high <- round(max(datos$bid), digits = 6)
soporte <- round(seq(b_low, b_high, by = 0.000001), digits = 6)

# Armo una dummy que toma valor 1 cuando hay que hacer el trim
datos <- datos %>%
  mutate(
    bid_plus = round(bid + 2 * h_optimo, digits = 6),
    bid_minus = round(bid - 2 * h_optimo, digits = 6),
    trim = if_else(bid_plus %in% soporte & bid_minus %in% soporte, 0, 1)
  )

# Armo la variable psi_hat
datos <- datos %>% 
  mutate(psi_hat = case_when(trim == 1 ~ Inf,
                             trim == 0 ~ G/g)) %>% 
  mutate(pseudo_val = bid + 1/2*psi_hat) %>% 
  filter(trim != 1)

h_gpv <- sd(datos$pseudo_val)*2.34*(nrow(datos)^(-1/5))

densidad_valuaciones <- density(datos$pseudo_val, bw = h_gpv, kernel = 'epanechnikov')

datos %>%
  ggplot() +
  stat_density(aes(x = pseudo_val, color = 'Valuación Estimada'), geom = 'line', bw = h_gpv, kernel = 'epanechnikov') +
  stat_density(aes(x = bid, color = 'Bid'), geom = 'line', bw = h_optimo, kernel = 'epanechnikov') +
  labs(x = 'Bid/Valuación', y = 'Densidad', color = '') +
  theme_minimal() +
  theme(legend.position = c(.75, .85), text = element_text(size = 17))

datos <- datos %>% 
  mutate(diferencia = pseudo_val - bid)

summary(datos$diferencia)


datos %>% 
  ggplot(aes(x = pseudo_val, y = bid)) +
  geom_point() +
  labs(y = 'Bid', x = 'Valuación Estimada') +
  theme_minimal()
