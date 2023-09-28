source("test_hFWI.r")
library(ggplot2)

r <- test_hfwi()
print(ggplot(r) +
    geom_line(aes(TIMESTAMP, FFMC)) +
    geom_point(aes(TIMESTAMP, DFFMC), data = r[hour(TIMESTAMP) == 17]))
print(ggplot(r) +
    geom_line(aes(TIMESTAMP, DMC)) +
    geom_point(aes(TIMESTAMP, DDMC), data = r[hour(TIMESTAMP) == 17]))
print(ggplot(r) +
    geom_line(aes(TIMESTAMP, DC)) +
    geom_point(aes(TIMESTAMP, DDC), data = r[hour(TIMESTAMP) == 17]))
