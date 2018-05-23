col1 <- c("a", "c", "b", "a", "b",  "c", "a")
col2 <- c(2, 4, 3, 1, 2, 5, 7)
df <- data.frame(col1, col2)
df <- df %>% group_by(col1,col2) %>% arrange(col1,col2) %>% mutate(count = n())
