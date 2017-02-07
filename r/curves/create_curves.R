
library(Faraday.Pricing)
library(ggplot2)
library(reshape2)

qry <- "SELECT [attachment]
      ,[value]
,[type]
,[updated]
,[audit]
FROM [fire_engine].[dbo].[fisrt_loss_curve_new]
ORDER BY attachment"


df <- get_data_from_sql_server(qry, server = "GBLONTDV56\\PRICING,57066",
                               database = "fire_engine")



p <- ggplot(df, aes(attachment, value)) +
  geom_line(aes(color = type))
p



f <- function(curve, x){
  return(approx(x = curve$attachment, y = curve$value, xout = x)$y)
}


curves <- lapply(unique(df$type), function(t){
  df %>%
    filter(type == t)
})

#########################################################################
#
# Test curve differences
#
#
#########################################################################
for( c in curves){

  # sort(c)
  #take every 100th point
  c1 <- c[c(1, seq(1, nrow(c), 20),nrow(c)),]
  c1 <- distinct(c1)

  y_approx <- f(c1, c$attachment)

  df <- data.frame(x = c$attachment,
                   y = c$value,
                   pred = y_approx) %>%
    mutate(diff = abs(y- pred))

  tt <- df %>%
    filter(is.na(pred))

  diff <- max(abs(y_approx - c$value))

  print(diff)

}


# max difference of 6.259258e-06 - can live with this


#### write curves to file


output <- lapply(curves, function(c){
  c1 <- c[c(1, seq(1, nrow(c), 20),nrow(c)),]
  c1 <- distinct(c1)

  c1
}) %>%
  bind_rows()

print_output <- output %>%
  dcast(attachment ~ type, value.var = "value")


write.csv(print_output, "../../data/curves.csv")
