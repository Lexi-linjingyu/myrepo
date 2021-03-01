library(data.table)
list <- list.files(pattern = '^p')
data_list <- lapply(list,fread)
df_data <- data.frame(matrix(unlist(data_list),nrow = length(data_list),byrow = TRUE))
dfdata <- data.frame(t(df_data))
data <- dfdata[-1,]
plot(data$X790,ylim=c(0,100))

##merge weather data
