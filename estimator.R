remove_outliers <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}

library(tidyverse)

# filename = "result.csv"

do.estimation <- function(filename) {
  d.src = read.csv(filename, sep = ";", encoding = "UTF-8", header = F)
  names(d.src) <- c("session","time", "name","E","size","n","r", "ln","iter","citer","duration","mark","bank")
  
  d.src$time <- as.numeric(d.src$time)
  d.src$session <- as.character(d.src$session)
  E.names <- c("1" = "Первая попытка", "2" = "Вторая попытка", "3" = "Третья попытка")
  
  for(x in unique(d.src$session)) {
    d <- d.src[d.src$session==x,]
    
    d <- rbind(d[1,], d)
    d$n[1] <- d$n[1]-1
    d$iter[1] <- d$iter[1]-1
    d$citer[1] <- d$citer[1]-1
    d$mark[1:(nrow(d)-1)] <- d$mark[2:(nrow(d))]
    d$duration[1:(nrow(d)-1)] <- d$duration[2:(nrow(d))]
    
    d$tick <- c(NA,diff(d$time))
    
    d$tick[1:(nrow(d)-1)] <- d$tick[2:(nrow(d))]
    d <- d[-c(nrow(d)),]
    
    d$tick[which((d$E == 1 | d$E == 2 | d$E == 3) & d$iter==0)] <- 0
    
    d$time     <- as.integer(d$time/1000)
    d$datetime <- as.POSIXct(d$time, origin="1970-01-01")
    d$datetime <- format(d$datetime, format="%H:%M:%S")
    result <- data.frame(session = d$session[1], name = d$name[1])
    
    ##########################################################################################
    # Интервал между касаниями
    ggplot(d, aes(x=factor(size), y=remove_outliers(tick))) +
      geom_violin(scale="width") +
      geom_smooth(method = "gam", se=TRUE, aes(group=1)) +
      geom_boxplot(width=.12, fill=I("black"), notch=T, outlier.size=NA, col="grey40") +
      stat_summary(fun.y="median", geom="point", shape=20, col="red", size = 10) +
      ggtitle("Результаты BART") +
      labs(x="Вид шара", y="Интервал между касаниями, мс") +
      facet_grid(E~name, labeller = labeller(E = E.names)) +
      scale_x_discrete(labels = c("Красный шар","Желтый шар","Зеленый шар")) +
      theme(legend.position="none",
            axis.text=element_text(size=12),
            axis.title=element_text(size=16,face="bold")) +
      scale_fill_brewer() + geom_jitter(alpha = 0.8) +
      theme(plot.title = element_text(size=16, face='bold', hjust=0.5),
            axis.text.x  = element_text(angle=0, vjust=0, size=16),
            axis.ticks.x = element_blank(),
            axis.title.y = element_text(face="bold", size=20),
            axis.text.y  = element_text(angle=90, vjust=0, size=16))
    
    sprintf("%.2f±%.2f", mean(d$tick[d$E == 1 & d$size == 8], na.rm = T), sd(d$tick[d$E == 1 & d$size == 8], na.rm = T))
    sprintf("%.2f±%.2f", mean(d$tick[d$E == 2 & d$size == 8], na.rm = T), sd(d$tick[d$E == 2 & d$size == 8], na.rm = T))
    sprintf("%.2f±%.2f", mean(d$tick[d$E == 3 & d$size == 8], na.rm = T), sd(d$tick[d$E == 3 & d$size == 8], na.rm = T))
    
    sprintf("%.2f±%.2f", mean(d$tick[d$E == 1 & d$size == 32], na.rm = T), sd(d$tick[d$E == 1 & d$size == 32], na.rm = T))
    sprintf("%.2f±%.2f", mean(d$tick[d$E == 2 & d$size == 32], na.rm = T), sd(d$tick[d$E == 2 & d$size == 32], na.rm = T))
    sprintf("%.2f±%.2f", mean(d$tick[d$E == 3 & d$size == 32], na.rm = T), sd(d$tick[d$E == 3 & d$size == 32], na.rm = T))
    
    sprintf("%.2f±%.2f", mean(d$tick[d$E == 1 & d$size == 128], na.rm = T), sd(d$tick[d$E == 1 & d$size == 128], na.rm = T))
    sprintf("%.2f±%.2f", mean(d$tick[d$E == 2 & d$size == 128], na.rm = T), sd(d$tick[d$E == 2 & d$size == 128], na.rm = T))
    sprintf("%.2f±%.2f", mean(d$tick[d$E == 3 & d$size == 128], na.rm = T), sd(d$tick[d$E == 3 & d$size == 128], na.rm = T))
    
    
    result$interval.H1 <- mean(d$tick[d$E == 1 & d$size == 8])
    result$interval.H2 <- mean(d$tick[d$E == 2 & d$size == 8])
    result$interval.H3 <- mean(d$tick[d$E == 3 & d$size == 8])
    
    result$interval.M1 <- mean(d$tick[d$E == 1 & d$size == 32])
    result$interval.M2 <- mean(d$tick[d$E == 2 & d$size == 32])
    result$interval.M3 <- mean(d$tick[d$E == 3 & d$size == 32])
    
    result$interval.L1 <- mean(d$tick[d$E == 1 & d$size == 128])
    result$interval.L2 <- mean(d$tick[d$E == 2 & d$size == 128])
    result$interval.L3 <- mean(d$tick[d$E == 3 & d$size == 128])
    
    # scaled time
    result$interval.std.H1 <- mean(scale(d$tick[d$E == 1 & d$size == 8]))
    result$interval.std.H2 <- mean(scale(d$tick[d$E == 2 & d$size == 8]))
    result$interval.std.H3 <- mean(scale(d$tick[d$E == 3 & d$size == 8]))
    
    result$interval.std.M1 <- mean(scale(d$tick[d$E == 1 & d$size == 32]))
    result$interval.std.M2 <- mean(scale(d$tick[d$E == 2 & d$size == 32]))
    result$interval.std.M3 <- mean(scale(d$tick[d$E == 3 & d$size == 32]))
    
    result$interval.std.L1 <- mean(scale(d$tick[d$E == 1 & d$size == 128]))
    result$interval.std.L2 <- mean(scale(d$tick[d$E == 2 & d$size == 128]))
    result$interval.std.L3 <- mean(scale(d$tick[d$E == 3 & d$size == 128]))
    
    ##########################################################################################
    # Продолжительность касания
    
    ggplot(d, aes(x=factor(size), y=remove_outliers(duration))) +
      geom_violin(scale="width") +
      geom_smooth(method = "gam", se=TRUE, aes(group=1)) +
      geom_boxplot(width=.12, fill=I("black"), notch=T, outlier.size=NA, col="grey40") +
      stat_summary(fun.y="median", geom="point", shape=20, col="red", size = 10) +
      ggtitle("Результаты BART") +
      labs(x="Вид шара", y="Продолжительность касания, мс") +
      facet_grid(E~name, labeller = labeller(E = E.names)) +
      scale_x_discrete(labels = c("Красный шар","Желтый шар","Зеленый шар")) +
      theme(legend.position="none",
            axis.text=element_text(size=12),
            axis.title=element_text(size=16,face="bold")) +
      scale_fill_brewer() + geom_jitter(alpha = 0.8) +
      theme(plot.title = element_text(size=16, face='bold', hjust=0.5),
            axis.text.x  = element_text(angle=0, vjust=0, size=16),
            axis.ticks.x = element_blank(),
            axis.title.y = element_text(face="bold", size=20),
            axis.text.y  = element_text(angle=90, vjust=0, size=16))
    
    
    sprintf("%.2f±%.2f", mean(d$duration[d$E == 1 & d$size == 8], na.rm = T), sd(d$duration[d$E == 1 & d$size == 8], na.rm = T))
    sprintf("%.2f±%.2f", mean(d$duration[d$E == 2 & d$size == 8], na.rm = T), sd(d$duration[d$E == 2 & d$size == 8], na.rm = T))
    sprintf("%.2f±%.2f", mean(d$duration[d$E == 3 & d$size == 8], na.rm = T), sd(d$duration[d$E == 3 & d$size == 8], na.rm = T))
    
    sprintf("%.2f±%.2f", mean(d$duration[d$E == 1 & d$size == 32], na.rm = T), sd(d$duration[d$E == 1 & d$size == 32], na.rm = T))
    sprintf("%.2f±%.2f", mean(d$duration[d$E == 2 & d$size == 32], na.rm = T), sd(d$duration[d$E == 2 & d$size == 32], na.rm = T))
    sprintf("%.2f±%.2f", mean(d$duration[d$E == 3 & d$size == 32], na.rm = T), sd(d$duration[d$E == 3 & d$size == 32], na.rm = T))
    
    sprintf("%.2f±%.2f", mean(d$duration[d$E == 1 & d$size == 128], na.rm = T), sd(d$duration[d$E == 1 & d$size == 128], na.rm = T))
    sprintf("%.2f±%.2f", mean(d$duration[d$E == 2 & d$size == 128], na.rm = T), sd(d$duration[d$E == 2 & d$size == 128], na.rm = T))
    sprintf("%.2f±%.2f", mean(d$duration[d$E == 3 & d$size == 128], na.rm = T), sd(d$duration[d$E == 3 & d$size == 128], na.rm = T))
    
    
    result$duration.H1 <- mean(d$duration[d$E == 1 & d$size == 8])
    result$duration.H2 <- mean(d$duration[d$E == 2 & d$size == 8])
    result$duration.H3 <- mean(d$duration[d$E == 3 & d$size == 8])
    
    result$duration.M1 <- mean(d$duration[d$E == 1 & d$size == 32])
    result$duration.M2 <- mean(d$duration[d$E == 2 & d$size == 32])
    result$duration.M3 <- mean(d$duration[d$E == 3 & d$size == 32])
    
    result$duration.L1 <- mean(d$duration[d$E == 1 & d$size == 128])
    result$duration.L2 <- mean(d$duration[d$E == 2 & d$size == 128])
    result$duration.L3 <- mean(d$duration[d$E == 3 & d$size == 128])
    
    # scaled time
    result$duration.std.H1 <- mean(scale(d$duration[d$E == 1 & d$size == 8]))
    result$duration.std.H2 <- mean(scale(d$duration[d$E == 2 & d$size == 8]))
    result$duration.std.H3 <- mean(scale(d$duration[d$E == 3 & d$size == 8]))
    
    result$duration.std.M1 <- mean(scale(d$duration[d$E == 1 & d$size == 32]))
    result$duration.std.M2 <- mean(scale(d$duration[d$E == 2 & d$size == 32]))
    result$duration.std.M3 <- mean(scale(d$duration[d$E == 3 & d$size == 32]))
    
    result$duration.std.L1 <- mean(scale(d$duration[d$E == 1 & d$size == 128]))
    result$duration.std.L2 <- mean(scale(d$duration[d$E == 2 & d$size == 128]))
    result$duration.std.L3 <- mean(scale(d$duration[d$E == 3 & d$size == 128]))
    
    ##########################################################################################
    # Скорость касания (отношение интервала между касаниями к их продолжительности)
    
    ggplot(d, aes(x=factor(size), y=remove_outliers(tick/duration))) +
      geom_violin(scale="width") +
      geom_smooth(method = "gam", se=TRUE, aes(group=1)) +
      geom_boxplot(width=.12, fill=I("black"), notch=T, outlier.size=NA, col="grey40") +
      stat_summary(fun.y="median", geom="point", shape=20, col="red", size = 10) +
      ggtitle("Результаты BART") +
      labs(x="Вид шара", y="Скорость касания, мс") +
      facet_grid(E~name, labeller = labeller(E = E.names)) +
      scale_x_discrete(labels = c("Красный шар","Желтый шар","Зеленый шар")) +
      theme(legend.position="none",
            axis.text=element_text(size=12),
            axis.title=element_text(size=16,face="bold")) +
      scale_fill_brewer() + geom_jitter(alpha = 0.8) +
      theme(plot.title = element_text(size=16, face='bold', hjust=0.5),
            axis.text.x  = element_text(angle=0, vjust=0, size=16),
            axis.ticks.x = element_blank(),
            axis.title.y = element_text(face="bold", size=20),
            axis.text.y  = element_text(angle=90, vjust=0, size=16))
    
    result$speed.H1 <- mean(d$tick[d$E == 1 & d$size == 8]/d$duration[d$E == 1 & d$size == 8], na.rm = T)
    result$speed.H2 <- mean(d$tick[d$E == 2 & d$size == 8]/d$duration[d$E == 2 & d$size == 8], na.rm = T)
    result$speed.H3 <- mean(d$tick[d$E == 3 & d$size == 8]/d$duration[d$E == 3 & d$size == 8], na.rm = T)
    
    result$speed.M1 <- mean(d$tick[d$E == 1 & d$size == 32]/d$duration[d$E == 1 & d$size == 32], na.rm = T)
    result$speed.M2 <- mean(d$tick[d$E == 2 & d$size == 32]/d$duration[d$E == 2 & d$size == 32], na.rm = T)
    result$speed.M3 <- mean(d$tick[d$E == 3 & d$size == 32]/d$duration[d$E == 3 & d$size == 32], na.rm = T)
    
    result$speed.L1 <- mean(d$tick[d$E == 1 & d$size == 128]/d$duration[d$E == 1 & d$size == 128], na.rm = T)
    result$speed.L2 <- mean(d$tick[d$E == 2 & d$size == 128]/d$duration[d$E == 2 & d$size == 128], na.rm = T)
    result$speed.L3 <- mean(d$tick[d$E == 3 & d$size == 128]/d$duration[d$E == 3 & d$size == 128], na.rm = T)
    
    result$speed.cor.H1 <- cor.test(d$tick[d$E == 1 & d$size == 8], d$duration[d$E == 1 & d$size == 8], method = "sp")$estimate
    result$speed.cor.H2 <- cor.test(d$tick[d$E == 2 & d$size == 8], d$duration[d$E == 2 & d$size == 8], method = "sp")$estimate
    result$speed.cor.H3 <- cor.test(d$tick[d$E == 3 & d$size == 8], d$duration[d$E == 3 & d$size == 8], method = "sp")$estimate
    
    result$speed.cor.M1 <- cor.test(d$tick[d$E == 1 & d$size == 32], d$duration[d$E == 1 & d$size == 32], method = "sp")$estimate
    result$speed.cor.M2 <- cor.test(d$tick[d$E == 2 & d$size == 32], d$duration[d$E == 2 & d$size == 32], method = "sp")$estimate
    result$speed.cor.M3 <- cor.test(d$tick[d$E == 3 & d$size == 32], d$duration[d$E == 3 & d$size == 32], method = "sp")$estimate
    
    result$speed.cor.L1 <- cor.test(d$tick[d$E == 1 & d$size == 128], d$duration[d$E == 1 & d$size == 128], method = "sp")$estimate
    result$speed.cor.L2 <- cor.test(d$tick[d$E == 2 & d$size == 128], d$duration[d$E == 2 & d$size == 128], method = "sp")$estimate
    result$speed.cor.L3 <- cor.test(d$tick[d$E == 3 & d$size == 128], d$duration[d$E == 3 & d$size == 128], method = "sp")$estimate
    
    
    ##########################################################################################
    # Доля взорвавшихся и сохраненных шаров
    dm <- d[,c("size","mark","E")]
    dm <- dm[dm$mark != "pump", ]; dm$mark <- droplevels(dm$mark)
    table(dm)
    mosaicplot(~E+mark+size, data = dm, color = 2:4, main = "Доля взорвавшихся и сохраненных шаров")
    
    result$save.ratio.E1 <- sum(dm$mark == "save" & dm$E == 1)/sum(dm$E == 1)
    result$save.ratio.E2 <- sum(dm$mark == "save" & dm$E == 2)/sum(dm$E == 2)
    result$save.ratio.E3 <- sum(dm$mark == "save" & dm$E == 3)/sum(dm$E == 3)
    
    result$save.ratio.H1 <- sum(dm$mark == "save" & dm$E == 1 & dm$size == 8)/sum(dm$E == 1 & dm$size == 8)
    result$save.ratio.H2 <- sum(dm$mark == "save" & dm$E == 2 & dm$size == 8)/sum(dm$E == 2 & dm$size == 8)
    result$save.ratio.H3 <- sum(dm$mark == "save" & dm$E == 3 & dm$size == 8)/sum(dm$E == 3 & dm$size == 8)
    
    result$save.ratio.M1 <- sum(dm$mark == "save" & dm$E == 1 & dm$size == 32)/sum(dm$E == 1 & dm$size == 32)
    result$save.ratio.M2 <- sum(dm$mark == "save" & dm$E == 2 & dm$size == 32)/sum(dm$E == 2 & dm$size == 32)
    result$save.ratio.M3 <- sum(dm$mark == "save" & dm$E == 3 & dm$size == 32)/sum(dm$E == 3 & dm$size == 32)
    
    result$save.ratio.L1 <- sum(dm$mark == "save" & dm$E == 1 & dm$size == 128)/sum(dm$E == 1 & dm$size == 128)
    result$save.ratio.L2 <- sum(dm$mark == "save" & dm$E == 2 & dm$size == 128)/sum(dm$E == 2 & dm$size == 128)
    result$save.ratio.L3 <- sum(dm$mark == "save" & dm$E == 3 & dm$size == 128)/sum(dm$E == 3 & dm$size == 128)
    
    ##########################################################################################
    # Продолжительность страйков
    dm <- d[,c("mark","E")]
    dm <- dm[dm$mark != "pump", ]; dm$mark <- droplevels(dm$mark)
    
    # Всего страйков взорвавшихся и сохраненных по 3 этапам
    tmp <- rle(as.character(dm$mark))
    result$seq.exploded <- mean(tmp$lengths[tmp$values == "exploded"])
    result$seq.saved    <- mean(tmp$lengths[tmp$values == "save"])
    
    # Взорвавшихся и сохраненных страйков на первом этапе
    dm1 <- dm[dm$E == 1,]
    tmp <- rle(as.character(dm1$mark))
    result$seq.exploded.E1 <- mean(tmp$lengths[tmp$values == "exploded"])
    result$seq.saved.E1    <- mean(tmp$lengths[tmp$values == "save"])
    
    # Взорвавшихся и сохраненных страйков на втором этапе
    dm2 <- dm[dm$E == 2,]
    tmp <- rle(as.character(dm2$mark))
    result$seq.exploded.E2 <- mean(tmp$lengths[tmp$values == "exploded"])
    result$seq.saved.E2    <- mean(tmp$lengths[tmp$values == "save"])
    
    # Взорвавшихся и сохраненных страйков на третьем этапе
    dm3 <- dm[dm$E == 3,]
    tmp <- rle(as.character(dm3$mark))
    result$seq.exploded.E3 <- mean(tmp$lengths[tmp$values == "exploded"])
    result$seq.saved.E3    <- mean(tmp$lengths[tmp$values == "save"])
    
    rm(dm1, dm2, dm3, tmp)
    
    # Результаты
    
    tbl <- t(result)
    tbl <- cbind(names(result), tbl); rownames(tbl) <- NULL
    colnames(tbl) <- c("Показатель", "Численное значение")
    tbl <- knitr::kable(tbl, align = "c")
    kableExtra::kable_styling(tbl, "striped", full_width = T, position = "left")
    
    # save results
    if(file.exists(sprintf("%sreport.csv", gsub(basename(filename), "", tools::file_path_as_absolute(filename))))) {
      write.table(result, file = sprintf("%sreport.csv",gsub(basename(filename), "", tools::file_path_as_absolute(filename))),
                  sep = ";", row.names = F, append = T, col.names = F)
    } else {
      write.table(result, file = sprintf("%sreport.csv",gsub(basename(filename), "", tools::file_path_as_absolute(filename))), sep = ";", row.names = F)
    }
  }
}

# usage:
# do.estimation(filename = "result.csv")

