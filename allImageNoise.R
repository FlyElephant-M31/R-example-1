# вообщем получается так, что в некоторой степени приближённо
# можно для ОСШ 20, 10 и 0 дБ можно брать СКО нормального шума
# 0.03, 0.1 и 0.3

con <- file("data")
sink(con, append = TRUE, type = "output")
sink(con, append = TRUE, type = "message")


install.packages('foreach') 
install.packages('jpeg') 

MyNorm = function(data)
{
  data <- data - min(data)
  data <- data / max(data)
  
}
# функция для оценки параметров изображений
# и для их искажений

procdir <- '~/workdir/images/'
fls <- list.files(procdir, pattern = "\\.jpg$", ignore.case = TRUE)

snrs <- c(20, 10, 0)
starts <- 1

save(snrs, file = '1.rdata')

library(foreach)
library(jpeg)

prval <- c(seq(0.01, 1, 0.01))

system.time(foreach(imInd = 1:2, .packages = 'jpeg') %do% # %dopar%
{              
  somesnr <- c()
  print(imInd)
  img <- readJPEG(paste(procdir, fls[[imInd]], sep = ""))

  if (length(dim(img)) == 3)
  {
    img <- img[,,1] + img[,,2] + img[,,3];
    img <- img / 3;
  }  
  
  # нормализация изображения по амплитуде к [0; 1]
  img <- MyNorm(img)
  
  # формирование искажённого изображения
  
  for (t in 1:length(snrs))
  {
    # формирование искажённого изображения
    
    srcsnr <- snrs[t]
    thr <- 0.5
    for (ipr in starts:length(prval))
    {
      ns <- rnorm(prod(dim(img)), 0, prval[ipr])
      dimg <- img + ns
      dimg <- MyNorm(dimg)
      snr <- 10*log10( sum(img ^ 2) / sum(ns ^ 2) )
      if (((snr - srcsnr) < thr) && (snr > 0))
      {
        #print(prval[ipr])
        print(snr)
        starts <- ipr
        somesnr <- c(somesnr, prval[ipr])
        break
      }
    }
  }
  write(somesnr, file = 'data')
})

