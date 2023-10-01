list.of.packages <- c("audio", "ggplot2", "sound", "grid","devtools", "magrittr")

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages) > 0){install.packages(new.packages, dependencies = TRUE, quiet = TRUE)}
invisible(lapply(list.of.packages, function(x){
  suppressPackageStartupMessages(library(x, character.only = TRUE,
                                         warn.conflicts = FALSE,  quietly = TRUE))}))

# #list.of.packages <- c("yyplot", "htmltab")
# list.of.packages <- "yyplot"
# new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
# if(length(new.packages) > 0){
#   if("yyplot" %in% new.packages){
#     Sys.unsetenv("GITHUB_PAT")
#     install_github("GuangchuangYu/yyplot", dependencies = TRUE, upgrade = "never")
#   } 
#   
#   if("htmltab" %in% new.packages){
#     install.packages("~/Maria_Camila/input/htmltab_0.8.2.tar.gz", repos = NULL, type = "source")
#   }
# }

#library(yyplot)
salida <- tempfile(fileext = ".zip")
download.file("https://github.com/OscarGOGO/test_hbd/blob/main/input.zip?raw=TRUE",
              salida, quiet = T, mode = "wb")
out <- unzip(salida, exdir = tempdir())
#Empieza
bday <- data.frame(notas = c('D4', 'D4', 'E4', 'D4', 'G4', 'F#4', 'D4', 'D4', 'E4', 'D4', 'A4',
                             'G4', 'D4', 'D4', 'D5', 'B4', 'G4', 'F#4', 'E4', 'C5', 'C5', 'B4',
                             'G4', 'A4', 'G4'), 
                   value = c(rep(c(0.75, 0.25, 1, 1, 1, 2), 2), 
                             0.75, 0.25, 1, 1, 1, 1, 1, 0.75, 0.25, 1, 1, 1, 2))

# wikitab <- tryCatch(htmltab("https://de.wikipedia.org/wiki/Frequenzen_der_gleichstufigen_Stimmung",1, colNames = c("note","eng","ger","freq")), error = function(err)err)
# if(inherits(wikitab, "error")){
#   wikitab <- readRDS("./input/tabla_notas.rds")
# } else {
#   wikitab$freq <- suppressWarnings(as.numeric(gsub(",", ".", wikitab$freq)))
#   wikitab$freq[40] <- 440; wikitab$eng <- substr(wikitab$eng,0,3)  
# }
wikitab <- readRDS(out[which(basename(out) == "tabla_notas.rds")])
bday$freq <- lapply(bday$notas, function(x){
  x.1 <- wikitab[wikitab$eng==x,4]; return(x.1)
})

cancion_MC <- mapply(Sine, bday$freq, (bday$value/110)*60, rate=44100, channels=1 ) %>%
  do.call("c", .) %>% as.Sample(.)


## Ahora algo muy bonito para ti :)
t <- seq(0,2*pi, by = 0.16);
#d <- data.frame(x = 16*sin(t)^3, y = 13*cos(t) - 5*cos(2*t) - 2*cos(3*t) - cos(4*t))
# d$color1 = sample(diverging_hcl(9, "Purple-Green"), nrow(d), replace = T)
# d$color2 = sample(sequential_hcl(5, "OrYel"), nrow(d), replace = T)
# d$size = runif(nrow(d), 0.05, 0.1); d$angle = runif(nrow(d), -360, 360)
#saveRDS(d, file = "./input/d.rds")
d <- readRDS(out[which(basename(out) == "d.rds")])

# p3 <- ggplot(d, aes(x, y)) +
#   geom_cake(aes(color.cake = color1,
#                 color.candle = color2,
#                 color.fire = color1,
#                 size = I(size),
#                 angle = angle)) + theme_void() + 
#   theme(panel.background = element_rect(fill = "#fa495d"))
#saveRDS(p3, "./input/p3.rds")
p3 <- readRDS(out[which(basename(out) == "p3.rds")])
p4 <- p3 + annotate("text", x = 0, y = 2, label = "De: Oscar",
              colour = "white", size = 7)+
  annotate("text", x = 0, y = 0, label = "Para: Maria Camila",
           colour = "white", size = 7)+
  annotate("text", x = 0, y = -3, label = "Feliz cumpleaños",
           colour = "white", size = 9)+
  annotate("text", x = 0, y = -6, label = "TQM",
           colour = "white", size = 9)
Sys.sleep(3)
plot(p4)
Sys.sleep(3)
message("Te quiero mucho, y espera aquí viene otra sorpresa, estoy seguro que la reconoceras")
audio::play(cancion_MC)
Sys.sleep(10)
message("espera")
Sys.sleep(3)
message("espera")
Sys.sleep(3)
bd <- audio::load.wave(out[which(basename(out) == "The Lord of the Rings   Bilbo's Birthday.wav")])
audio::play(bd)



