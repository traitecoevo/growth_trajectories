table_plant_parameters <- function(plant_parameters, dest){
  x <- xtable(plant_parameters[-c(1,2,4,7,10,21,22,25:31),1:4],
              hline.after=c(1),
              align='lp{5cm}lll')
  y <- print(x, sanitize.text.function=as.character,
             include.rownames=FALSE, floating=FALSE,
             print.results=FALSE)
  cat(y, file=dest)
}
