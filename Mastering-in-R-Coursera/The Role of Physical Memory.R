library(pryr)
mem_used()
ls() 
object_size(ext_tracks)

library(magrittr)
sapply(ls(), function(x) object_size(get(x))) %>% sort %>% tail(5)

mem_used()
rm(ext_tracks)
mem_used()
#mem_change(rm(check_tracks, denver, b))
object_size(integer(0))
object_size(integer(1000))  ## 4 bytes per integer
object_size(numeric(1000))  ## 8 bytes per numeric
str(.Machine)
gc()

