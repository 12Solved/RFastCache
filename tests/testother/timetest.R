dir_path = "tmp/test"
ttl_path = file.path(dir_path, "ttl")



add_1_to_n = function(n){
  mycache = RFastCache$new(dir=dir_path)
  for (i in c(1:n)) {
    mycache$set(key = toString(x = i), value = i, ttl = 10000)
  }
}

test1 = function(n){
  mycache = RFastCache$new(dir=dir_path)

  time = Sys.time()
  add_1_to_n(n)
  time = Sys.time() - time
  cat("Add", n, "in:", time, "s.\n")
  flush.console()

  time = Sys.time()
  print(mycache$get(key = toString(x = 1)))
  time = Sys.time() - time
  cat("Get first in:", time, "s.\n")
  flush.console()

  time = Sys.time()
  print(mycache$get(key = toString(x = floor(n/2))))
  time = Sys.time() - time
  cat("Get middle in:", time, "s.\n")
  flush.console()

  time = Sys.time()
  print(mycache$get(key = toString(x = n)))
  time = Sys.time() - time
  cat("Get last in:", time, "s.\n")
  flush.console()

  unlink(x = dir_path, recursive = TRUE)
}




test2 = function(){
  # mb100 = read.csv("C:\\Users\\Gin\\Downloads\\Age-sex-by-ethnic-group-grouped-total-responses-census-usually-resident-population-counts-2006-2013-2018-Censuses-RC-TA-SA2-DHB\\Data8277.csv")
  # mb10 = read.csv("C:\\Users\\Gin\\Downloads\\Age-sex-by-ethnic-group-grouped-total-responses-census-usually-resident-population-counts-2006-2013-2018-Censuses-RC-TA-SA2-DHB\\gsquarterlySeptember20.csv")
  # mb1 = read.csv("C:\\Users\\Gin\\Downloads\\Age-sex-by-ethnic-group-grouped-total-responses-census-usually-resident-population-counts-2006-2013-2018-Censuses-RC-TA-SA2-DHB\\annual-enterprise-survey-2019-financial-year-provisional-size-bands-csv.csv")
  mycache = RFastCache$new(dir=dir_path)

  time = Sys.time()
  mycache$set(key = "mb1",value = mb1)
  time = Sys.time() - time
  cat("Add 1 mb in:", time, "s.\n")

  time = Sys.time()
  test = mycache$get(key = "mb1")
  time = Sys.time() - time
  cat("Get 1 mb in:", time, "s.\n\n")

  # print(test)


  time = Sys.time()
  mycache$set(key = "mb10",value = mb10)
  time = Sys.time() - time
  cat("Add 10 mb in:", time, "s.\n")

  time = Sys.time()
  mycache$get(key = "mb10")
  time = Sys.time() - time
  cat("Get 10 mb in:", time, "s.\n\n")



  time = Sys.time()
  mycache$set(key = "mb100",value = mb100)
  time = Sys.time() - time
  cat("Add 100 mb in:", time, "s.\n")

  time = Sys.time()
  mycache$get(key = "mb100")
  time = Sys.time() - time
  cat("Get 100 mb in:", time, "s.\n\n")


}
#test = readRDS(file = ttl_path)
# test = readRDS(file.path(dir_path, "1"))

#unlink(x = dir_path, recursive = TRUE)


# > test2()
# CSV of 1 mb (R object of  55 kb)
# Add 1 mb in: 0.1462989 s.
# Get 1 mb in: 0.071841 s.
#
# CSV of 72 mb (R object of 8 mb)
# Add 10 mb in: 4.437661 s.
# Get 10 mb in: 1.731059 s.
#
# CSV of 837 mb (R object of 24 mb)
# Add 100 mb in: 27.66034 s.
# Get 100 mb in: 16.20583 s.
