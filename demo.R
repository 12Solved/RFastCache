library(R6)


RFastCache <- R6Class(
  "RFastCache",
  public = list(
    
    initialize = function(dir){
      private$cache.dir = dir
      private$cache.ttl_path = file.path(private$cache.dir, "ttl")
      if(!dir.exists(private$cache.dir)){
        dir.create(path = private$cache.dir, showWarnings = TRUE, recursive = FALSE, mode = "0777")
      }
      if(!file.exists(private$cache.ttl_path)){
        val = c()
        saveRDS(object = val, file = private$cache.ttl_path)
      }
    },
    
    
    set = function(key, value, ttl = NULL){
      # ttl is reserved for ttl table
      if(key != "ttl"){
        if(!is.numeric(ttl)){
          # Some const
          # Implement later is.POSIXt(ttl)
          ttl = 1000
        }
        
        die = Sys.time() + ttl
        ttl_table <- readRDS(file = private$cache.ttl_path)
        ttl_table[key] = die
        saveRDS(object = ttl_table, file = private$cache.ttl_path)

        path = file.path(private$cache.dir, key)
        saveRDS(object = value, file = path)
      }
    },
    
    get = function(key, default = NULL){
      ttl_table <- readRDS(file = private$cache.ttl_path)
      if(!is.null(ttl_table[key]) && !is.na(ttl_table[key]) && ttl_table[key] > Sys.time()){
        path = file.path(private$cache.dir, key)
        print(readRDS(file = path))
      }
      else{
        if(!is.null(default)){
          default
        }
        else{
          print("Data expired!")
        }
      }
    },
    
    has = function(key){
      # ttl_table <- readRDS(file = private$cache.ttl_path){
      #   if(!is.null(ttl_table[key]) && !is.na(ttl_table[key]) && ttl_table[key] > Sys.time()){
      #     #TRUE
      #   }
      #   else{
      #     #FALSE
      #   }
      # }
    }
    
    
  ),
  private = list(
    cache.dir = NULL,
    cache.ttl_path = NULL
  )
)

ca = RFastCache$new("Cache")
ca$get(key = "abc")
ca$get(key = "abc", default = c(1,2,3))
ca$set(key = "for_30_s", value = c("One", "Six"), ttl = 30)
ca$get(key = "for_30_s")
# ca$set(key = "for_10_s", value = "abc", ttl = 10)
# ca$get(key = "for_10_s")