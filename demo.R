library(R6)


RFastCache <- R6Class(
  "RFastCache",
  public = list(
    
    initialize = function(dir){
      private$cache.dir = dir
      if(!dir.exists(private$cache.dir)){
        dir.create(path = private$cache.dir, showWarnings = TRUE, recursive = FALSE, mode = "0777")
      }
    },
    
    
    set = function(key, value, ttl = NULL){
      # ttl is reserved for ttl table
      if(key != "ttl"){
        data = value
        ttl_path = file.path(private$cache.dir, "ttl")
        if(!is.numeric(ttl)){
          # Some const
          ttl = 1000
        }
        die = Sys.time() + ttl
        load(file = ttl_path)
        value = c(value, setNames(die,key))
        save(value, file = ttl_path)
        # Implement later is.POSIXt(ttl)
        path = file.path(private$cache.dir, key)
        save(data, file = path)
      }
    },
    
    get = function(key, default = null){
      path = file.path(private$cache.dir, key)
      load(file = path)
      value
    }
    
    
    
  ),
  private = list(
    cache.dir = NULL
  )
)