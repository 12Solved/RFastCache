# Liblary shoud be defined somwere else?

library(R6)

RFastCache <- R6Class(
  "RFastCache",
  public = list(

    initialize = function(dir){
      # Init paths
      private$cache.dir = dir
      private$cache.ttl_path = file.path(private$cache.dir, "ttl")

      # If no dir, create one.
      # !!! What should mode be? 777? or ??? !!!
      if(!dir.exists(private$cache.dir)){
        dir.create(path = private$cache.dir, showWarnings = TRUE, recursive = TRUE, mode = "0777")
      }

      # If no ttl table, create one.
      if(!file.exists(private$cache.ttl_path)){
        ttl_table = c()
        saveRDS(object = ttl_table, file = private$cache.ttl_path)
      }
    },


    set = function(key, value, ttl = NULL){
      # ttl is reserved for ttl table.
      if(key != "ttl"){
        # If not number transform the ttl to number
        if(!is.numeric(ttl)){
          # Implement later is.POSIXt(ttl)
          ttl = 1000
        }
        # Update ttl table.
        # !!! Concurency problems !!!
        die = Sys.time() + ttl
        ttl_table <- readRDS(file = private$cache.ttl_path)
        ttl_table[key] = die
        saveRDS(object = ttl_table, file = private$cache.ttl_path)

        # Update object.
        path = file.path(private$cache.dir, key)
        saveRDS(object = value, file = path)
      }
    },

    get = function(key, default = NULL){
      ttl_table <- readRDS(file = private$cache.ttl_path)
      # If key is valid get file
      if(!is.null(ttl_table[key]) && !is.na(ttl_table[key]) && ttl_table[key] > Sys.time()){
        path = file.path(private$cache.dir, key)
        readRDS(file = path)
      }
      else{
        default
      }
    },

    has = function(key){
      ttl_table <- readRDS(file = private$cache.ttl_path)
      # Same as get, but doesn't read object.
      !is.null(ttl_table[key]) && !is.na(ttl_table[key]) && ttl_table[key] > Sys.time()
    },

    delete = function(key){
      # !!! How to remove values from c() completely, curently seting to NA !!!
      ttl_table <- readRDS(file = private$cache.ttl_path)
      if(!is.null(ttl_table[key]) && !is.na(ttl_table[key])){
        # Update ttl table
        ttl_table <- readRDS(file = private$cache.ttl_path)
        ttl_table[key] = NA
        saveRDS(object = ttl_table, file = private$cache.ttl_path)

        # Delete file
        path = file.path(private$cache.dir, key)
        if (file.exists(path)){
          file.remove(path)
        }
      }
    },

    clear = function(){
      # !!! How to remove values from c() completely, curently seting to NA !!!
      ttl_table <- readRDS(file = private$cache.ttl_path)
      rm_list = names(Filter(f = function(x){x < Sys.time()}, x = ttl_table))
      ttl_table[rm_list] = NA
      saveRDS(object = ttl_table, file = private$cache.ttl_path)

      paths = file.path(private$cache.dir, rm_list)
      file.remove(paths)
    }


  ),

  private = list(
    cache.dir = NULL,
    cache.ttl_path = NULL
  )
)

# mycache = RFastCache$new(dir="tmp/mycache")
# bigdatatable=data.frame(x=c(1,2,3),y=c(1,4,9))
# mycache$set('mykey', bigdatatable, 10)
# print(mycache$get('long', default="long not cached anymore"))
#
# for(i in 1:20) {
#   print(mycache$get('mykey', default="mykey not cached anymore"))
#   Sys.sleep(1); #1s
# }
#
# mycache$set('long', bigdatatable, 10*60)
