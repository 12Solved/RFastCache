#' @name RFastCache
#' @title RFastCache
#'
#' Cache class for R with set, get, has, delete and clear functions.

#' @docType class
#' @importFrom R6 R6Class
#' @export RFastCache
#' @description An R6 class that chaches an R object.
#' @usage
#' mycache <- RFastCache$new('/path/to/cache')
#' ... FIXME
#'
#' @format An ['R6Class`] generator object.
#' @keywords cache, disk
#' @examples
#' mycache = RFastCache$new(dir="tmp/mycache")
#' bigdatatable=data.frame(x=c(1,2,3),y=c(1,4,9))
#' mycache$set('mykey', bigdatatable, 10)
#' print(mycache$get('long', default="long not cached anymore"))
#'
#' for(i in 1:20) {
#'   print(mycache$get('mykey', default="mykey not cached anymore"))
#'   Sys.sleep(1); #1s
#' }
#'
#' mycache$set('long', bigdatatable, 10*60)



# Liblary shoud be defined somwere else?

#library(R6)

RFastCache <- R6Class(
  "RFastCache",
  public = list(

    #' @description
    #' Initializes R6 cache object.
    #' @param dir Directory for cache.
    #' @return A R6 RFastCache object.
    #' @examples cache = RFastCache$new(my/new/cache)
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
        saveRDS(object = ttl_table, file = private$cache.ttl_path, compress = FALSE)
      }
    },


    #' @description
    #' Method for adding objects to the cache
    #' @param key By this key object will be stored in cache
    #' @param value The value to be stored
    #' @param ttl Time to live until stored object is outdated
    #' @examples
    #' My_value = c(1:100)
    #' cache$set(key = "My_Key", value = My_value, 1000)
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
        saveRDS(object = ttl_table, file = private$cache.ttl_path, compress = FALSE)

        # Update object.
        path = file.path(private$cache.dir, key)
        saveRDS(object = value, file = path, compress = FALSE)
      }
    },

    #' @description
    #' Method for geting objects from the cache
    #' @param key The key by which the cached value will be retrieved
    #' @param default If no value with the given key exist, set the value to default
    #' @return Returns value by key, if no value exist returns default
    #' @examples
    #' value = cache$get(key = "My_Key", default = "Sorry, but no value")
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

    #' @description
    #' Method for looking up objects from the cache
    #' @param key The key by which the cached value will be looked up
    #' @return true if value exist, false if no value or if value is outdated
    #' @examples
    #' is_value_there = cache$has(key = "My_Key")
    has = function(key){
      ttl_table <- readRDS(file = private$cache.ttl_path)
      # Same as get, but doesn't read object.
      !is.null(ttl_table[key]) && !is.na(ttl_table[key]) && ttl_table[key] > Sys.time()
    },

    #' @description
    #' Method for removing objects from the cache by key
    #' @param key The key of value
    #' @examples
    #' cache$delete(key = "My_Key")
    delete = function(key){
      # !!! How to remove values from c() completely, curently seting to NA !!!
      ttl_table <- readRDS(file = private$cache.ttl_path)
      if(!is.null(ttl_table[key]) && !is.na(ttl_table[key])){
        # Update ttl table
        ttl_table <- readRDS(file = private$cache.ttl_path)
        ttl_table[key] = NA
        saveRDS(object = ttl_table, file = private$cache.ttl_path, compress = FALSE)

        # Delete file
        path = file.path(private$cache.dir, key)
        if (file.exists(path)){
          file.remove(path)
        }
      }
    },

    #' @description
    #' Method for clearing outdated objects from the cache
    #' @examples
    #' cache$clear()
    clear = function(){
      # !!! How to remove values from c() completely, curently seting to NA !!!
      ttl_table <- readRDS(file = private$cache.ttl_path)
      rm_list = names(Filter(f = function(x){x < Sys.time()}, x = ttl_table))
      ttl_table[rm_list] = NA
      saveRDS(object = ttl_table, file = private$cache.ttl_path, compress = FALSE)

      paths = file.path(private$cache.dir, rm_list)
      file.remove(paths)
    }


  ),

  private = list(
    cache.dir = NULL,
    cache.ttl_path = NULL
  )
)
