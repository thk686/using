#' Load objects from packages
#'
#' Loads objects contained in packages or the entire package
#'
#' @param ... names of objects to be loaded
#'
#' @details Given an object name, this function will make that object available
#'   in the current \link{search} path. If the name given names a package, the
#'   entire package will be added to the search path. If the name specifies an
#'   object within a package, only that object will be available in the search
#'   path. The namespace of the object's package will be loaded, so other
#'   objects from that package will be available if given a namespace prefix.
#'   Note that you can expose objects from packages that are not exported. This
#'   is generally a bad idea, except for purposes of debugging.
#'
#'   Package namespaces are loaded by calling \code{\link{loadNamespace}} with
#'   default arguments. If you need more control, manage package loading
#'   manually prior to calling \code{using}.
#'
#'   The function \code{stop_using} attempts to remove the object from the
#'   search path or an entire package. This is intrinsically less reliable for
#'   packages, so use with caution.
#'
#' @examples
#' using(using::use)
#' list_using()
#' formals(use)
#' stop_using(use)
#' list_using()
#' using(my_use = using::use)
#' list_using()
#' formals(my_use)
#' stop_using(my_use)
#'
#' @rdname using
#' @export
using = function(...)
{
  x = deparse(substitute(list(...)))
  x = gsub("list\\(|\\)|\"", "", x)
  x = unlist(strsplit(x, ", "))
  if (length(x) == 0 ||
      any(!nzchar(x)))
    stop("Invalid input")
  invisible(lapply(x, use))
}

#' @rdname using
#' @export
stop_using = function(...)
{
  x = deparse(substitute(list(...)))
  x = gsub("list\\(|\\)|,|\"", "", x)
  x = unlist(strsplit(x, " "))
  if (length(x) == 0 ||
      any(!nzchar(x)))
    stop("Invalid input")
  invisible(lapply(x, unuse))
}

#' @rdname using
#' @export
list_using = function() ls(envir = get_env(), all.names = TRUE)

use = function(directive)
{
  vals = unlist(strsplit(directive, "::| = "))
  if (any(!nzchar(vals))) stop("Invalid input")
  switch(paste0("is", length(vals)),
    is3 =
    {
      newname = vals[1]
      nsname = vals[2]
      oldname = vals[3]
    },
    is2 =
    {
      newname = vals[2]
      nsname = vals[1]
      oldname = vals[2]
    },
    is1 =
    {
      attach_package(vals)
      return(directive)
    },
    default = stop("Invalid input")
  )
  loadNamespace(nsname)
  obj = getFromNamespace(oldname, nsname)
  assign(newname, obj, envir = get_env())
  return(directive)
}

unuse = function(directive)
{
  vals = unlist(strsplit(directive, "::| = "))
  if (any(!nzchar(vals))) stop("Invalid input")
  switch(paste0("is", length(vals)),
         is3 =
         {
           newname = vals[1]
           nsname = vals[2]
           oldname = vals[3]
         },
         is2 =
         {
           newname = vals[2]
           nsname = vals[1]
           oldname = vals[2]
         },
         is1 =
         {
           newname = vals;
           nsname = vals;
           oldname = vals;
         },
         default = stop("Invalid input")
  )
  if (newname %in% list_using())
    remove(list = newname, envir = get_env())
  else
    unload_package(nsname)
  return(directive)
}

get_env = function() as.environment("_using_")

is_attached = function(x)
  any(grepl(paste0("package:", x), search()))

attach_package = function(x)
  if (!is_attached(x)) attachNamespace(x)

unload_package = function(x)
{
  if (is_attached(x))
    do.call("detach", list(name = paste0("package:", x)))
  if (isNamespaceLoaded(x)) unloadNamespace(x)
}

attach_environment = function()
  if (!"_using_" %in% search())
    attach(NULL, name = "_using_")

detach_environment = function()
  while("_using_" %in% search()) detach("_using_")

.onLoad = function(...)
{
  detach_environment()
  attach_environment()
}

.onUnload = function(...) detach_environment()



