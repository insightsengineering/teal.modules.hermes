.onLoad <- function(libname, pkgname) { # nolint
  teal.logger::register_logger(namespace = "teal.modules.hermes")
  teal.logger::register_handlers("teal.modules.hermes")
}
