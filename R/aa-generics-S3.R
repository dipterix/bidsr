# ---- S3 common -------------------------------------------------------

format.generic <- S7::new_external_generic("base", "format", "x")
as.character.generic <- S7::new_external_generic("base", "as.character", "x")
print.generic <- S7::new_external_generic("base", "print", "x")
names.generic <- S7::new_external_generic("base", "names", "x")
as.list.generic <- S7::new_external_generic("base", "as.list", "x")
as.data.frame.generic <- S7::new_external_generic("base", "as.data.frame", "x")

extract_bracket.generic <- S7::new_external_generic("base", "[[",  c("x", "name"))
# extract_set_bracket.generic <- S7::new_external_generic("base", "[[<-", c("x", "name", "value"))

extract.generic <- S7::new_external_generic("base", "$", c("x", "name"))
# extract_set.generic <- S7::new_external_generic("base", "$<-", c("x", "name", "value"))
