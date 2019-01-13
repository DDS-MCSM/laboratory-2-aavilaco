#******************************************************************************#
#                                                                              #
#                          Lab 2 - CPE Standard                                #
#                                                                              #
#              Arnau Sangra Rocamora - Data Driven Securty                     #
#                                                                              #
#******************************************************************************#

# install.packages("xml2")
# library(xml2)

#compressed_cpes_url <- "https://nvd.nist.gov/feeds/xml/cpe/dictionary/official-cpe-dictionary_v2.3.xml.zip"
#cpes_filename <- "cpes.zip"
#download.file(compressed_cpes_url, cpes_filename)
#unzip(zipfile = cpes_filename)
cpe.file <- "./official-cpe-dictionary_v2.3.xml"

concat <- function(...) {
  return (paste(..., sep="", collapse=""))
}

GetCPEItems <- function(cpe.raw) {
  items <- xml2::xml_find_all(cpe.raw, "//cpe-item")
  mat <- matrix(ncol = 5, nrow = length(items))

  i <- 1
  for (a in items) {
    # Creamos un documento XML nuevo con el nodo para no tener que parsear
    # el archivo entero para obtener los datos de cada item
    item <- xml2::xml_new_root("cpe-item")
    xml2::xml_add_child(item, a)

    name <- xml2::xml_attr(item, "name")
    title <- xml2::xml_text(xml2::xml_find_first(item, "//title/text()"))
    references <- paste(xml2::xml_text(xml2::xml_find_all(item, "//references/reference/text()")), collapse="|")
    notes <- paste(xml2::xml_text(xml2::xml_find_all(item, "//notes/note/text()")), collapse="|")
    check <- xml2::xml_text(xml2::xml_find_first(item, "//check/text()"))

    mat[i,] <- c(name, title, references, notes, check)
    if (i %% 1000 == 0) {
      print(i)
    }
    i <- i + 1
  }

  df <- data.frame(mat)
  names(df) <- c("cpe-name", "title", "references", "notes", "check")

  return (df)
}

CleanCPEs <- function(cpes){

  # data manipulation

  return(data.frame())
}

ParseCPEData <- function(cpe.file) {

  # load cpes as xml file
  cpes <- xml2::read_xml(cpe.file)

  # get CPEs
  cpes <- GetCPEItems(cpes)

  # transform, clean, arrange parsed cpes as data frame
  df <- CleanCPEs(cpes)

  # return data frame
  return(df)
}

df <- ParseCPEData("./official-cpe-dictionary_v2.3.xml")


