library(data.table)

RawDataHelyreigazitasok <- fread("helyreigazitasok.csv", dec = ",")
RawDataCimkek <- fread("cimkek.csv", dec = ",")
RawDataCimkeHelyreigazitas <- fread("cimke_helyreigazitas.csv", dec = ",")
RawDataSajtotermekek <- fread("sajtotermekek.csv")
RawDataHivatkozasok <- fread("hivatkozasok.csv", dec = ",")
RawDataHivatkozasok <- merge(RawDataHivatkozasok, RawDataSajtotermekek)

RawDataHelyreigazitasok$HelyreigazitasDatum <- ifelse(RawDataHelyreigazitasok$HelyreigazitasURL!="",
                                                      RawDataHelyreigazitasok$HelyreigazitasDatum, "")
RawDataHelyreigazitasok$HelyreigazitasIdo <- ifelse(RawDataHelyreigazitasok$HelyreigazitasURL!="",
                                                    RawDataHelyreigazitasok$HelyreigazitasIdo, "")

RawDataHelyreigazitasok <- merge(RawDataHelyreigazitasok, RawDataSajtotermekek)

RawDataHelyreigazitasok$Hivatkozasok <- lapply(RawDataHelyreigazitasok$HelyreigazitasID, function(id) RawDataHivatkozasok[HelyreigazitasID==id])
RawDataHelyreigazitasok$Cimkek <- lapply(RawDataHelyreigazitasok$HelyreigazitasID, function(id) merge(RawDataCimkeHelyreigazitas, RawDataCimkek)[HelyreigazitasID==id])

RawDataHelyreigazitasok$HelyreigazitasSzoveg <- gsub("\n", "<br>", RawDataHelyreigazitasok$HelyreigazitasSzoveg)
RawDataHelyreigazitasok$HelyreigazitasSzoveg <- gsub("<br><br>", "<br>", RawDataHelyreigazitasok$HelyreigazitasSzoveg)

RawDataHelyreigazitasok$Keses <- ifelse(RawDataHelyreigazitasok$HelyreigazitasDatum=="", NA,
                                        difftime(
                                          as.POSIXct(paste(ifelse(RawDataHelyreigazitasok$HelyreigazitasDatum=="", "2000/01/01", RawDataHelyreigazitasok$HelyreigazitasDatum),
                                                           ifelse(RawDataHelyreigazitasok$HelyreigazitasIdo=="", "12:00", RawDataHelyreigazitasok$HelyreigazitasIdo))),
                                          as.POSIXct(paste(RawDataHelyreigazitasok$CikkDatum, ifelse(RawDataHelyreigazitasok$CikkIdo=="", "12:00", RawDataHelyreigazitasok$CikkIdo))),
                                          units = "days"))

jsonlite::write_json(list(data = RawDataHelyreigazitasok), "helyreigazitasok.json", na = "string")

# jsonlite::write_json(RawDataCimkek$CimkeSzoveg, "cimkek.json")

cimkekfajl <- file("cimkek.js")
writeLines(paste0("var cimkek = ['", paste0(RawDataCimkek$CimkeSzoveg, collapse = "','"), "'];"), cimkekfajl)
close(cimkekfajl)

temp <- merge(merge(RawDataHelyreigazitasok, RawDataCimkeHelyreigazitas, by = "HelyreigazitasID"), RawDataCimkek, by = "CimkeID")
temp <- merge(RawDataHelyreigazitasok, RawDataCimkeHelyreigazitas, by = "HelyreigazitasID")
temp$Year <- as.numeric(substring(temp$CikkDatum, 1, 4))
temp$Month <- as.numeric(substring(temp$CikkDatum, 6, 7))

temp2 <- setkey(temp, Year, Month, CimkeID)[
  CJ(seq(min(temp$Year), max(temp$Year), by = 1), 1:12, unique(CimkeID)), .N, by = .EACHI]
temp2$date <- as.Date(paste0(temp2$Year, "/", temp2$Month, "/", 15))
temp2 <- merge(temp2, RawDataCimkek, by = "CimkeID")

jsonlite::write_json(lapply(RawDataCimkeHelyreigazitas[, .N , .(CimkeID)][order(N, decreasing = TRUE)]$CimkeID, function(cid)
  list(type = "scatter", name = RawDataCimkek[CimkeID==cid]$CimkeSzoveg,
       OsszN = sum(RawDataCimkeHelyreigazitas$CimkeID==cid),
       PolitikaiPart = RawDataCimkek[CimkeID==cid]$PolitikaiPart,
       x = temp2[CimkeID==cid]$date, y = temp2[CimkeID==cid]$N)), "helyreigido.json", auto_unbox = TRUE)

temp <- merge(rbindlist(lapply(merge(RawDataCimkeHelyreigazitas, RawDataCimkek)[PolitikaiPart==0, .N , .(CimkeID)][N>10][order(N, decreasing = TRUE)]$CimkeID, function(cid) {
  docs <- tm::Corpus(tm::VectorSource(RawDataHelyreigazitasok[HelyreigazitasID%in%RawDataCimkeHelyreigazitas[CimkeID==cid]$HelyreigazitasID]$HelyreigazitasSzoveg))
  docs <- tm::tm_map(docs, tm::removePunctuation)
  docs <- tm::tm_map(docs, tm::removeNumbers)
  docs <- tm::tm_map(docs, tm::stripWhitespace)
  docs <- tm::tm_map(docs, tm::content_transformer(tolower))
  docs <- tm::tm_map(docs, tm::removeWords, tm::stopwords("hungarian"))
  docs <- tm::tm_map(docs, tm::stripWhitespace)
  
  dtm <- as.matrix(tm::TermDocumentMatrix(docs))
  dtm <- sort(rowSums(dtm), decreasing = TRUE)
  if(length(dtm)>0) data.table(CimkeID = cid, word = names(dtm), freq = dtm) else NULL
})), RawDataCimkek, by = "CimkeID", sort = FALSE)

stopwords <- c("??ll??tottuk", "val??tlanul", "cikk??nkben", "megjelent", "napj??n", "h??resztelt??k", "val??s??g",
               "c??m??", "cikkben", "c??mmel", "cikk??nk", "t??vesen", "lapunk", "sz??m??ban", "hamis",
               "val??s??gban", "val??tlan", "sz??nben", "k??z??lt??k", "janu??r", "febru??r", "m??rcius", "??prilis",
               "m??jus", "j??nius", "j??lius", "augusztus", "szeptember", "okt??ber", "november",
               "december", "t??nyt", "t??ntett??k", "alperes", "felperes", "j??h??rn??vhez", "f??z??d??",
               "h??resztelte", "rend??", "???", "l??tszatot", "keltett??nk", "keltett??k", "megs??rtett??k")

unique(temp$CimkeSzoveg)

optsizes <- data.table(CimkeSzoveg = c("Juh??sz P??ter", "Gyurcs??ny Ferenc", "Czegl??dy Csaba", "Vona G??bor",
                                       "Portik Tam??s", "SZEVI??P", "Jakab P??ter", "Habony ??rp??d",
                                       "Simicska Lajos", "Botka L??szl??", "Ujhelyi Istv??n",
                                       "M??sz??ros L??rinc", "Bajnai Gordon", "Magyar Helsinki Bizotts??g"),
                       size = c(0.35, 0.3, 0.5, 0.65, 0.45, 0.7, 0.5, 0.5, 0.7, 0.3, 0.6, 0.45, 0.7, 0.4))

wcres <- rbindlist(lapply(unique(temp$CimkeSzoveg), function(csz) {
  temp2 <- temp[CimkeSzoveg==csz&!word%in%stopwords&!word%in%tolower(strsplit(csz, " ")[[1]]), .(word, freq)]
  htmlwidgets::saveWidget(wordcloud2::wordcloud2(temp2, size = optsizes[CimkeSzoveg==csz]$size),
                          "../wctemp.html", selfcontained = FALSE)
  webshot::webshot("../wctemp.html", "../wctemp.png", delay = 10)
  data.table(CimkeSzoveg = csz, Nevvel = TRUE, Img = xfun::base64_uri("../wctemp.png"))
}))

jsonlite::write_json(wcres, "szofelhok.json")
