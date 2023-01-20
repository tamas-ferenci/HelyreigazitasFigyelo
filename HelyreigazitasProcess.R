library(data.table)

RawDataHelyreigazitasok <- fread("helyreigazitasok.csv", dec = ",")
RawDataCimkek <- fread("cimkek.csv", dec = ",")
RawDataCimkeHelyreigazitas <- fread("cimke_helyreigazitas.csv", dec = ",")
RawDataSajtotermekek <- fread("sajtotermekek.csv", dec = ",")
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