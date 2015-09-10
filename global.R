#file.copy("~/Desktop/faodash_data/data_processed/bulk.Rdata", "./data", overwrite = TRUE)
load("./data/bulk.Rdata")
load("./data/FAOcountryProfile.RData")
#FAOcountryProfile <- read_csv(paste0(root.dir,"./input/data/FAOcountryProfile.csv"))

# Recode the Short Name Variables
## Abbreviate names
FAOcountryProfile[FAOcountryProfile[, "FAO_TABLE_NAME"] == "Latin America and the Caribbean"          & !is.na(FAOcountryProfile[, "FAO_TABLE_NAME"]), "FAO_TABLE_NAME"] <- "Latin America\nand the Caribbean"
FAOcountryProfile[FAOcountryProfile[, "FAO_TABLE_NAME"] == "Developed countries"                      & !is.na(FAOcountryProfile[, "FAO_TABLE_NAME"]), "FAO_TABLE_NAME"] <- "Developed\ncountries"
FAOcountryProfile[FAOcountryProfile[, "FAO_TABLE_NAME"] == "Developing countries"                     & !is.na(FAOcountryProfile[, "FAO_TABLE_NAME"]), "FAO_TABLE_NAME"] <- "Developing\ncountries"
# 
FAOcountryProfile[FAOcountryProfile[, "SHORT_NAME"] == "Saint Vincent and the Grenadines"             & !is.na(FAOcountryProfile[, "SHORT_NAME"]), "SHORT_NAME"] <- "Saint Vincent\nand the\nGrenadines"
FAOcountryProfile[FAOcountryProfile[, "SHORT_NAME"] == "Antigua and Barbuda"                          & !is.na(FAOcountryProfile[, "SHORT_NAME"]), "SHORT_NAME"] <- "Antigua and\nBarbuda"
FAOcountryProfile[FAOcountryProfile[, "SHORT_NAME"] == "Trinidad and Tobago"                          & !is.na(FAOcountryProfile[, "SHORT_NAME"]), "SHORT_NAME"] <- "Trinidad and\nTobago"
FAOcountryProfile[FAOcountryProfile[, "SHORT_NAME"] == "Republic of Moldova"                          & !is.na(FAOcountryProfile[, "SHORT_NAME"]), "SHORT_NAME"] <- "Republic of\nMoldova"
FAOcountryProfile[FAOcountryProfile[, "SHORT_NAME"] == "Saint Helena, Ascension and Tristan da Cunha" & !is.na(FAOcountryProfile[, "SHORT_NAME"]), "SHORT_NAME"] <- "Saint Helena"
FAOcountryProfile[FAOcountryProfile[, "SHORT_NAME"] == "Northern Mariana Islands"                     & !is.na(FAOcountryProfile[, "SHORT_NAME"]), "SHORT_NAME"] <- "N. Mariana\nIslands"
FAOcountryProfile[FAOcountryProfile[, "SHORT_NAME"] == "Wallis and Futuna Islands"                    & !is.na(FAOcountryProfile[, "SHORT_NAME"]), "SHORT_NAME"] <- "Wallis and\nFutuna Is."
FAOcountryProfile[FAOcountryProfile[, "SHORT_NAME"] == "United Arab Emirates"                         & !is.na(FAOcountryProfile[, "SHORT_NAME"]), "SHORT_NAME"] <- "United Arab\nEmirates"
FAOcountryProfile[FAOcountryProfile[, "SHORT_NAME"] == "Turks and Caicos Islands"                     & !is.na(FAOcountryProfile[, "SHORT_NAME"]), "SHORT_NAME"] <- "Turks and\nCaicos Is."
FAOcountryProfile[FAOcountryProfile[, "SHORT_NAME"] == "Central African Republic"                     & !is.na(FAOcountryProfile[, "SHORT_NAME"]), "SHORT_NAME"] <- "Central African\nRepublic"
FAOcountryProfile[FAOcountryProfile[, "SHORT_NAME"] == "Sao Tome and Principe"                        & !is.na(FAOcountryProfile[, "SHORT_NAME"]), "SHORT_NAME"] <- "Sao Tome and\nPrincipe"
FAOcountryProfile[FAOcountryProfile[, "SHORT_NAME"] == "United States of America"                     & !is.na(FAOcountryProfile[, "SHORT_NAME"]), "SHORT_NAME"] <- "United States\nof America"
FAOcountryProfile[FAOcountryProfile[, "SHORT_NAME"] == "Iran (Islamic Republic of)"                   & !is.na(FAOcountryProfile[, "SHORT_NAME"]), "SHORT_NAME"] <- "Iran\n(Islamic Republic of)"
FAOcountryProfile[FAOcountryProfile[, "SHORT_NAME"] == "Bosnia and Herzegovina"                       & !is.na(FAOcountryProfile[, "SHORT_NAME"]), "SHORT_NAME"] <- "Bosnia and\nHerzegovina"

# FAOcountryProfile["SHORT_NAME"][FAOcountryProfile[, "FAOST_CODE"] == 107]                                                                                        <- "Côte d'Ivoire"

FAOcountryProfile[FAOcountryProfile[, "SHORT_NAME"] == "Falkland Islands (Malvinas)"                  & !is.na(FAOcountryProfile[, "SHORT_NAME"]), "SHORT_NAME"] <- "Falkland Islands\n(Malvinas)"
FAOcountryProfile[FAOcountryProfile[, "SHORT_NAME"] == "Papua New Guinea"                             & !is.na(FAOcountryProfile[, "SHORT_NAME"]), "SHORT_NAME"] <- "Papua New\nGuinea"
FAOcountryProfile[FAOcountryProfile[, "SHORT_NAME"] == "American Samoa"                               & !is.na(FAOcountryProfile[, "SHORT_NAME"]), "SHORT_NAME"] <- "American\nSamoa"
FAOcountryProfile[FAOcountryProfile[, "SHORT_NAME"] == "Western Sahara"                               & !is.na(FAOcountryProfile[, "SHORT_NAME"]), "SHORT_NAME"] <- "Western\nSahara"

FAOcountryProfile[FAOcountryProfile[, "SHORT_NAME"] == "Union of Soviet Socialist Republic"           & !is.na(FAOcountryProfile[, "SHORT_NAME"]), "SHORT_NAME"] <- "Soviet Union"
FAOcountryProfile[FAOcountryProfile[, "SHORT_NAME"] == "Micronesia (Federated States of)"             & !is.na(FAOcountryProfile[, "SHORT_NAME"]), "SHORT_NAME"] <- "Micronesia"
FAOcountryProfile[FAOcountryProfile[, "SHORT_NAME"] == "Svalbard and Jan Mayen Islands"               & !is.na(FAOcountryProfile[, "SHORT_NAME"]), "SHORT_NAME"] <- "Svalbard"
FAOcountryProfile[FAOcountryProfile[, "SHORT_NAME"] == "Occupied Palestinian Territory"               & !is.na(FAOcountryProfile[, "SHORT_NAME"]), "SHORT_NAME"] <- "Occupied\nPalestinian Territory"
FAOcountryProfile[FAOcountryProfile[, "SHORT_NAME"] == "United States Virgin Islands"                 & !is.na(FAOcountryProfile[, "SHORT_NAME"]), "SHORT_NAME"] <- "U.S. Virgin Islands"
FAOcountryProfile[FAOcountryProfile[, "SHORT_NAME"] == "Saint Pierre and Miquelon"                    & !is.na(FAOcountryProfile[, "SHORT_NAME"]), "SHORT_NAME"] <- "Saint Pierre\nand Miquelon"
FAOcountryProfile[FAOcountryProfile[, "SHORT_NAME"] == "Serbia and Montenegro"                        & !is.na(FAOcountryProfile[, "SHORT_NAME"]), "SHORT_NAME"] <- "Serbia and\nMontenegro"
FAOcountryProfile[FAOcountryProfile[, "SHORT_NAME"] == "Saint Kitts and Nevis"                        & !is.na(FAOcountryProfile[, "SHORT_NAME"]), "SHORT_NAME"] <- "Saint Kitts\nand Nevis"
FAOcountryProfile[FAOcountryProfile[, "SHORT_NAME"] == "Netherlands Antilles"                         & !is.na(FAOcountryProfile[, "SHORT_NAME"]), "SHORT_NAME"] <- "Netherlands\nAntilles"
# #FAOcountryProfile[FAOcountryProfile[, "SHORT_NAME"] == ""                                            & !is.na(FAOcountryProfile[, "SHORT_NAME"]), "SHORT_NAME"] <- ""


# load("./data/SYB.RData")
# syb.df <- SYB.df

# fsi <- read.csv("~/btsync/faosync/pocketbooks/GSPB15/database/Data/Raw/FSI2015_DisseminationDataset.csv",stringsAsFactors=FALSE)
# save(fsi, file="./data/fsi.RData")
# load("./data/fsi.RData")

#
#
# # # Doanload data from FAOSTAT
# library(FAOSTAT)
# library(dplyr)
# metdat <- data.frame(domainCode  = c("QC","QC","OA"),
#                      elementCode = c(5510,5510,511),
#                      itemCode    = c(486,1717,3010),
#                      start_year = c(1995,1995,1995),
#                      end_year = c(2012,2012,2012),
#                      indicator_name = c("banana.production",
#                                         "cereal.production",
#                                         "total.population"),
#                      stringsAsFactors = FALSE
# )
# # # Population data
# i <- 3
# dat <- getFAOtoSYB(domainCode = metdat[i,"domainCode"],
#                    elementCode = metdat[i,"elementCode"],
#                    itemCode = metdat[i,"itemCode"],
#                    yearRange = metdat[i,"start_year"]:metdat[i,"end_year"])
# data <- dat[["entity"]]
# names(data) <- c("FAOST_CODE","Year",metdat[i,"indicator_name"])
# #
# for (i in 1:2){
#   dat <- getFAOtoSYB(domainCode = metdat[i,"domainCode"],
#                      elementCode = metdat[i,"elementCode"],
#                      itemCode = metdat[i,"itemCode"],
#                      yearRange = metdat[i,"start_year"]:metdat[i,"end_year"])
#   dat <- dat[["entity"]]
#   names(dat) <- c("FAOST_CODE","Year",metdat[i,"indicator_name"])
#   data <- merge(data,dat,by=c("FAOST_CODE","Year"))
# }
#
# library(countrycode)
# data$Country <- countrycode(data$FAOST_CODE, "fao", "country.name")
# data <- data[!is.na(data$banana.h3("Buttons"),
#                     actionButton("action", label = "Action"),
#                     br(),
#                     br(),
#                     submitButton("Submit"))production),]
# data <- data[!is.na(data$cereal.production),]
# data <- data[!is.na(data$total.population),]
# # Manual adds
# data$Country[data$FAOST_CODE == 41] <- "China"
#
# # Lets take the region from gisfao shapefiles
# library(gisfao)
# reg <- fao_world@data
# data2 <- reg[c("FAO_CODE","RAF","LAC","RAP","REU","RNE","ADM0_NAME")]
# data2$Region[data2$RAF == TRUE] <- "Africa"
# data2$Region[data2$RNE == TRUE] <- "Near East and North Africa"
# data2$Region[data2$LAC == TRUE] <- "Latin America and the Caribbean"
# data2$Region[data2$RAP == TRUE] <- "Asia and the Pacific"
# data2$Region[data2$REU == TRUE] <- "Europe and Central Asia"
# data2$Region[is.na(data2$Region)] <- "Other regions"
#
# names(data2)[names(data2)=="FAO_CODE"] <- "FAOST_CODE"
# names(data2)[names(data2)=="ADM0_NAME"] <- "Country"
#
# reg <- data2
#
# save(reg, file="reg.RData")
# load("reg.RData")
#
# # data$Region <- as.factor(data$Region)
# # data$Country <- as.factor(data$Country)
# # data$Year <- as.numeric(data$Year)
#
#
# # data3 <- readRDS("healthexp.Rds")
# # data3$Region <- as.factor(data3$Region)
# # h(data3)
#
# library(FAOSTAT)
# #save(FAOmetaTable, file="FAOmetaTable.RData")
# load("./data/FAOmetaTable.RData")
# #save(FAOcountryProfile, file="FAOcountryProfile.RData")
# load("./data/FAOcountryProfile.RData")
# #
# #
# # str(FAOmetaTable)
# #
# groupTable <- FAOmetaTable[[1]]
# domainTable <- FAOmetaTable[[2]]
# itemTable <- FAOmetaTable[[3]]
# itemAggTable <- FAOmetaTable[[4]]
# elementTable <- FAOmetaTable[[5]]
#
# ### getFAO
#
# getFAO = function(name = NULL, domainCode = "RL", elementCode = 5110,
#                   itemCode = 6621, query, printURL = FALSE, productionDB = FALSE,
#                   useCHMT = TRUE, outputFormat = "wide", returnNames = FALSE,
#                   returnFlags = FALSE, yearRange = NULL, countrySet = NULL){
#
#   ## Year range
#   if (!is.null(yearRange)) {
#     if (!is.numeric(yearRange)) {
#       stop("Please, provide a numeric vector for the year range.")
#     } else {
#       yearRange = paste(yearRange, collapse = ":")
#     }
#   }
#   ## Country set
#   if (!is.null(countrySet)) {
#     if (!is.numeric(countrySet)) {
#       stop("Please, provide a numeric vector for the year range.")
#     } else {
#       countrySet = paste(countrySet, collapse = ":")
#     }
#   }
#   ## Query
#   if(!missing(query)){
#     if(NROW(query) > 1)
#       stop("Use 'getFAOtoSYB' for batch download")
#     domainCode = query$domainCode
#     itemCode = query$itemCode
#     elementCode = query$elementCode
#     if(is.null(query$name)){
#       name = with(query, paste(domainCode, itemCode, elementCode, sep = "_"))
#     } else {
#       name = query$name
#     }
#   }
#   ## Name
#   if(is.null(name))
#     name = paste(domainCode, itemCode, elementCode, sep = "_")
#
#   if(productionDB){
#     ## Base
#     #         base = "http://ldvapp07.fao.org:8030/wds/api?"
#     #         base = "http://lprapp16.fao.org:4012/wds/api?"
#     #         base = "http://lprapp16.fao.org/wds/api?"
#     base = "http://ldvapp07.fao.org:8032/wds/api?"
#     ## Database
#     database = "db=faostatproddiss&"
#     ## Selection
#     selection = "select=D.AreaCode[FAOST_CODE],D.Year[Year],D.Value[Value]"
#     from = "&from=data[D],element[E]&"
#     condition =
#       paste0("where=D.elementcode(", elementCode, "),D.itemcode(",
#              itemCode, "),D.domaincode('", domainCode, "')")
#     if (!is.null(yearRange)) {
#       condition = paste0(condition, ",D.year(", yearRange, ")")
#     }
#     if (!is.null(countrySet)) {
#       condition = paste0(condition, ",A.AreaCode(", countrySet, ")")
#     }
#     join = ",JOIN(D.elementcode:E.elementcode)&orderby=E.elementnamee,D.year"
#   } else {
#     base = c("http://fenix.fao.org/wds/api?", "http://faostat3.fao.org/wds/api?", "http://fenixapps.fao.org/wds/api?")
#     database = "db=faostat2&"
#     selection = "select=A.AreaCode[FAOST_CODE],D.year[Year],D.value[Value]"
#     from = "&from=data[D],element[E],item[I],area[A]&"
#     condition = paste0("where=D.elementcode(", elementCode, "),D.itemcode(",
#                        itemCode, "),D.domaincode('", domainCode, "')")
#     if (!is.null(yearRange)) {
#       condition = paste0(condition, ",D.year(", yearRange, ")")
#     }
#     if (!is.null(countrySet)) {
#       condition = paste0(condition, ",A.AreaCode(", countrySet, ")")
#     }
#     join = ",JOIN(D.elementcode:E.elementcode),JOIN(D.itemcode:I.itemcode),JOIN(D.areacode:A.areacode)&orderby=E.elementnamee,D.year"
#   }
#   ## Flags
#   if(returnFlags){
#     outputFormat = "long"
#     selection = paste0(selection, ",D.Flag[Flags]")
#   }
#   ## Names
#   if(returnNames)
#     selection = paste0(selection, "A.AreaNameE[AreaName],E.elementnamee[ElementName],I.itemnamee[ItemName]")
#   ## Call
#   out = "out=csv&"
#   url = paste0(base, out, database, selection, from, condition, join)
#   if(printURL)
#     print(url)
#
#   ## Allowing multiple server if any failed.
#   for(i in 1:length(url)){
#     faoData = suppressWarnings(try(read.csv(file = url[i],
#                                             stringsAsFactors = FALSE), silent = TRUE))
#     if(!inherits(faoData, "try-error"))
#       break
#   }
#   faoData$FAOST_CODE = as.integer(faoData$FAOST_CODE)
#   faoData$Year = as.integer(faoData$Year)
#   ## CHMT
#   if(useCHMT)
#     faoData = CHMT(var = "Value", data = faoData, year = "Year")
#   ## Output format
#   if(outputFormat == "long" & NROW(faoData) != 0){
#     faoData$domainCode = domainCode
#     faoData$itemCode = itemCode
#     faoData$elementCode = elementCode
#     faoData$name = name
#     faoData$Value <- as.numeric(gsub("n.a.", "", faoData$Value))
#   } else if(outputFormat == "wide"){
#     colnames(faoData)[colnames(faoData) == "Value"] = name
#     faoData[, name] <- as.numeric(gsub("n.a.", "", faoData[, name]))
#   }
#   faoData
# }
#
# ### getFAOtoSYB
#
# getFAOtoSYB = function(name = NULL, domainCode = "RL",
#                        elementCode = 5110, itemCode = 6621, query, printURL = FALSE,
#                        productionDB = FALSE, useCHMT = TRUE, yearRange = NULL, countrySet = NULL,
#                        outputFormat = c("wide", "long"), returnFlags = FALSE){
#   outputFormat = match.arg(outputFormat)
#   if(returnFlags)
#     outputFormat = "long"
#
#   if(!missing(query)){
#     domainCode = query$domainCode
#     itemCode = query$itemCode
#     elementCode = query$elementCode
#     if(is.null(query$name)){
#       name = with(query, paste(domainCode, itemCode, elementCode, sep = "_"))
#     } else {
#       name = query$name
#     }
#   }
#
#   if(is.null(name))
#     name = paste(domainCode, itemCode, elementCode, sep = "_")
#   n = length(name)
#   if(any(length(domainCode) != n, length(elementCode) != n,
#          length(itemCode) != n))
#     stop("length of inputs are not all the same, check the number of names")
#
#   faoData = data.frame(FAOST_CODE = integer(),
#                        Year = integer(), stringsAsFactors = FALSE)
#   results = data.frame(Name = name, Success = logical(length(name)),
#                        Reason = character(length(name)),
#                        Time = as.POSIXct(rep(NA, length(name))),
#                        stringsAsFactors = FALSE)
#   printLab(paste("FAOSTAT Data Download (", n, " in Total)", sep = ""))
#
#   i = 1
#   retry = 1
#   while(i <= n){
#     if(retry == 1)
#       cat(paste("(", i, "): Downloading variable ", name[i], " ... ",
#                 sep = ""))
#     if(any(is.na(domainCode[i]), is.na(elementCode[i]), is.na(itemCode)[i])){
#       cat("FAIL\n\t Error: One of Domain, Element or Item code is missing\n")
#       results[i, "Success"] = FALSE
#       results[i, "Reason"] = "One of Domain, Element or Item code is missing"
#     } else {
#       tmp = try(getFAO(name = name[i],
#                        domainCode = domainCode[i],
#                        elementCode = elementCode[i],
#                        itemCode = itemCode[i], printURL = printURL,
#                        productionDB = productionDB,
#                        useCHMT = useCHMT, outputFormat = outputFormat,
#                        returnFlags = returnFlags,
#                        yearRange = yearRange,
#                        countrySet = countrySet))
#       if(!inherits(tmp, "try-error")){
#         ## This was to account sometimes the download is successful, yet
#         ## the data frame is empty
#         if(NROW(tmp) != 0){
#           cat("OK\n")
#           results[i, "Success"] = TRUE
#           results[i, "Reason"] = "Download Successful"
#           results[i, "Time"] = Sys.time()
#           if(outputFormat == "wide"){
#             faoData = merge(x = faoData, y = tmp, all = TRUE,
#                             by = c("FAOST_CODE", "Year"))
#           } else if(outputFormat == "long"){
#             faoData = rbind(faoData, tmp)
#           }
#           i = i + 1
#           retry = 1
#         } else {
#           tmp = c("The specified query has no data, consult FAOSTAT")
#           cat(paste(tmp, "\n"))
#           class(tmp) = "try-error"
#           attr(tmp, "condition") =
#             list(message = tmp, call = NULL)
#           i = i + 1
#           retry = 1
#         }
#       } else {
#         if(retry <=50){
#           print(retry)
#           retry = retry + 1
#         } else {
#           cat("Download fail after 50 tries\n")
#           results[i, "Success"] = FALSE
#           results[i, "Reason"] = attr(tmp, "condition")$message
#           i = i + 1
#           retry = 1
#         }
#       }
#     }
#   }
#   entity.df = arrange(with(faoData, faoData[FAOST_CODE %in%
#                                               FAOcountryProfile[, "FAOST_CODE"], ]), FAOST_CODE, Year)
#   region.df = arrange(with(faoData, faoData[!(FAOST_CODE %in%
#                                                 FAOcountryProfile[, "FAOST_CODE"]), ]), FAOST_CODE, Year)
#   cat(paste("\n Number of variables successfully downloaded: ",
#             sum(results$Success), " out of ", NROW(results), "\n\n", sep = ""))
#   list(entity = entity.df, aggregates = region.df, results = results)
# }
#
# ### printLab
#
# printLab = function(label, span = FALSE, width = getOption("width")){
#   nc = nchar(label)
#   sides = (width - nc)/2 - 3
#   if(span){
#     pre = paste(c("\n\n", rep("-", width), "\n"), collapse = "")
#     post = paste(c("\n", rep("-", width), "\n\n"), collapse = "")
#   } else {
#     pre = paste(c("\n\n", rep(" ", sides), rep("-", nc + 6),
#                   rep(" ", sides), "\n"), collapse = "")
#     post = paste(c("\n", rep(" ", sides), rep("-", nc + 6),
#                    rep(" ", sides), "\n\n"), collapse = "")
#   }
#   sandwich = paste(c(rep(" ", sides), "** ", label, " **",
#                      rep(" ", sides)), collapse = "")
#   cat(paste(pre, sandwich, post, sep = ""))
# }
#
#
