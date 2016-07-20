head(addresses_df$address, n = 50)


grepl("(GIR 0AA)|((([A-Z-[QVX]][0-9][0-9]?)|(([A-Z-[QVX]][A-Z-[IJZ]][0-9][0-9]?)|(([A-Z-[QVX]][0-9][A-HJKPSTUW])|([A-Z-[QVX]][A-Z-[IJZ]][0-9][ABEHMNPRVWXY])))) [0-9][A-Z-[CIKMOV]]{2})", addresses_df$address)


### Postcode Checker from http://codereview.stackexchange.com/a/117817

library(stringr)

postcode_vector <- str_match(pattern = '((GIR 0AA)|((([A-PR-UWYZ][A-HK-Y]?[0-9][0-9]?)|(([A-PR-UWYZ][0-9][A-HJKSTUW])|([A-PR-UWYZ][A-HK-Y][0-9][ABEHMNPRV-Y]))) [0-9][ABD-HJLNP-UW-Z]{2}))',string = addresses_df$address)
postcode_vector <- postcode_vector[!is.na(postcode_vector)]

find_long_lat(postcode_vector[1], "33")
