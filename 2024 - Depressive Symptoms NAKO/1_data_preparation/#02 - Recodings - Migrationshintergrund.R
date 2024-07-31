library(easystats)

# Ländercodes ----------------------

code_suedamerika <- c(210, 224, 227, 232, 241, 258, 282, 338, 339, 392, 396)

code_subsahara <- c(207, 209, 213, 221, 226, 230, 231, 240, 243, 244, 249, 250,
                    252, 256, 257, 274, 276, 279, 283, 284, 285, 292, 295, 300,
                    301, 304, 308, 309, 317, 319, 324, 325, 343, 347, 350, 354,
                    356, 357, 358, 362, 363, 369, 374, 377, 379, 389, 402, 403,
                    383)

code_nordafrika <- c(203, 205, 296, 306, 370, 385)

code_westasien <- c(386, 211, 212, 216, 251, 263, 267, 272, 278, 290, 294, 328,
                    330, 334, 351, 375, 397, 127, 201, 222, 271, 372 )

code_suedasien <- c(202, 217, 223, 261, 264, 303, 321, 333, 365, 373)

code_suedostasien <- c(228, 262, 273, 291, 302, 318, 332, 340, 359, 378, 400)

code_ostasien <- c(233, 270, 315, 327, 371)

code_zentralasien <- c(277, 280, 376, 387, 393)

code_nordamerika <- c(398, 275)

code_nordeuropa <- c(126, 103, 105, 106, 109, 266, 111, 112, 329, 120)

code_westeuropa <- c(116, 115, 107, 101, 286, 297, 113, 314, 353)

code_osteuropa <- c(117, 345, 390, 119, 102, 313, 121, 124, 125, 401, 381)

code_suedeuropa <- c(110, 355, 316, 108, 288, 225, 118, 123, 204, 206, 114, 310,
                     349, 122, 395, 287)

code_karibik <- c(208, 215, 218, 220, 235, 238, 239, 242, 253, 255, 259, 260,
                  269, 289, 311, 323, 336, 366, 367, 368, 382)

code_ozeanien <- c(214, 234, 281, 307, 312, 320, 322, 326, 335, 337, 546, 348,
                   380, 388, 394, 246)



value_labels <- c(
  `0` = "Unbekannt", `1` = "Südamerika", `2` = "Subsahara",
  `3` = "Nordafrika", `4` = "Westasien", `5` = "Südasien", `6` = "Südostasien",
  `7` = "Ostasien", `8` = "Zentralasien", `9` = "Nordamerika",
  `10` = "Nordeuropa", `11` = "Westeuropa", `12` = "Osteuropa",
  `13` = "Südeuropa", `14` = "Deutschland", `15` = "Zentralamerika/Karibik",
  `16` = "Ozeanien"
)




# Geburtsregion Vater ----------------------

nako <- data_modify(nako, geb_region_vater = recode_into(
  # in DE geboren
  d_se_bf3 == 1 ~ 14,
  # in Südamerika geboren
  d_se_bf3a %in% code_suedamerika ~ 1,
  # in Sub-Sahara Afrika geboren
  d_se_bf3a %in% code_subsahara ~ 2,
  # in Nordafrika geboren
  d_se_bf3a %in% code_nordafrika ~ 3,
  # in Westasien geboren
  d_se_bf3a %in% code_westasien ~ 4,
  # in Südasien geboren
  d_se_bf3a %in% code_suedasien ~ 5,
  # in Südostasien geboren
  d_se_bf3a %in% code_suedostasien ~ 6,
  # in Ostasien geboren
  d_se_bf3a %in% code_ostasien ~ 7,
  # in Zentralasien geboren
  d_se_bf3a %in% code_zentralasien ~ 8,
  # in Nordamerika geboren
  d_se_bf3a %in% code_nordamerika ~ 9,
  # in Norderuropa geboren
  d_se_bf3a %in% code_nordeuropa ~ 10,
  # in Westeuropa geboren
  d_se_bf3a %in% code_westeuropa ~ 11,
  # in Osteuropa geboren
  d_se_bf3a %in% code_osteuropa ~ 12,
  # in Südeuropa geboren
  d_se_bf3a %in% code_suedeuropa ~ 13,
  # in Zentralamerika und Karibik geboren, eigentlich auch in region = 9 rekodiert
  d_se_bf3a %in% code_karibik ~ 15,
  # in Ozeanien geboren, eigentlich auch in region = 6 rekodiert
  d_se_bf3a %in% code_ozeanien ~ 16,
  default = 0
))


nako$geb_region_vater[is.na(nako$d_se_bf3a) & (is.na(nako$d_se_bf3) | nako$d_se_bf3 != 1)] <- NA
nako$geb_region_vater <- assign_labels(nako$geb_region_vater, variable = "Geburtsregion", values = value_labels)




# Geburtsregion Mutter ----------------------

nako <- data_modify(nako, geb_region_mutter = recode_into(
  # in DE geboren
  d_se_bf6 == 1 ~ 14,
  # in Südamerika geboren
  d_se_bf6a %in% code_suedamerika ~ 1,
  # in Sub-Sahara Afrika geboren
  d_se_bf6a %in% code_subsahara ~ 2,
  # in Nordafrika geboren
  d_se_bf6a %in% code_nordafrika ~ 3,
  # in Westasien geboren
  d_se_bf6a %in% code_westasien ~ 4,
  # in Südasien geboren
  d_se_bf6a %in% code_suedasien ~ 5,
  # in Südostasien geboren
  d_se_bf6a %in% code_suedostasien ~ 6,
  # in Ostasien geboren
  d_se_bf6a %in% code_ostasien ~ 7,
  # in Zentralasien geboren
  d_se_bf6a %in% code_zentralasien ~ 8,
  # in Nordamerika geboren
  d_se_bf6a %in% code_nordamerika ~ 9,
  # in Norderuropa geboren
  d_se_bf6a %in% code_nordeuropa ~ 10,
  # in Westeuropa geboren
  d_se_bf6a %in% code_westeuropa ~ 11,
  # in Osteuropa geboren
  d_se_bf6a %in% code_osteuropa ~ 12,
  # in Südeuropa geboren
  d_se_bf6a %in% code_suedeuropa ~ 13,
  # in Zentralamerika und Karibik geboren, eigentlich auch in region = 9 rekodiert
  d_se_bf6a %in% code_karibik ~ 15,
  # in Ozeanien geboren, eigentlich auch in region = 6 rekodiert
  d_se_bf6a %in% code_ozeanien ~ 16,
  default = 0
))


nako$geb_region_mutter[is.na(nako$d_se_bf6a) & (is.na(nako$d_se_bf6) | nako$d_se_bf6 != 1)] <- NA
nako$geb_region_mutter <- assign_labels(nako$geb_region_mutter, variable = "Geburtsregion", values = value_labels)




# Geburtsregion Befragte Person ----------------------

nako <- data_modify(nako, geb_region = recode_into(
  # in DE geboren
  d_se_n1 == 1 ~ 14,
  # in Südamerika geboren
  d_se_n1a %in% code_suedamerika ~ 1,
  # in Sub-Sahara Afrika geboren
  d_se_n1a %in% code_subsahara ~ 2,
  # in Nordafrika geboren
  d_se_n1a %in% code_nordafrika ~ 3,
  # in Westasien geboren
  d_se_n1a %in% code_westasien ~ 4,
  # in Südasien geboren
  d_se_n1a %in% code_suedasien ~ 5,
  # in Südostasien geboren
  d_se_n1a %in% code_suedostasien ~ 6,
  # in Ostasien geboren
  d_se_n1a %in% code_ostasien ~ 7,
  # in Zentralasien geboren
  d_se_n1a %in% code_zentralasien ~ 8,
  # in Nordamerika geboren
  d_se_n1a %in% code_nordamerika ~ 9,
  # in Norderuropa geboren
  d_se_n1a %in% code_nordeuropa ~ 10,
  # in Westeuropa geboren
  d_se_n1a %in% code_westeuropa ~ 11,
  # in Osteuropa geboren
  d_se_n1a %in% code_osteuropa ~ 12,
  # in Südeuropa geboren
  d_se_n1a %in% code_suedeuropa ~ 13,
  # in Zentralamerika und Karibik geboren, eigentlich auch in region = 9 rekodiert
  d_se_n1a %in% code_karibik ~ 15,
  # in Ozeanien geboren, eigentlich auch in region = 6 rekodiert
  d_se_n1a %in% code_ozeanien ~ 16,
  default = 0
))

nako$geb_region[is.na(nako$d_se_n1a) & (is.na(nako$d_se_n1) | nako$d_se_n1 == 2)] <- NA
nako$geb_region <- assign_labels(nako$geb_region, variable = "Geburtsregion", values = value_labels)




# migrationshintergrund / generation ------------------

nako <- data_modify(nako, mig_status = recode_into(
  geb_region_vater == 14 & geb_region_mutter == 14 ~ 0,
  (geb_region_vater != 14 | geb_region_mutter != 14) & geb_region == 14 ~ 1,
  (geb_region_vater != 14 & geb_region_mutter != 14) & geb_region == 14 ~ 2,
  (geb_region_vater != 14 | geb_region_mutter != 14) & geb_region != 14 ~ 3,
  geb_region_vater != 14 & geb_region_mutter != 14 & geb_region != 14 ~ 4,
  default = as.numeric(NA)
))

nako$mig_status <- assign_labels(
  nako$mig_status,
  variable = "Migrationshintergrund",
  values = c(
    `0` = "Ohne MH",
    `1` = "2. Generation, ein Elternteil mit MH",
    `2` = "2. Generation, beide Elternteile mit MH",
    `3` = "1. Generation, ein Elternteil mit MH",
    `4` = "1. Generation, beide Elternteile mit MH"
  )
)



nako$geb_region_vater_num <- nako$geb_region_vater
nako$geb_region_vater <- to_factor(nako$geb_region_vater)

nako$geb_region_mutter_num <- nako$geb_region_mutter
nako$geb_region_mutter <- to_factor(nako$geb_region_mutter)

nako$geb_region_proband_num <- nako$geb_region
nako$geb_region_proband <- to_factor(nako$geb_region)

nako$mig_status_num <- nako$mig_status
nako$mig_status <- to_factor(nako$mig_status)

nako$jahre_in_de <- 2019 - nako$d_se_n3_j
nako$jahre_in_de <- assign_labels(nako$jahre_in_de, variable = "Aufenthaltsdauer in Deutschland")

# Rekrutierung war 2016-2021, d.h. es gibt auch Fälle, bei denen die Aufenthaltsdauer
# überschätzt wird, wenn diese 2016 teilgenommen haben. In diesem Fall ist die
# Aufenthaltsdauer größer als das Alter - diese Fälle nun korrigieren
fehlerhaft <- which(nako$jahre_in_de > nako$basis_age)
nako$jahre_in_de[fehlerhaft] <- nako$jahre_in_de[fehlerhaft] - (nako$jahre_in_de[fehlerhaft] - nako$basis_age[fehlerhaft] + 1)

nako$jahre_in_de_cat <- categorize(nako$jahre_in_de, "equal_range", range = 10, lowest = 0)
nako$jahre_in_de_cat <- assign_labels(
  nako$jahre_in_de_cat,
  values = c(
    `0` = "<10", `1` = "10-19", `2` = "20-29", `3` = "30-39",
    `4` = "40-49", `5` = "50-59", `6` = "60-69", `7` = "70+"
  ),
  variable = "Aufenthaltsdauer in Deutschland (gruppiert)"
)
nako$jahre_in_de_cat_f <- to_factor(nako$jahre_in_de_cat)

# speichern ------------------
# save(nako, nako_label, file = "Daten/nako.RData")
# sjlabelled::write_spss(nako, "Daten/nako.sav")

