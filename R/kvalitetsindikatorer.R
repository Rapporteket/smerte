
#' Kvalitetsindikator behandlertilsyn
#'
#' @description
#' Indikatoren regner ut andel pasienter som mottar tilsyn fra to eller flere
#' behandlergrupper.
#' Dette finnes ved å telle opp antall registrerte tilsyn for
#' Lege (**AntTilsLege**), Sykepleier (**AntTilsSykPleier**),
#' Fysioterapeut (**AntTilsFysioT**), Psykolog (**AntTilsPsyk**),
#' Sosionom (**AntTilsSosio**), og Konferert med lege (**AntTilsKonfLege**).
#'
#' Funksjonen teller opp antall registrerte tilsyn for overnevnte variabler
#' og returnerer `ki_krit_nevner` er `TRUE` eller `FALSE` avhengig av om pasienten er
#' tilsett av behandler og dermed skal inkluderes i indikator.
#' `ki_krit_teller` er `TRUE` hvis det er registrert to eller flere
#' tilsyn samtidig som  `ki_krit_nevner` er `TRUE`.
#'
#' Utdata kan videre mates inn i [rapwhale::aggreger_ki_prop()], gruppert etter
#' ønske for å beregne andel for aktuelle undergrupper.
#'
#' @param d datasett som inneholder de radene vi skal beregne indikator for.
#'
#' @returns Returnerer inndata, med ekstra kolonner `ki_krit_teller` og
#' `ki_krit_nevner`egnet for bruk med [rapwhale::aggreger_ki_prop()].
#' @export
#'
#' @examples
#' d = tibble::tibble(
#' AntTilsLege      = c(0L, 1L, 2L, 5L, NA_integer_, NA_integer_),
#' AntTilsSykPleier = c(0L, 0L, 0L, 0L, NA_integer_, 0L),
#' AntTilsFysioT    = c(0L, 0L, 0L, 0L, NA_integer_, 0L),
#' AntTilsPsyk      = c(0L, 0L, 0L, 0L, NA_integer_, 0L),
#' AntTilsSosio     = c(0L, 0L, 0L, 0L, NA_integer_, 0L),
#' AntTilsKonfLege  = c(0L, 0L, 0L, 0L, NA_integer_, 0L),
#' Tilsett          = c(1L, 1L, 1L, 1L, 4L, 1L))
#'
#' d_behtils = ki_behandlertilsyn(d)
ki_behandlertilsyn = function(d) {

  indikatorvariabler = c("AntTilsLege", "AntTilsSykPleier", "AntTilsFysioT",
                         "AntTilsPsyk", "AntTilsSosio", "AntTilsKonfLege",
                         "Tilsett")

  assert_that(all(has_name(d, indikatorvariabler)),
              msg = paste0("'",
                           kjed_ord(indikatorvariabler[!indikatorvariabler %in% names(d)],
                                    skiljeteikn = "', '",
                                    og = "' og '"),
                           "' må være med i inndata."))

  d |> mutate(
    across(all_of(indikatorvariabler[indikatorvariabler != "Tilsett"]), \(x) replace_na(x, replace = 0)),
    ki_krit_nevner = .data$Tilsett == 1,
    ki_krit_teller = .data$ki_krit_nevner &
      (rowSums(pick(all_of(indikatorvariabler[indikatorvariabler != "Tilsett"]))) >= 2)
    )
}


#' Kvalitetsindikator smerteendring
#'
#' @description
#' Indikatoren regner ut andelen pasienter som opplever nedgang i smertenivå fra
#' første til siste tilsyn.
#' Pasienten registrerer sterkeste og svakeste smerte i ro og bevegelse ved første
#' og siste tilsyn.
#' Vi bruker variablene `StSm12` og `StSm21` for
#' sterkeste smerte og `SvSm12` og `SvSm21` for svakeste smerte.
#' `12` viser til første tilsyn og `21` til siste tilsyn.
#'
#' For å oppfylle kravet for nevner må pasienten ha svart ut smertenivå ved både
#' første og siste tilsyn.
#' For å oppfylle kravet for teller må det være en nedgang fra første til siste tilsyn.
#'
#' @param d datasett som inneholder nødvendige variabler for å beregne smerteendring.
#' @param var Tekststreng for å indikere om det er endring i `sterkeste`
#' eller `svakeste` smerte vi skal hente ut. Aksepterte verdier er `sterkeste` eller
#' `svakeste`.
#'
#' @returns Returnerer inndata med to ekstra variabler `ki_krit_teller`
#' og `ki_krit_nevner` som indikerer om kriterier for indikator er oppfylt.
#'
#' Utdata kan videre mates inn i [rapwhale::aggreger_ki_prop()], gruppert etter
#' ønske for å beregne andel for aktuelle undergrupper.
#'
#' @export
#' @examples
#' d = tibble::tibble(
#' StSm12 = c(0L, 1L, 2L, 5L, NA_integer_, NA_integer_),
#' StSm21 = c(0L, 0L, 0L, 0L, NA_integer_, 0L),
#' SvSm12 = c(0L, 0L, 0L, 0L, NA_integer_, 0L),
#' SvSm21 = c(0L, 0L, 0L, 0L, NA_integer_, 0L)
#' )
#'
#' d_smerteendring_st = ki_smerteendring(d, "sterkeste")
#' d_smerteendrinig_sv = ki_smerteendring(d, "svakeste")
ki_smerteendring = function(d, var) {

  if(!var %in% c("sterkeste", "svakeste")) {
    stop("var må være enten 'sterkeste' eller 'svakeste'.")
  }

  ind_vars <- switch(
    var,
    sterkeste = c("StSm12", "StSm21"),
    svakeste = c("SvSm12", "SvSm21")
  )

  if (!all(ind_vars %in% names(d))) {
    stop(
      paste0("Variablene '", kjed_ord(ind_vars, skiljeteikn = "', '", og = "' og '"),
             "' må være i inndata for å beregne endring i ", var, " smerte. ")
    )
  }

  d |>
    mutate(
      ki_krit_nevner = !is.na(.data[[ind_vars[1]]]) & !is.na(.data[[ind_vars[2]]]),
      ki_krit_teller = .data$ki_krit_nevner & .data[[ind_vars[2]]] < .data[[ind_vars[1]]]
    )
}
