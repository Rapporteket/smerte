
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
