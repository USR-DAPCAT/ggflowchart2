#' @title                     criteris_exclusio_taula
#' @description               criteris_exclusio_taula
#' @param dt                  dt
#' @param taulavariables      taulavariables
#' @param criteris            criteris
#' @param ordre               ordre
#' @param ...                 Altres parametres
#' @export                    criteris_exclusio_taula
criteris_exclusio_taula<-function(dt=dades,
                                  taulavariables=here::here("Conductor_CANA.xlsx"),
                                  criteris="exclusio",
                                  ordre=NA,
                                  ...) {
  # NUMERO_53)
  
  ####  Funcio que retorna una taula les N's aplicant els criteris d'exclusio i la N cada vegada que s'aplica un criteri de manera sequencial
  #### dades, conductor i camp on tenim els criteris, i si es vol un camp amb l'ordre
  
  
  # dt=dades
  # taulavariables=here::here("Conductor_CANA.xlsx")
  # criteris="exclusio1"
  # ordre="exc_ordre"
  
  # extrec variables i criteris del conductor
  if (!is.na(ordre)) {
    
    dt_criteris<-vec_excl<-read_conductor(taulavariables,...) %>%
      dplyr::select(camp,criteris, ordre) %>% dplyr::arrange(!!dplyr::sym(ordre)) %>% dplyr::filter (!is.na(!!dplyr::sym(criteris)))} else {
        
        dt_criteris<-vec_excl<-read_conductor(taulavariables,...) %>%
          dplyr::select(camp,criteris) %>% dplyr::filter (!is.na(!!dplyr::sym(criteris)))
        
      }
  
  # Extrec vector d'exclusions
  vec_excl<-dt_criteris %>% dplyr::mutate(criteri=paste0(camp,!!dplyr::sym(criteris))) %>% dplyr:: pull(criteri)
  
  # Genero taula amb n per cada exclusio
  dt_temp<-vec_excl %>%
    purrr::map(~dt %>% dplyr::filter(eval(parse(text = .x))))%>%
    purrr::map_df(~dplyr::count(.x),.id="Criteri") %>% dplyr::transmute(N_excluded=dplyr::n)
  
  # Genero taula de N's despres d'aplicar exclusions sequencials
  dt_temp2<-seq(1:length(vec_excl)) %>%
    purrr::map(~paste0("!",vec_excl[1:.x],collapse = " & ")) %>%
    purrr::map(~dt %>% dplyr::filter(eval(parse(text = .x)))) %>%
    purrr::map_df(~dplyr::count(.x),.id="Criteri") %>% dplyr::transmute(N_remain=dplyr::n)
  
  dt_criteris<-dt_criteris %>% dplyr::select(camp,!!criteris) %>% dplyr::bind_cols(dt_temp) %>%dplyr:: bind_cols(dt_temp2)
  
  # Afegeixo N inicial
  tibble::tibble(camp="",N_excluded=0,N_remain=dt %>% dplyr::count() %>% as.numeric()) %>%dplyr:: bind_rows(dt_criteris)
  
}


