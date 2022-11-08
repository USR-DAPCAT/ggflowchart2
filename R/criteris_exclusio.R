#' @title                     criteris_exclusio
#' @description               criteris_exclusio
#' @param dt                  dt
#' @param taulavariables      taulavariables
#' @param criteris            criteris
#' @param missings            missings
#' @param ...                 Altres parametres
#' @export                    criteris_exclusio
criteris_exclusio<-function(dt=dades,
                            taulavariables="VARIABLES_R3b.xls",
                            criteris="exclusio1",
                            missings=T,
                            ...) {
  # NUMERO_52)
  
  #  APLICA CRITERIS D'EXCLUSIo A dades  -----------------------
  
  # Per defecte exclou registres que tenen missings en variables implicades
  # missings=F --> no elimina per criteri amb valors missings
  
  
  # dt=dt_matching
  # taulavariables=conductor
  # criteris="exc_pre"
  # missings=T
  
  ##  2. Eliminar els espais en blanc de les variables factors del data.frame
  dt<-dt %>%
    dplyr::mutate_if(is.factor,dplyr::funs(stringr::str_trim(.))) %>%
    dplyr::mutate_if(is.character,dplyr::funs(stringr::str_trim(.)))
  
  ##  Llegeix criteris de variables
  variables <- read_conductor(taulavariables,col_types = "text",...) %>% tidyr::as_tibble() %>% dplyr::select(camp,!!criteris)
  
  # Filtrar valors
  criteris_sym<-dplyr::sym(criteris)
  variables<-variables %>% dplyr::filter(!is.na(!!criteris_sym))
  # variables[is.na(variables)]<- 0
  
  # llista de caracters logics del filtre
  char_logics<-c(">",">=","<","<=","==","!=","is.na") %>% paste0(collapse = '|')
  
  ##  0. Filtro taula variables nomes variables implicades en el filtre i el genero
  maco<-variables %>%
    dplyr::filter_(paste0(criteris,"!=0")) %>% dplyr::select_("camp",criteris) %>%
    dplyr::transmute_("camp","crit_temp"=criteris) %>%
    # if criteri missing is.na()
    dplyr::mutate(crit_temp=dplyr::if_else(stringr::str_detect(crit_temp,"is.na"),paste0("is.na(",camp,")"),crit_temp)) %>%
    dplyr::mutate(camp=dplyr::if_else(stringr::str_detect(crit_temp,"is.na"),"",camp)) %>%
    # Si es texte sense igualtat --> la poso
    dplyr::mutate(crit_temp=dplyr::if_else(stringr::str_detect(crit_temp,char_logics),crit_temp,paste0("=='",crit_temp,"'")))
  
  # Genero la llista de filtres
  maco<-maco %>% tidyr::unite(filtres, c("camp", "crit_temp"),sep="", remove=F) %>%
    dplyr::mutate(filtres=paste0("(",filtres,")"))
  
  # Afegir valors valids per aplicar criteri (Si missings==F)
  if (missings==F) maco<-maco %>% dplyr::mutate(filtres=stringr::str_c("(", filtres, " & !is.na(",camp, "))"))
  
  # Concateno condicions amb un OR
  maco<-stringr::str_c(maco$filtres,collapse=" | ")
  
  ## 1. Genera filtre en base a columna exclusio1   popes
  popes<-stringr::str_c("!(",maco,")")
  
  ##  3. Aplicar filtre: popes a dt
  dt %>% dplyr::filter(eval(parse(text=popes)))
  
}

