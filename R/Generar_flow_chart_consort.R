#' @title                Dibuixa un flow_chart_ggconsort. 
#' @description          Dibuixa un flow_chart_ggconsort a partir  llista de n criteris exclusio
#' @param dt             Dataframe tibble
#' @param exclusions     Vector de n strings amb la definicio del criteris:exclusions
#' @param grups          String referent al grup de dues categories (Opcional)
#' @param sequencial     Logic TRUE FALSE Exclusions sequencials o No
#' @param lab_start      String referida a la etiqueta de l acaixa inici (default -Assessed for eligibility- )
#' @param lab_random     String referida a la etiqueta de caixa post exclusions (default "Analyzed sample" )
#' @param lab_exclusions Vector etiquetes referent a les N exclusions (Opcional)
#' @importFrom           dplyr "%>%"
#' @export               Flow_chart_Consort
#' @examples
#' 
#' k<-Flow_chart_Consort(dt=ggconsort::trial_data,
#'                    exclusions=c("declined","prior_chemo","bone_mets"),
#'                    sequencial=TRUE,
#'                    grups=NA,
#'                    lab_start="Assessed for eligibility",
#'                    lab_random="Analyzed sample",
#'                    lab_exclusions=NULL)
#' k                    
Flow_chart_Consort<-function(dt,
                             exclusions=c("declined","prior_chemo","bone_mets"),
                             sequencial=TRUE,
                             grups=NA,
                             lab_start="Assessed for eligibility",
                             lab_random="Analyzed sample",
                             lab_exclusions=NULL)
{
  
  # Testing
  
  # dt=dt_nova
  # exclusions= exclusions
  # lab_exclusions=variables$labels
  # grups = grups
  # sequencial=sequencial
  # lab_start="Assessed for eligibility"
  # lab_random="Randomized"
  # lab_exclusions=NULL
  #
  # actualitzacio::
  # devtools::install_github("jrealgatius/ggconsort")
  
  # Dicotomitzar
  subvector_a_canviar<-exclusions[exclusions %in% (dt %>% names())] %>% paste0("==1")
  exclusions[exclusions %in% (dt %>% names())]<-subvector_a_canviar
  dt<-generar_dummies_exclusions(dt=dt, criteris=exclusions)
  
  # Num exclusions
  N_exc<-length(exclusions)
  noms_exclusions<-exclusions
  exclusions<-paste0("exclusio",c(1:N_exc))
  
  # Selecciono camps necessaris de dt (dades)
  if (is.na(grups))
  {dt<-dt %>%
    dplyr::select(exclusions) %>% dplyr::mutate(grup="Overall",idp = dplyr::row_number())
  grup<-"Overall"} else
  {dt<-dt %>% dplyr::select(exclusions,grup=grups) %>% dplyr::mutate(idp = dplyr::row_number())}
  
  # capturar etiquetes
  if (is.null(lab_exclusions)) labels_exclusions<-noms_exclusions else labels_exclusions<-lab_exclusions
  
  label_grup<-grups
  levels_grup<-dt %>% dplyr::select(grup) %>% stats::na.omit() %>% dplyr::distinct() %>% dplyr::pull(grup)
  # Canvi de categories a grup numerats en character
  dt<-dt %>% dplyr::mutate(grup=as.factor(grup) %>% as.numeric(),
                           grup=dplyr::if_else(!is.na(grup),as.character(paste0("grup",grup)),NA_character_))
  
  # Generar dataframes de filtre
  
  # Inclusions sequencials
  # Genero fitxers inclusions sequencials
  
  dtlist_incl<-seq(1:N_exc) %>%
    purrr::map(~paste0("!",exclusions[1:.x],collapse = " & ")) %>%
    purrr::map(~dt %>% dplyr::filter(eval(parse(text = .x)))) %>%
    purrr::set_names(paste0("included",1:N_exc))
  
  # Inclusio final
  dt_inclusio_final<-dtlist_incl[N_exc] %>% purrr::set_names("Included_final")
  
  # Generar fitxers exclusions sequencials
  dt_excluded_totals = dplyr::anti_join(dt, dt_inclusio_final[[1]], by = "idp")
  dt_exc1 = dplyr::anti_join(dt, dtlist_incl[[1]], by = "idp")
  # si hi ha una unica exclusio dtlist_exclusions<-NULL
  
  if (N_exc>1) {
    if (sequencial) {
      dtlist_exclusions<-
        purrr::map2(dtlist_incl[1:(N_exc-1)],dtlist_incl[-1],~dplyr::anti_join(.x,.y,by = "idp")) %>%
        purrr::set_names(paste0("Excluded",c(2:N_exc)))} else {
          
          dtlist_exclusions<-c(2:N_exc) %>%
            purrr::map(~paste0(exclusions[.x],"==1")) %>%
            purrr::map(~dt %>% dplyr::filter(eval(parse(text = .x)))) %>%
            purrr::set_names(paste0("Excluded",2:N_exc))
        }} else {dtlist_exclusions<-NULL}
  
  dtlist_exclusions<-append(list(Excluded1=dt_exc1),dtlist_exclusions)
  # Inclusions finals per grups
  # grup="FF.HTA"
  dt_inclusions_grups<-
    dtlist_incl[[N_exc]] %>% base::split(.[["grup"]])
  
  # Elimino els NA s
  # dt_inclusions_grups$grupNA<-NULL
  
  # Fusionar llistats de de inclosos + exclosos
  llistat_arguments<-
    c(dt_inclusio_final,dtlist_incl,dt_inclusions_grups,
      list(Excluded_total=dt_excluded_totals),
      dtlist_exclusions)
  
  arglist = append(list(ggconsort::cohort_start(dt,lab_start)),
                   llistat_arguments)
  
  # Generar les cohorts via funcio
  dades_cohorts<-
    do.call(ggconsort::cohort_define,
            arglist)
  
  # Provide text labels for cohorts 
  llistat_noms<-dades_cohorts$data %>% names()
  
  llistat_labels<- c(lab_start,lab_random,
                     paste0("Included",c(1:N_exc)),
                     levels_grup,"Excluded",labels_exclusions)
  
  
  
  for (i in 1:length(llistat_noms)) {
    dades_cohorts$labels[llistat_noms[i]]<-llistat_labels[i]}
  
  study_cohorts<-dades_cohorts
  
  
  # Generar caixa d exclusions
  noms_exc<-paste0("Excluded",1:N_exc)
  
  caixa_exc<-noms_exc %>%
    purrr::map_chr(~paste0('• {ggconsort::cohort_count_adorn(study_cohorts, ', .x,')}<br>')) %>%
    glue::glue_collapse()
  caixa_exclusions<-paste0(
    "{ggconsort::cohort_count_adorn(study_cohorts, Excluded_total)}<br>",
    caixa_exc)
  
  
  study_consort <- study_cohorts %>%
    ggconsort::consort_box_add(
      "full", 0, 50, ggconsort::cohort_count_adorn(study_cohorts, .full)
    ) %>%
    ggconsort::consort_box_add(
      lab_random, 0, 30, ggconsort::cohort_count_adorn(study_cohorts,Included_final)
    ) %>%
    
    
    ggconsort::consort_box_add(
      "exclusions", 10, 40, glue::glue(caixa_exclusions)
    ) %>%
    
    
    ggconsort::consort_arrow_add(
      end = "exclusions", end_side = "left", start_x = 0, start_y = 40
    ) %>%
    ggconsort::consort_arrow_add(
      "full", "bottom", lab_random, "top"
    )
  
  # En cas de By grups
  if (!is.na(grups)) {
    
    # By grups
    study_consort <- study_consort %>%
      
      ggconsort::consort_box_add(
        "arm_a", -30, 10, ggconsort::cohort_count_adorn(study_cohorts, grup1)
      ) %>%
      
      ggconsort::consort_box_add(
        "arm_b", 30, 10, ggconsort::cohort_count_adorn(study_cohorts, grup2)
      )
    
    study_consort<- study_consort %>%
      ggconsort::consort_arrow_add(
        start_x = 0, start_y = 30, end_x = 0, end_y = 20,
      ) %>%
      ggconsort::consort_line_add(
        start_x = -30, start_y = 20, end_x = 30, end_y = 20,
      )  %>%
      ggconsort::consort_arrow_add(
        end = "arm_a", end_side = "top", start_x = -30, start_y = 20
      ) %>%
      ggconsort::consort_arrow_add(
        end = "arm_b", end_side = "top", start_x = 30, start_y = 20)
    
  }
  
  ## Fer_ho maco
  
  
  if (!is.na(grups)) {
    
    study_consort %>%
      ggplot2::ggplot() +
      ggconsort::geom_consort() +
      ggconsort::theme_consort(margin_h = 8, margin_v = 1) +
      
      
      ggtext::geom_richtext(
        ggplot2::aes(x = 0, y = 10, label = "Allocation"),
        fill = "#9bc0fc") } else
        {
          study_consort %>%
            ggplot2::ggplot() +
            ggconsort::geom_consort() + ggplot2::xlim(-10,20) +
            ggconsort::theme_consort(margin_h = 10, margin_v = 1)
        }
}


#' @title                Dibuixa un flow_chart_ggconsort 
#' @description          Dibuixa un flow_chart_ggconsort a partir d una llista de n criteris d exclusio que estan en un fitxer extern (Tipo excel)
#' @param dt             Dataframe tibble
#' @param taulavariables String referent a path o tibble on hi ha una columna consten les exclusions: "conductor_cars.xls"
#' @param camp           String referent al nom de la columna de les variables implicades (Inecesari)
#' @param criteris       String referent a la columna on consten les exclusions
#' @param grups          String referent al grup de dues categories
#' @param missings       Logic TRUE FALSE (De moment no operatiu)
#' @param sequencial     Logic TRUE FALSE.Exclusions sequencials o No
#' @param labels         String nom del camp on hi ha les etiquetes de les exclusions (Optatiu)
#' @param lab_start      String referida a la etiqueta de l acaixa d inici (default "Assessed for eligibility" )
#' @param lab_random     String referida a la etiqueta de caixa post exclusions (default "Analyzed sample" )
#' @param ...            Altres parametres
#' @importFrom           dplyr "%>%"
#' @export               Generar_flow_chart_consort
#' 
#' @examples
#' KK<-Generar_flow_chart_consort(
#' dt=iris,
#' taulavariables=conductor_cars,
#' camp="camp",
#' criteris="exclusio",
#' grups=NA,
#' missings=F,
#' sequencial=T,
#' labels="descripcio",
#' lab_start="Assessed for eligibility",
#' lab_random="Analyzed sample")
#' 
#' KK
Generar_flow_chart_consort<-function(dt=iris,
                                     taulavariables="conductor_cars.xls",
                                     camp="camp",
                                     criteris="exclusio",
                                     grups=NA,
                                     missings=F,
                                     sequencial=T,
                                     labels=NULL,
                                     lab_start="Assessed for eligibility",
                                     lab_random="Analyzed sample",
                                     ...)
{

 
  # dt=iris
  # taulavariables=conductor_cars
  # camp="camp"
  # criteris="exclusio"
  # grups=NA
  # missings=F
  # sequencial=T
  # labels="descripcio"
  # lab_start="Assessed for eligibility"
  # lab_random="Analyzed sample"
 

  # 1. Obrir conductor d exclusions del conductor

  ##  Llegeixo criteris de variables i selecciono variables amb filtres
  variables <- read_conductor(taulavariables,col_types = "text",...)  %>%
    # variables <- read_conductor(taulavariables,col_types = "text")  %>%
    # variables <- read_conductor(taulavariables,col_types = "text",sheet="criteris_exclusio")%>%
    dplyr::select(camp=!!camp,criteris=!!criteris, labels=!!labels) %>%
    # Filtrar valors
    dplyr::filter(!is.na(criteris)) %>% 
    dplyr::filter(criteris!="") 

  # Parar si no hi ha criteris d exclusio
  num_excl<-variables$criteris %>% length()
  if (num_excl==0) {
    print("No hi ha criteris !")
    return("Error") }

  # 2. Generar dummies
  dt_nova<-generar_dummies_exclusions(dt=dt,criteris=variables$criteris)

  # Noves variables generades
  exclusions<-paste0("exclusio",c(1:num_excl))
  
  #exclusions<-paste0("exclusio",c(1:2))
  #exclusions<-c("exclusio1","exclusio5")
  
  # labels si no s han passat
  if (is.null(labels)) variables<-variables %>% dplyr::mutate(labels=criteris)

  # 3. Generar flow_chart
  Flow_chart_Consort(dt=dt_nova,
                     exclusions=exclusions,
                     lab_exclusions=variables$labels,
                     grups = grups,
                     sequencial=sequencial,
                     lab_start=lab_start,
                     lab_random=lab_random)

}




#' @title                      Aplica criteris partir d una llista de n criteris exclusio
#' @description                Aplica criteris partir d una llista de n criteris exclusio que estan en un fitxer extern (Tipo excel)
#' @param dt                   Dataframe tibble
#' @param taulavariables       String referent a path o tibble on hi ha una columna consten les exclusions: "conductor_cars.xls"
#' @param criteris             String referent a la columna on consten les exclusions
#' @param missings             Logic TRUE FALSE  (De moment no operatiu)
#' @param ...                  Altres prametres
#' @importFrom                 dplyr "%>%"
#' @export                     criteris_exclusio_ggconsort
criteris_exclusio_ggconsort<-function(dt="dades",
                                      taulavariables="VARIABLES_R3b.xls",
                                      criteris="exclusio1",
                                      missings=T,
                                      ...) {
  
  # dt=dades
  # taulavariables=conductor
  # sheet="Exclusions"
  # criteris="exclusio"
  # missings=T
  
  
  ##  2. Eliminar els espais en blanc de les variables factors del data.frame
  dt<-dt %>%
    dplyr::mutate_if(is.factor,dplyr::funs(stringr::str_trim(.))) %>%
    dplyr::mutate_if(is.character,dplyr::funs(stringr::str_trim(.)))
  
  ##  Llegeix criteris de variables
  variables <- read_conductor(taulavariables,col_types = "text",...) %>% tidyr::as_tibble() %>% dplyr::select(camp,!!criteris)
  # variables <- read_conductor(taulavariables,col_types = "text",sheet=sheet) %>% tidyr::as_tibble() %>% dplyr::select(camp,!!criteris)
  # Filtrar valors
  criteris_sym<-sym(criteris)
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
  maco<-maco %>% dplyr::mutate(filtres=paste0("(",crit_temp,")"))
  
  # Afegir valors valids per aplicar criteri (Si missings==F)
  if (missings==F) maco<-maco %>% dplyr::mutate(filtres=stringr::str_c("(", filtres, " & !is.na(",camp, "))"))
  
  # Concateno condicions amb un OR
  maco<-stringr::str_c(maco$filtres,collapse=" | ")
  
  ## 1. Genera filtre en base a columna exclusio1   popes
  popes<-stringr::str_c("!(",maco,")")
  
  ##  3. Aplicar filtre: popes a dt
  dt %>% dplyr::filter(eval(parse(text=popes)))
  
}






