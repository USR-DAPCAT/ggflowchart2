#' @title               Genera n variables dummis
#' @description         Genera n variables dummis (exclusio1,exclusio2,....exclusion) segons criteris d'exclusio determinats
#' @param dt            Dataframe/tibble
#' @param criteris      Vector string amb la definicio del criteris: criteris=c("Sepal.Length < 5","Species=='setosa'")
#' @return              Data.frame o tibble amb n columnes tipo dummie (0/1) afegides referides a les n exclusions
#' @export              generar_dummies_exclusions
#' @import              dplyr 
#' @examples
#' generar_dummies_exclusions(dt=iris,criteris=c("Sepal.Length < 5","Species=='setosa'"))
#' 
generar_dummies_exclusions<-function(dt=iris, criteris=c("Sepal.Length < 5","Species=='setosa'")) {

  # dt=dt
  # criteris=exclusions
  # dt=dt, criteris=exclusions

  # Generar dummies segons criteris d inclusio
  num_excl<-criteris %>% length()
  cols_dummies<-paste0("exclusio",c(1:num_excl))

  dt_dumies<-
    purrr::map2_dfc(criteris,cols_dummies,
                    ~dplyr::transmute(dt,!!dplyr::sym(.y):=eval(parse(text=.x)))
    ) %>% dplyr::mutate_all(as.numeric)

  # Juntar-ho tot
  # Truc per eliminar dumiis existents en dt
  vars_dt<-names(dt)[!names(dt)%in% c(names(dt_dumies))]
  dt %>% dplyr::select(vars_dt) %>% dplyr::bind_cols(dt_dumies)

}

