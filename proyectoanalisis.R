library(eph)
library(tidyverse)
library(haven)
library(stringi)

# loop general itera de 2017 a 2019 para apilar de a 1 año por vez
for (y in 2017:2019) {
   
    # paso a caracteres el año de la vuelta del loop
    y <- as.character(y)
    # uso y (año) para crear el path correspondiente a los archivos de la eph de ese año
    eph_path <- paste0("~/Documentos/rProyects/EPHfiles/eph_",y,"_")
    
    # creo una lista vacia llamada "eph" 
    eph <- list()
    
    # dentro de cada año itero trabajando la seleccion de casos de a 1 trimestre por vez
    for (i in 1:4) {
        # termino de completar el path de la base trimestral de la eph agregando "i" que representa el trimestre
        t_i <- paste0(eph_path,i,".RDS")
        
        # levanto el archivo correspondiente
        eph[[i]] <- readRDS(t_i)
        
        # selecciono solo el elemento que contiene los datos
        eph[[i]] <- eph[[i]][[5]] %>% 
            as.data.frame()
        
        # creacion de una variable que combina CODUSU, NRO_HOGAR, TRIMESTRE y año
        eph[[i]]$idhogar <- paste0(eph[[i]]$CODUSU,eph[[i]]$NRO_HOGAR, eph[[i]]$TRIMESTRE,eph[[i]]$ANO4)
        
        # filtro y me quedo solo con individuos ocupados
        eph[[i]] <- eph[[i]] %>% 
            filter(ESTADO == 1)
        
        # luego vuelvo a filtrar y me quedo solo con jefe/a de hogar ocupado y mayor a 24 años
        # y con jovenes de 18 a 24 no jefes de hogar ocupados
        eph[[i]] <- eph[[i]] %>% 
            filter(CH03 == 1 & CH06>24 | CH03!= 1 & CH06 >= 18 & CH06<=24)
        
        # a esta altura todos los casos en la base tienen ESTADO == Ocupado, y son:
        # o bien jefes de hogar ocupados mayores de 24 o bien jovenes de 18 a 24 no jefes y ocupados
        # creo una tabla hogares, detalle del procedimiento:
        hogares <- eph[[i]] %>% 
            # a partir de las variables ch03 ych06 creo una variable "sujeto" que identifica al tipo de individuo entrevistado
            mutate(sujeto = case_when(
                # si es jefe/a y mayor de 24 años lo clasifica como "psh ocupado"
                CH03 == 1 & CH06 > 24 ~ "psh_ocupado",
                # si no es jefe/a y tiene entre 18 y 24 años como "joven ocupado"
                CH03 != 1 & CH06 <= 24 & CH06 >= 18 ~ "joven_ocupado",
                # si hubiera otros casos (no deberia haber) los clasifica como "otro"
                TRUE ~ "otro"
            )) %>% 
            # tomo solamente las variables "idhogar" y "sujeto"
            select(idhogar, sujeto) %>% 
            # a partir de dichas variables creo una tabla que indica para cada idhogar cuantos individuos hay según el tipo de sujeto
            pivot_wider(.,id_cols = "idhogar",names_from = "sujeto", values_from = "sujeto", values_fn = list(sujeto = length))
        
        # filtro la tabla para quedarme sólo con los hogares que tienen "psh ocupado" (debería haber sólo 1 por hogar) y
        # al mismo tiempo tienen al menos 1 "joven ocupado"
        hogares <- hogares %>% 
            filter(`psh_ocupado` == 1 & `joven_ocupado` >= 1)
        
        # a partir de la tabla de hogares seleccionados hago el filtro de esos hogares en la eph
        # concretamente a esta altura estamos excluyendo los psh ocupados y jovenes ocupados que pertenezcan
        # a un hogar que no cumple con tener presentes simultaneamente al psh ocupado y al joven ocupado
        eph[[i]] <- semi_join(eph[[i]], hogares)
    }
    # ahora la lista "eph" creada inicialmente contiene 4 elementos cada uno con la seleccion de casos de 1 trimestre
    # a constinuación unimos las filas de cada trimestre en una unica tabla "eph"
    eph <- rbind(eph[[1]], eph[[2]],eph[[3]],eph[[4]])
    eph <- as.data.frame(eph)
    eph$CODUSU <- as.character(eph$CODUSU)
    # guardo la tabla del año como ".sav" 
    save_path <- paste0("~/Documentos/rProyects/proyectoSalvia/eph_apilada_", y)
    write_sav(eph, path = paste0(save_path, ".sav"))
    # idem pero con formato ".RDS"
    write_rds(eph, path = paste0(save_path, ".RDS"), compress = "gz")
    write_csv(eph, path = paste0(save_path, ".csv"))
}


eph_apilada_2017 <- readRDS("~/Documentos/rProyects/proyectoSalvia/eph_apilada_2017.RDS")
eph_apilada_2018 <- readRDS("~/Documentos/rProyects/proyectoSalvia/eph_apilada_2018.RDS")
eph_apilada_2019 <- readRDS("~/Documentos/rProyects/proyectoSalvia/eph_apilada_2019.RDS")

eph2017a2019 <- rbind(eph_apilada_2017,eph_apilada_2018,eph_apilada_2019)


write_sav(eph2017a2019, path = paste0("~/Documentos/rProyects/proyectoSalvia/eph2017a2019", ".sav"))
# idem pero con formato ".RDS"
write_rds(eph2017a2019, path = paste0("~/Documentos/rProyects/proyectoSalvia/eph2017a2019", ".RDS"), compress = "gz")
write_csv(eph2017a2019, path = paste0("~/Documentos/rProyects/proyectoSalvia/eph2017a2019", ".csv"))