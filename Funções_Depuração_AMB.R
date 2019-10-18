# rm(list = ls())

# Funções e pacotes ----

library(tidyverse)

FUNA_removeAcentos <-
  function(x) {
    gsub(pattern = "á", replacement = "a", x) %>% gsub(pattern = "â", replacement = "a") %>% gsub(pattern = "à", replacement = "a") %>% 	gsub(pattern = "ã", replacement = "a") %>% 	gsub(pattern = "é", replacement = "e") %>% 	gsub(pattern = "ê", replacement = "e") %>% 	gsub(pattern = "í", replacement = "i") %>% 	gsub(pattern = "ó", replacement = "o") %>% 	gsub(pattern = "ô", replacement = "o") %>% 	gsub(pattern = "õ", replacement = "o") %>% 	gsub(pattern = "ú", replacement = "u") %>% 	gsub(pattern = "ç", replacement = "c") %>% 	gsub(pattern = "Á", replacement = "A") %>% 	gsub(pattern = "Â", replacement = "A") %>% 	gsub(pattern = "À", replacement = "A") %>% 	gsub(pattern = "Ã", replacement = "A") %>% 	gsub(pattern = "É", replacement = "E") %>% 	gsub(pattern = "Ê", replacement = "E") %>% 	gsub(pattern = "Í", replacement = "I") %>% 	gsub(pattern = "Ó", replacement = "O") %>% 	gsub(pattern = "Ô", replacement = "O") %>% 	gsub(pattern = "Õ", replacement = "O") %>% 	gsub(pattern = "Ú", replacement = "U") %>% gsub(pattern = "Ç", replacement = "C") %>% gsub(pattern = "'", replacement = "") %>% gsub(pattern = "-", replacement = " ")
  }
FUNA_maiusculas <-
  function(x) {
    gsub(pattern =  "a", replacement =  "A", x) %>%	gsub(pattern =  "b", replacement =  "B") %>% gsub(pattern =  "c", replacement =  "C") %>%	gsub(pattern =  "d", replacement =  "D") %>%	gsub(pattern =  "e", replacement =  "E") %>%	gsub(pattern =  "f", replacement =  "F") %>%	gsub(pattern =  "g", replacement =  "G") %>%	gsub(pattern =  "h", replacement =  "H") %>%	gsub(pattern =  "i", replacement =  "I") %>%	gsub(pattern =  "j", replacement =  "J") %>%	gsub(pattern =  "l", replacement =  "L") %>%	gsub(pattern =  "m", replacement =  "M") %>%	gsub(pattern =  "n", replacement =  "N") %>%	gsub(pattern =  "o", replacement =  "O") %>%	gsub(pattern =  "p", replacement =  "P") %>%	gsub(pattern =  "q", replacement =  "Q") %>%	gsub(pattern =  "r", replacement =  "R") %>%	gsub(pattern =  "s", replacement =  "S") %>%	gsub(pattern =  "t", replacement =  "T") %>%	gsub(pattern =  "u", replacement =  "U") %>%	gsub(pattern =  "v", replacement =  "V") %>%	gsub(pattern =  "x", replacement =  "X") %>% gsub(pattern =  "y", replacement =  "Y")	%>% gsub(pattern =  "z", replacement =  "Z")
  }
FUNA_minusculas <-
  function(x) {
    gsub(replacement =  "a", pattern =  "A", x) %>%	gsub(replacement =  "b", pattern =  "B") %>% gsub(replacement =  "c", pattern =  "C") %>%	gsub(replacement =  "d", pattern =  "D") %>%	gsub(replacement =  "e", pattern =  "E") %>%	gsub(replacement =  "f", pattern =  "F") %>%	gsub(replacement =  "g", pattern =  "G") %>%	gsub(replacement =  "h", pattern =  "H") %>%	gsub(replacement =  "i", pattern =  "I") %>%	gsub(replacement =  "j", pattern =  "J") %>%	gsub(replacement =  "l", pattern =  "L") %>%	gsub(replacement =  "m", pattern =  "M") %>%	gsub(replacement =  "n", pattern =  "N") %>%	gsub(replacement =  "o", pattern =  "O") %>%	gsub(replacement =  "p", pattern =  "P") %>%	gsub(replacement =  "q", pattern =  "Q") %>%	gsub(replacement =  "r", pattern =  "R") %>%	gsub(replacement =  "s", pattern =  "S") %>%	gsub(replacement =  "t", pattern =  "T") %>%	gsub(replacement =  "u", pattern =  "U") %>%	gsub(replacement =  "v", pattern =  "V") %>%	gsub(replacement =  "x", pattern =  "X") %>% gsub(replacement =  "y", pattern =  "Y")	%>% gsub(replacement =  "z", pattern =  "Z")
  }
FUNA_numerosFormatados <-
  function(x, decimais = 0) {
    m <- x
    for (i in 1:nrow(m)) {
      for (j in 1:ncol(m)) {
        if (is.na(x[[i, j]]) == TRUE) {
          m[i, j] <- c("-")
        } else {
          if (is.numeric(x[[i, j]]) == TRUE) {
            m[i, j] <-
              format(
                round(x[[i, j]], digits = decimais),
                big.mark = ".",
                decimal.mark = ","
              )
          }
        }
      }
    }
    return(m)
  }

FUNA_ordenacao <-
  function(tabela, coluna) {
    as.data.frame(tabela)[order(as.data.frame(tabela)[, coluna], decreasing = TRUE),]
  }

FUNA_BARPLOT <- function(x) {
  barplot(height = as.matrix(x))
}

FUNA_reserva_PARETO <-
  function(subsAMB, year, prob) {
    lista <- list()
    for (i in 1:nrow(reserva_AMB[grepl(reserva_AMB$substancia.amb, pattern = subsAMB) == TRUE &
                                 reserva_AMB$ano == year,])) {
      if ((
        quantile(
          reserva_AMB[grepl(reserva_AMB$substancia.amb, pattern = subsAMB) == TRUE &
                      reserva_AMB$ano == year,]$massa.medida,
          na.rm = TRUE,
          probs = prob
        ) < reserva_AMB[grepl(reserva_AMB$substancia.amb, pattern = subsAMB) == TRUE &
                        reserva_AMB$ano == year,]$massa.medida[i]
      )) {
        lista[[i]] <-
          reserva_AMB[grepl(reserva_AMB$substancia.amb, pattern = subsAMB) == TRUE &
                        reserva_AMB$ano == year, ][i, c(1:6, 8, 11)]
      }
    }
    arrange(do.call("rbind", lista),
            desc(massa.medida))
  }



FUNA_producaoBRUTA_PARETO <-
  function(subsAMB, year, prob) {
    lista <- list()
    for (i in 1:nrow(producaoBRUTA[grepl(producaoBRUTA$substancia.amb, pattern = subsAMB) == TRUE &
                                   producaoBRUTA$ano == year,])) {
      if ((
        quantile(
          producaoBRUTA[grepl(producaoBRUTA$substancia.amb, pattern = subsAMB) == TRUE &
                        producaoBRUTA$ano == year,]$quantidade.producao.ajuste,
          na.rm = TRUE,
          probs = prob
        ) < producaoBRUTA[grepl(producaoBRUTA$substancia.amb, pattern = subsAMB) == TRUE &
                          producaoBRUTA$ano == year,]$quantidade.producao.ajuste[i]
      )) {
        lista[[i]] <-
          producaoBRUTA[grepl(producaoBRUTA$substancia.amb, pattern = subsAMB) == TRUE &
                          producaoBRUTA$ano == year,][i, c(1:6, 8, 11)]
      }
    }
    arrange(do.call("rbind", lista),
            desc(quantidade.producao.ajuste))
  }



FUNA_producaoBENEFICIADA_PARETO <-
  function(subsAMB, year, prob) {
    lista <- list()
    for (i in 1:nrow(producaoBENEFICIADA[grepl(producaoBENEFICIADA$substancia.amb, pattern = subsAMB) == TRUE &
                                         producaoBENEFICIADA$ano == year,])) {
      if ((
        quantile(
          producaoBENEFICIADA[grepl(producaoBENEFICIADA$substancia.amb, pattern = subsAMB) == TRUE &
                              producaoBENEFICIADA$ano == year,]$quantidade.producao.ajuste,
          na.rm = TRUE,
          probs = prob
        ) < producaoBENEFICIADA[grepl(producaoBENEFICIADA$substancia.amb, pattern = subsAMB) == TRUE &
                                producaoBENEFICIADA$ano == year,]$quantidade.producao.ajuste[i]
      )) {
        lista[[i]] <-
          producaoBENEFICIADA[grepl(producaoBENEFICIADA$substancia.amb, pattern = subsAMB) == TRUE &
                                producaoBENEFICIADA$ano == year,][i, c(1:6, 8, 11)]
      }
    }
    arrange(do.call("rbind", lista),
            desc(quantidade.producao.ajuste))
  }



#_____Abatimento Reserva producao




FUNA_Abatimento_Reserva_Producao <-
  function(processo, subsAMB = ".", mina = ".", cpfcnpj = ".", ano1 = '2011') {
    lista <- list()
    lista[[as.integer(ano1)]] <-
      reserva_groupBY_MINA(
        subsAMB = subsAMB,
        cpfcnpj = cpfcnpj,
        processo = processo,
        mina = mina)[1, ano1]
    
    for (i in ano1:2017) {
      lista[[i + 1]] <-
        lista[[i]] - producaoBRUTA_groupBY_MINA(subsAMB = subsAMB,
                                                processo = processo,
                                                mina = mina)[1, as.character(i + 1)]
    }
    do.call('rbind', lista)
  }




FUNA_visao_RESERVA <-
  function(processo = '.',
           cpfcnpj = '.',
           mina = '.',
           subsAMB = '.') {
    reserva_groupBY_SUBSTANCIA.AMB(
      processo = processo,
      cpfcnpj = cpfcnpj,
      mina = mina,
      subsAMB = subsAMB
    ) %>%
      #FUNA_numerosFormatados() %>%
      print()
    reserva_groupBY_MINA(
      processo = processo,
      cpfcnpj = cpfcnpj,
      mina = mina,
      subsAMB = subsAMB
    ) %>%
      #FUNA_numerosFormatados() %>%
      print()
    reserva_groupBY_PROCESSO(
      processo = processo,
      cpfcnpj = cpfcnpj,
      mina = mina,
      subsAMB = subsAMB
    ) %>%
      #FUNA_numerosFormatados() %>%
      print()
    reserva_groupBY_MUNICIPIO(
      processo = processo,
      cpfcnpj = cpfcnpj,
      mina = mina,
      subsAMB = subsAMB
    ) %>%
      #FUNA_numerosFormatados() %>%
      print()
    reserva_groupBY_TITULAR(
      processo = processo,
      cpfcnpj = cpfcnpj,
      mina = mina,
      subsAMB = subsAMB
    ) %>%
      #FUNA_numerosFormatados() %>%
      print()
    a <-
      paste("reserva ", paste(processo, paste(cpfcnpj, paste(mina, subsAMB)))) # título do gráfico
    reserva_GERAL(
      processo = processo,
      cpfcnpj = cpfcnpj,
      mina = mina,
      subsAMB = subsAMB
    ) %>% as.matrix() %>% barplot(main = a)
    # prodBruta
    producaoBRUTA_groupBY_SUBSTANCIA.AMB(
      processo = processo,
      cpfcnpj = cpfcnpj,
      mina = mina,
      subsAMB = subsAMB
    ) %>%
      #FUNA_numerosFormatados() %>%
      print()
    #eventos
    if (processo != ".") {
      FUNA_Eventos_RRR_RFP(processo = processo) %>% print()
    }
  }


FUNA_Eventos_RRR_RFP <-
  function(processo = ".",
           cpfcnpj = ".",
           subsAMB = ".",
           situacao.operacional = ".",
           reavaliacao.reserva = ".",
           alteração.pae.reserva = ".") {
    x <-
      select(Eventos_RRR_RFP[grepl(Eventos_RRR_RFP$substancia.amb, pattern = subsAMB) == TRUE &
                               grepl(Eventos_RRR_RFP$cpfcnpj, pattern = cpfcnpj) == TRUE &
                               grepl(Eventos_RRR_RFP$alteração.pae.reserva,
                                     pattern = alteração.pae.reserva) == TRUE &
                               grepl(Eventos_RRR_RFP$reavaliacao.reserva, pattern = reavaliacao.reserva) == TRUE &
                               grepl(Eventos_RRR_RFP$situacao.operacional.processo, pattern = situacao.operacional) == TRUE &
                               grepl(Eventos_RRR_RFP$processo, pattern = processo) == TRUE,], everything())
    return(x)
  }




# Funções RESERVA ----
#_____reserva_GERAL
reserva_GERAL <-
  function(subsAMB = ".",
           subsRAL = ".",
           cpfcnpj = ".",
           municipio = ".",
           mina = ".",
           processo = ".",
           reserva = "medida") {
    if (reserva == "medida") {
      x <-
        spread(
          select(reserva_AMB[grepl(reserva_AMB$substancia.amb, pattern = subsAMB) == TRUE &
                               grepl(reserva_AMB$substancia.ral, pattern = subsRAL) == TRUE &
                               grepl(reserva_AMB$cpfcnpj, pattern = cpfcnpj) == TRUE &
                               grepl(reserva_AMB$municipio, pattern = municipio) == TRUE &
                               grepl(reserva_AMB$processo, pattern = processo) == TRUE &
                               grepl(reserva_AMB$mina, pattern = mina) == TRUE, ], everything()) %>%
            group_by(ano) %>%
            summarise(soma = sum(massa.medida)),
          key = ano,
          value = soma
        )
      
      return(x)
      
    } else {
      if (reserva == "indicada") {
        x <-
          spread(
            select(reserva_AMB[grepl(reserva_AMB$substancia.amb, pattern = subsAMB) == TRUE &
                                 grepl(reserva_AMB$substancia.ral, pattern = subsRAL) == TRUE &
                                 grepl(reserva_AMB$cpfcnpj, pattern = cpfcnpj) == TRUE &
                                 grepl(reserva_AMB$municipio, pattern = municipio) == TRUE &
                                 grepl(reserva_AMB$processo, pattern = processo) == TRUE &
                                 grepl(reserva_AMB$mina, pattern = mina) == TRUE, ], everything()) %>%
              group_by(ano) %>%
              summarise(soma = sum(massa.indicada)),
            key = ano,
            value = soma
          )
        
        return(x)
      } else {
        if (reserva == "inferida") {
          x <-
            spread(
              select(reserva_AMB[grepl(reserva_AMB$substancia.amb, pattern = subsAMB) == TRUE &
                                   grepl(reserva_AMB$substancia.ral, pattern = subsRAL) == TRUE &
                                   grepl(reserva_AMB$cpfcnpj, pattern = cpfcnpj) == TRUE &
                                   grepl(reserva_AMB$municipio, pattern = municipio) == TRUE &
                                   grepl(reserva_AMB$processo, pattern = processo) == TRUE &
                                   grepl(reserva_AMB$mina, pattern = mina) == TRUE, ], everything()) %>%
                group_by(ano) %>%
                summarise(soma = sum(massa.inferida)),
              key = ano,
              value = soma
            )
          
          return(x)
        } else {
          if (reserva == "lavravel") {
            x <-
              spread(
                select(reserva_AMB[grepl(reserva_AMB$substancia.amb, pattern = subsAMB) == TRUE &
                                     grepl(reserva_AMB$substancia.ral, pattern = subsRAL) == TRUE &
                                     grepl(reserva_AMB$cpfcnpj, pattern = cpfcnpj) == TRUE &
                                     grepl(reserva_AMB$municipio, pattern = municipio) == TRUE &
                                     grepl(reserva_AMB$processo, pattern = processo) == TRUE &
                                     grepl(reserva_AMB$mina, pattern = mina) == TRUE, ], everything()) %>%
                  group_by(ano) %>%
                  summarise(soma = sum(massa.lavravel)),
                key = ano,
                value = soma
              )
            
            return(x)
            
          }
        }
      }
    }
  }


#_____reserva_groupBY_MINA
reserva_groupBY_MINA <-
  function(subsAMB = ".",
           subsRAL = ".",
           cpfcnpj = ".",
           municipio = ".",
           mina = ".",
           processo = ".",
           reserva = "medida") {
    if (reserva == "medida") {
      x <-
        spread(
          select(reserva_AMB[grepl(reserva_AMB$substancia.amb, pattern = subsAMB) == TRUE &
                               grepl(reserva_AMB$substancia.ral, pattern = subsRAL) == TRUE &
                               grepl(reserva_AMB$cpfcnpj, pattern = cpfcnpj) == TRUE &
                               grepl(reserva_AMB$municipio, pattern = municipio) == TRUE &
                               grepl(reserva_AMB$processo, pattern = processo) == TRUE &
                               grepl(reserva_AMB$mina, pattern = mina) == TRUE, ], everything()) %>%
            group_by(ano, mina) %>%
            summarise(soma = sum(massa.medida)),
          key = ano,
          value = soma
        )
      
      return(x)
      
    } else {
      if (reserva == "indicada") {
        x <-
          spread(
            select(reserva_AMB[grepl(reserva_AMB$substancia.amb, pattern = subsAMB) == TRUE &
                                 grepl(reserva_AMB$substancia.ral, pattern = subsRAL) == TRUE &
                                 grepl(reserva_AMB$cpfcnpj, pattern = cpfcnpj) == TRUE &
                                 grepl(reserva_AMB$municipio, pattern = municipio) == TRUE &
                                 grepl(reserva_AMB$processo, pattern = processo) == TRUE &
                                 grepl(reserva_AMB$mina, pattern = mina) == TRUE, ], everything()) %>%
              group_by(ano, mina) %>%
              summarise(soma = sum(massa.indicada)),
            key = ano,
            value = soma
          )
        
        return(x)
      } else {
        if (reserva == "inferida") {
          x <-
            spread(
              select(reserva_AMB[grepl(reserva_AMB$substancia.amb, pattern = subsAMB) == TRUE &
                                   grepl(reserva_AMB$substancia.ral, pattern = subsRAL) == TRUE &
                                   grepl(reserva_AMB$cpfcnpj, pattern = cpfcnpj) == TRUE &
                                   grepl(reserva_AMB$municipio, pattern = municipio) == TRUE &
                                   grepl(reserva_AMB$processo, pattern = processo) == TRUE &
                                   grepl(reserva_AMB$mina, pattern = mina) == TRUE, ], everything()) %>%
                group_by(ano, mina) %>%
                summarise(soma = sum(massa.inferida)),
              key = ano,
              value = soma
            )
          
          return(x)
        } else {
          if (reserva == "lavravel") {
            x <-
              spread(
                select(reserva_AMB[grepl(reserva_AMB$substancia.amb, pattern = subsAMB) == TRUE &
                                     grepl(reserva_AMB$substancia.ral, pattern = subsRAL) == TRUE &
                                     grepl(reserva_AMB$cpfcnpj, pattern = cpfcnpj) == TRUE &
                                     grepl(reserva_AMB$municipio, pattern = municipio) == TRUE &
                                     grepl(reserva_AMB$processo, pattern = processo) == TRUE &
                                     grepl(reserva_AMB$mina, pattern = mina) == TRUE, ], everything()) %>%
                  group_by(ano, mina) %>%
                  summarise(soma = sum(massa.lavravel)),
                key = ano,
                value = soma
              )
            
            return(x)
            
          }
        }
      }
    }
  }

#_____reserva_groupBY_MUNICIPIO
reserva_groupBY_MUNICIPIO <-
  function(subsAMB = ".",
           subsRAL = ".",
           cpfcnpj = ".",
           municipio = ".",
           mina = ".",
           processo = ".",
           reserva = "medida") {
    if (reserva == "medida") {
      x <-
        spread(
          select(reserva_AMB[grepl(reserva_AMB$substancia.amb, pattern = subsAMB) == TRUE &
                               grepl(reserva_AMB$substancia.ral, pattern = subsRAL) == TRUE &
                               grepl(reserva_AMB$cpfcnpj, pattern = cpfcnpj) == TRUE &
                               grepl(reserva_AMB$municipio, pattern = municipio) == TRUE &
                               grepl(reserva_AMB$processo, pattern = processo) == TRUE &
                               grepl(reserva_AMB$mina, pattern = mina) == TRUE, ], everything()) %>%
            group_by(ano, municipio) %>%
            summarise(soma = sum(massa.medida)),
          key = ano,
          value = soma
        )
      
      return(x)
      
    } else {
      if (reserva == "indicada") {
        x <-
          spread(
            select(reserva_AMB[grepl(reserva_AMB$substancia.amb, pattern = subsAMB) == TRUE &
                                 grepl(reserva_AMB$substancia.ral, pattern = subsRAL) == TRUE &
                                 grepl(reserva_AMB$cpfcnpj, pattern = cpfcnpj) == TRUE &
                                 grepl(reserva_AMB$municipio, pattern = municipio) == TRUE &
                                 grepl(reserva_AMB$processo, pattern = processo) == TRUE &
                                 grepl(reserva_AMB$mina, pattern = mina) == TRUE, ], everything()) %>%
              group_by(ano, municipio) %>%
              summarise(soma = sum(massa.indicada)),
            key = ano,
            value = soma
          )
        
        return(x)
      } else {
        if (reserva == "inferida") {
          x <-
            spread(
              select(reserva_AMB[grepl(reserva_AMB$substancia.amb, pattern = subsAMB) == TRUE &
                                   grepl(reserva_AMB$substancia.ral, pattern = subsRAL) == TRUE &
                                   grepl(reserva_AMB$cpfcnpj, pattern = cpfcnpj) == TRUE &
                                   grepl(reserva_AMB$municipio, pattern = municipio) == TRUE &
                                   grepl(reserva_AMB$processo, pattern = processo) == TRUE &
                                   grepl(reserva_AMB$mina, pattern = mina) == TRUE, ], everything()) %>%
                group_by(ano, municipio) %>%
                summarise(soma = sum(massa.inferida)),
              key = ano,
              value = soma
            )
          
          return(x)
        } else {
          if (reserva == "lavravel") {
            x <-
              spread(
                select(reserva_AMB[grepl(reserva_AMB$substancia.amb, pattern = subsAMB) == TRUE &
                                     grepl(reserva_AMB$substancia.ral, pattern = subsRAL) == TRUE &
                                     grepl(reserva_AMB$cpfcnpj, pattern = cpfcnpj) == TRUE &
                                     grepl(reserva_AMB$municipio, pattern = municipio) == TRUE &
                                     grepl(reserva_AMB$processo, pattern = processo) == TRUE &
                                     grepl(reserva_AMB$mina, pattern = mina) == TRUE, ], everything()) %>%
                  group_by(ano, municipio) %>%
                  summarise(soma = sum(massa.lavravel)),
                key = ano,
                value = soma
              )
            
            return(x)
            
          }
        }
      }
    }
  }

#_____reserva_groupBY_PROCESSO
reserva_groupBY_PROCESSO <-
  function(subsAMB = ".",
           subsRAL = ".",
           cpfcnpj = ".",
           municipio = ".",
           mina = ".",
           processo = ".",
           reserva = "medida") {
    if (reserva == "medida") {
      x <-
        spread(
          select(reserva_AMB[grepl(reserva_AMB$substancia.amb, pattern = subsAMB) == TRUE &
                               grepl(reserva_AMB$substancia.ral, pattern = subsRAL) == TRUE &
                               grepl(reserva_AMB$cpfcnpj, pattern = cpfcnpj) == TRUE &
                               grepl(reserva_AMB$municipio, pattern = municipio) == TRUE &
                               grepl(reserva_AMB$processo, pattern = processo) == TRUE &
                               grepl(reserva_AMB$mina, pattern = mina) == TRUE, ], everything()) %>%
            group_by(ano, processo) %>%
            summarise(soma = sum(massa.medida)),
          key = ano,
          value = soma
        )
      
      return(x)
      
    } else {
      if (reserva == "indicada") {
        x <-
          spread(
            select(reserva_AMB[grepl(reserva_AMB$substancia.amb, pattern = subsAMB) == TRUE &
                                 grepl(reserva_AMB$substancia.ral, pattern = subsRAL) == TRUE &
                                 grepl(reserva_AMB$cpfcnpj, pattern = cpfcnpj) == TRUE &
                                 grepl(reserva_AMB$municipio, pattern = municipio) == TRUE &
                                 grepl(reserva_AMB$processo, pattern = processo) == TRUE &
                                 grepl(reserva_AMB$mina, pattern = mina) == TRUE, ], everything()) %>%
              group_by(ano, processo) %>%
              summarise(soma = sum(massa.indicada)),
            key = ano,
            value = soma
          )
        
        return(x)
      } else {
        if (reserva == "inferida") {
          x <-
            spread(
              select(reserva_AMB[grepl(reserva_AMB$substancia.amb, pattern = subsAMB) == TRUE &
                                   grepl(reserva_AMB$substancia.ral, pattern = subsRAL) == TRUE &
                                   grepl(reserva_AMB$cpfcnpj, pattern = cpfcnpj) == TRUE &
                                   grepl(reserva_AMB$municipio, pattern = municipio) == TRUE &
                                   grepl(reserva_AMB$processo, pattern = processo) == TRUE &
                                   grepl(reserva_AMB$mina, pattern = mina) == TRUE, ], everything()) %>%
                group_by(ano, processo) %>%
                summarise(soma = sum(massa.inferida)),
              key = ano,
              value = soma
            )
          
          return(x)
        } else {
          if (reserva == "lavravel") {
            x <-
              spread(
                select(reserva_AMB[grepl(reserva_AMB$substancia.amb, pattern = subsAMB) == TRUE &
                                     grepl(reserva_AMB$substancia.ral, pattern = subsRAL) == TRUE &
                                     grepl(reserva_AMB$cpfcnpj, pattern = cpfcnpj) == TRUE &
                                     grepl(reserva_AMB$municipio, pattern = municipio) == TRUE &
                                     grepl(reserva_AMB$processo, pattern = processo) == TRUE &
                                     grepl(reserva_AMB$mina, pattern = mina) == TRUE, ], everything()) %>%
                  group_by(ano, processo) %>%
                  summarise(soma = sum(massa.lavravel)),
                key = ano,
                value = soma
              )
            
            return(x)
            
          }
        }
      }
    }
  }

#_____reserva_groupBY_SUBSTANCIA.AMB
reserva_groupBY_SUBSTANCIA.AMB <-
  function(subsAMB = ".",
           subsRAL = ".",
           cpfcnpj = ".",
           municipio = ".",
           mina = ".",
           processo = ".",
           reserva = "medida") {
    if (reserva == "medida") {
      x <-
        spread(
          select(reserva_AMB[grepl(reserva_AMB$substancia.amb, pattern = subsAMB) == TRUE &
                               grepl(reserva_AMB$substancia.ral, pattern = subsRAL) == TRUE &
                               grepl(reserva_AMB$cpfcnpj, pattern = cpfcnpj) == TRUE &
                               grepl(reserva_AMB$municipio, pattern = municipio) == TRUE &
                               grepl(reserva_AMB$processo, pattern = processo) == TRUE &
                               grepl(reserva_AMB$mina, pattern = mina) == TRUE, ], everything()) %>%
            group_by(ano, substancia.amb) %>%
            summarise(soma = sum(massa.medida)),
          key = ano,
          value = soma
        )
      
      return(x)
      
    } else {
      if (reserva == "indicada") {
        x <-
          spread(
            select(reserva_AMB[grepl(reserva_AMB$substancia.amb, pattern = subsAMB) == TRUE &
                                 grepl(reserva_AMB$substancia.ral, pattern = subsRAL) == TRUE &
                                 grepl(reserva_AMB$cpfcnpj, pattern = cpfcnpj) == TRUE &
                                 grepl(reserva_AMB$municipio, pattern = municipio) == TRUE &
                                 grepl(reserva_AMB$processo, pattern = processo) == TRUE &
                                 grepl(reserva_AMB$mina, pattern = mina) == TRUE, ], everything()) %>%
              group_by(ano, substancia.amb) %>%
              summarise(soma = sum(massa.indicada)),
            key = ano,
            value = soma
          )
        
        return(x)
      } else {
        if (reserva == "inferida") {
          x <-
            spread(
              select(reserva_AMB[grepl(reserva_AMB$substancia.amb, pattern = subsAMB) == TRUE &
                                   grepl(reserva_AMB$substancia.ral, pattern = subsRAL) == TRUE &
                                   grepl(reserva_AMB$cpfcnpj, pattern = cpfcnpj) == TRUE &
                                   grepl(reserva_AMB$municipio, pattern = municipio) == TRUE &
                                   grepl(reserva_AMB$processo, pattern = processo) == TRUE &
                                   grepl(reserva_AMB$mina, pattern = mina) == TRUE, ], everything()) %>%
                group_by(ano, substancia.amb) %>%
                summarise(soma = sum(massa.inferida)),
              key = ano,
              value = soma
            )
          
          return(x)
        } else {
          if (reserva == "lavravel") {
            x <-
              spread(
                select(reserva_AMB[grepl(reserva_AMB$substancia.amb, pattern = subsAMB) == TRUE &
                                     grepl(reserva_AMB$substancia.ral, pattern = subsRAL) == TRUE &
                                     grepl(reserva_AMB$cpfcnpj, pattern = cpfcnpj) == TRUE &
                                     grepl(reserva_AMB$municipio, pattern = municipio) == TRUE &
                                     grepl(reserva_AMB$processo, pattern = processo) == TRUE &
                                     grepl(reserva_AMB$mina, pattern = mina) == TRUE, ], everything()) %>%
                  group_by(ano, substancia.amb) %>%
                  summarise(soma = sum(massa.lavravel)),
                key = ano,
                value = soma
              )
            
            return(x)
            
          }
        }
      }
    }
  }


#_____reserva_groupBY_TITULAR
reserva_groupBY_TITULAR <-
  function(subsAMB = ".",
           subsRAL = ".",
           cpfcnpj = ".",
           municipio = ".",
           mina = ".",
           processo = ".",
           reserva = "medida") {
    if (reserva == "medida") {
      x <-
        spread(
          select(reserva_AMB[grepl(reserva_AMB$substancia.amb, pattern = subsAMB) == TRUE &
                               grepl(reserva_AMB$substancia.ral, pattern = subsRAL) == TRUE &
                               grepl(reserva_AMB$cpfcnpj, pattern = cpfcnpj) == TRUE &
                               grepl(reserva_AMB$municipio, pattern = municipio) == TRUE &
                               grepl(reserva_AMB$processo, pattern = processo) == TRUE &
                               grepl(reserva_AMB$mina, pattern = mina) == TRUE, ], everything()) %>%
            group_by(ano, cpfcnpj) %>%
            summarise(soma = sum(massa.medida)),
          key = ano,
          value = soma
        )
      
      return(x)
      
    } else {
      if (reserva == "indicada") {
        x <-
          spread(
            select(reserva_AMB[grepl(reserva_AMB$substancia.amb, pattern = subsAMB) == TRUE &
                                 grepl(reserva_AMB$substancia.ral, pattern = subsRAL) == TRUE &
                                 grepl(reserva_AMB$cpfcnpj, pattern = cpfcnpj) == TRUE &
                                 grepl(reserva_AMB$municipio, pattern = municipio) == TRUE &
                                 grepl(reserva_AMB$processo, pattern = processo) == TRUE &
                                 grepl(reserva_AMB$mina, pattern = mina) == TRUE, ], everything()) %>%
              group_by(ano, cpfcnpj) %>%
              summarise(soma = sum(massa.indicada)),
            key = ano,
            value = soma
          )
        
        return(x)
      } else {
        if (reserva == "inferida") {
          x <-
            spread(
              select(reserva_AMB[grepl(reserva_AMB$substancia.amb, pattern = subsAMB) == TRUE &
                                   grepl(reserva_AMB$substancia.ral, pattern = subsRAL) == TRUE &
                                   grepl(reserva_AMB$cpfcnpj, pattern = cpfcnpj) == TRUE &
                                   grepl(reserva_AMB$municipio, pattern = municipio) == TRUE &
                                   grepl(reserva_AMB$processo, pattern = processo) == TRUE &
                                   grepl(reserva_AMB$mina, pattern = mina) == TRUE, ], everything()) %>%
                group_by(ano, cpfcnpj) %>%
                summarise(soma = sum(massa.inferida)),
              key = ano,
              value = soma
            )
          
          return(x)
        } else {
          if (reserva == "lavravel") {
            x <-
              spread(
                select(reserva_AMB[grepl(reserva_AMB$substancia.amb, pattern = subsAMB) == TRUE &
                                     grepl(reserva_AMB$substancia.ral, pattern = subsRAL) == TRUE &
                                     grepl(reserva_AMB$cpfcnpj, pattern = cpfcnpj) == TRUE &
                                     grepl(reserva_AMB$municipio, pattern = municipio) == TRUE &
                                     grepl(reserva_AMB$processo, pattern = processo) == TRUE &
                                     grepl(reserva_AMB$mina, pattern = mina) == TRUE, ], everything()) %>%
                  group_by(ano, cpfcnpj) %>%
                  summarise(soma = sum(massa.lavravel)),
                key = ano,
                value = soma
              )
            
            return(x)
            
          }
        }
      }
    }
  }


# Funções PRODUÇÃO BRUTA ----
#_____producaoBRUTA_GERAL
producaoBRUTA_GERAL <-
  function(subsAMB = ".",
           subsRAL = ".",
           cpfcnpj = ".",
           municipio = ".",
           mina = ".",
           processo = ".",
           volume = "rom") {
    if (volume == "rom") {
      x <-
        spread(
          select(producaoBRUTA[grepl(producaoBRUTA$substancia.amb, pattern = subsAMB) == TRUE &
                                 grepl(producaoBRUTA$substancia.ral, pattern = subsRAL) == TRUE &
                                 grepl(producaoBRUTA$cpfcnpj, pattern = cpfcnpj) == TRUE &
                                 grepl(producaoBRUTA$municipio, pattern = municipio) == TRUE &
                                 grepl(producaoBRUTA$processo, pattern = processo) == TRUE &
                                 grepl(producaoBRUTA$mina, pattern = mina) == TRUE,], everything()) %>%
            group_by(ano) %>%
            summarise(soma = sum(quantidade.producao.ajuste)),
          key = ano,
          value = soma
        )
      
      return(x)
      
    } else {
      if (volume == "venda") {
        x <-
          spread(
            select(producaoBRUTA[grepl(producaoBRUTA$substancia.amb, pattern = subsAMB) == TRUE &
                                   grepl(producaoBRUTA$substancia.ral, pattern = subsRAL) == TRUE &
                                   grepl(producaoBRUTA$cpfcnpj, pattern = cpfcnpj) == TRUE &
                                   grepl(producaoBRUTA$municipio, pattern = municipio) == TRUE &
                                   grepl(producaoBRUTA$processo, pattern = processo) == TRUE &
                                   grepl(producaoBRUTA$mina, pattern = mina) == TRUE,], everything()) %>%
              group_by(ano) %>%
              summarise(soma = sum(quantidade.venda.ajuste)),
            key = ano,
            value = soma
          )
        
        return(x)
      } else {
        if (volume == "producao.substancia") {
          x <-
            spread(
              select(producaoBRUTA[grepl(producaoBRUTA$substancia.amb, pattern = subsAMB) == TRUE &
                                     grepl(producaoBRUTA$substancia.ral, pattern = subsRAL) == TRUE &
                                     grepl(producaoBRUTA$cpfcnpj, pattern = cpfcnpj) == TRUE &
                                     grepl(producaoBRUTA$municipio, pattern = municipio) == TRUE &
                                     grepl(producaoBRUTA$processo, pattern = processo) == TRUE &
                                     grepl(producaoBRUTA$mina, pattern = mina) == TRUE,], everything()) %>%
                group_by(ano) %>%
                summarise(soma = sum(
                  quantidade.producao.substancia.ajuste
                )),
              key = ano,
              value = soma
            )
          
          return(x)
        } else {
          if (volume == "venda.substancia") {
            x <-
              spread(
                select(producaoBRUTA[grepl(producaoBRUTA$substancia.amb, pattern = subsAMB) == TRUE &
                                       grepl(producaoBRUTA$substancia.ral, pattern = subsRAL) == TRUE &
                                       grepl(producaoBRUTA$cpfcnpj, pattern = cpfcnpj) == TRUE &
                                       grepl(producaoBRUTA$municipio, pattern = municipio) == TRUE &
                                       grepl(producaoBRUTA$processo, pattern = processo) == TRUE &
                                       grepl(producaoBRUTA$mina, pattern = mina) == TRUE,], everything()) %>%
                  group_by(ano) %>%
                  summarise(soma = sum(
                    quantidade.venda.substancia.ajuste
                  )),
                key = ano,
                value = soma
              )
            
            return(x)
            
          }
        }
      }
    }
  }

#_____producaoBRUTA_groupBY_MINA
producaoBRUTA_groupBY_MINA <-
  function(subsAMB = ".",
           subsRAL = ".",
           cpfcnpj = ".",
           municipio = ".",
           mina = ".",
           processo = ".",
           volume = "rom") {
    if (volume == "rom") {
      x <-
        spread(
          select(producaoBRUTA[grepl(producaoBRUTA$substancia.amb, pattern = subsAMB) == TRUE &
                                 grepl(producaoBRUTA$substancia.ral, pattern = subsRAL) == TRUE &
                                 grepl(producaoBRUTA$cpfcnpj, pattern = cpfcnpj) == TRUE &
                                 grepl(producaoBRUTA$municipio, pattern = municipio) == TRUE &
                                 grepl(producaoBRUTA$processo, pattern = processo) == TRUE &
                                 grepl(producaoBRUTA$mina, pattern = mina) == TRUE,], everything()) %>%
            group_by(ano, mina) %>%
            summarise(soma = sum(quantidade.producao.ajuste)),
          key = ano,
          value = soma
        )
      
      return(x)
      
    } else {
      if (volume == "venda") {
        x <-
          spread(
            select(producaoBRUTA[grepl(producaoBRUTA$substancia.amb, pattern = subsAMB) == TRUE &
                                   grepl(producaoBRUTA$substancia.ral, pattern = subsRAL) == TRUE &
                                   grepl(producaoBRUTA$cpfcnpj, pattern = cpfcnpj) == TRUE &
                                   grepl(producaoBRUTA$municipio, pattern = municipio) == TRUE &
                                   grepl(producaoBRUTA$processo, pattern = processo) == TRUE &
                                   grepl(producaoBRUTA$mina, pattern = mina) == TRUE,], everything()) %>%
              group_by(ano, mina) %>%
              summarise(soma = sum(quantidade.venda.ajuste)),
            key = ano,
            value = soma
          )
        
        return(x)
      } else {
        if (volume == "producao.substancia") {
          x <-
            spread(
              select(producaoBRUTA[grepl(producaoBRUTA$substancia.amb, pattern = subsAMB) == TRUE &
                                     grepl(producaoBRUTA$substancia.ral, pattern = subsRAL) == TRUE &
                                     grepl(producaoBRUTA$cpfcnpj, pattern = cpfcnpj) == TRUE &
                                     grepl(producaoBRUTA$municipio, pattern = municipio) == TRUE &
                                     grepl(producaoBRUTA$processo, pattern = processo) == TRUE &
                                     grepl(producaoBRUTA$mina, pattern = mina) == TRUE,], everything()) %>%
                group_by(ano, mina) %>%
                summarise(soma = sum(
                  quantidade.producao.substancia.ajuste
                )),
              key = ano,
              value = soma
            )
          
          return(x)
        } else {
          if (volume == "venda.substancia") {
            x <-
              spread(
                select(producaoBRUTA[grepl(producaoBRUTA$substancia.amb, pattern = subsAMB) == TRUE &
                                       grepl(producaoBRUTA$substancia.ral, pattern = subsRAL) == TRUE &
                                       grepl(producaoBRUTA$cpfcnpj, pattern = cpfcnpj) == TRUE &
                                       grepl(producaoBRUTA$municipio, pattern = municipio) == TRUE &
                                       grepl(producaoBRUTA$processo, pattern = processo) == TRUE &
                                       grepl(producaoBRUTA$mina, pattern = mina) == TRUE,], everything()) %>%
                  group_by(ano, mina) %>%
                  summarise(soma = sum(
                    quantidade.venda.substancia.ajuste
                  )),
                key = ano,
                value = soma
              )
            
            return(x)
            
          }
        }
      }
    }
  }



#_____producaoBRUTA_groupBY_MUNICIPIO
producaoBRUTA_groupBY_MUNICIPIO <-
  function(subsAMB = ".",
           subsRAL = ".",
           cpfcnpj = ".",
           municipio = ".",
           mina = ".",
           processo = ".",
           volume = "rom") {
    if (volume == "rom") {
      x <-
        spread(
          select(producaoBRUTA[grepl(producaoBRUTA$substancia.amb, pattern = subsAMB) == TRUE &
                                 grepl(producaoBRUTA$substancia.ral, pattern = subsRAL) == TRUE &
                                 grepl(producaoBRUTA$cpfcnpj, pattern = cpfcnpj) == TRUE &
                                 grepl(producaoBRUTA$municipio, pattern = municipio) == TRUE &
                                 grepl(producaoBRUTA$processo, pattern = processo) == TRUE &
                                 grepl(producaoBRUTA$mina, pattern = mina) == TRUE,], everything()) %>%
            group_by(ano, municipio) %>%
            summarise(soma = sum(quantidade.producao.ajuste)),
          key = ano,
          value = soma
        )
      
      return(x)
      
    } else {
      if (volume == "venda") {
        x <-
          spread(
            select(producaoBRUTA[grepl(producaoBRUTA$substancia.amb, pattern = subsAMB) == TRUE &
                                   grepl(producaoBRUTA$substancia.ral, pattern = subsRAL) == TRUE &
                                   grepl(producaoBRUTA$cpfcnpj, pattern = cpfcnpj) == TRUE &
                                   grepl(producaoBRUTA$municipio, pattern = municipio) == TRUE &
                                   grepl(producaoBRUTA$processo, pattern = processo) == TRUE &
                                   grepl(producaoBRUTA$mina, pattern = mina) == TRUE,], everything()) %>%
              group_by(ano, municipio) %>%
              summarise(soma = sum(quantidade.venda.ajuste)),
            key = ano,
            value = soma
          )
        
        return(x)
      } else {
        if (volume == "producao.substancia") {
          x <-
            spread(
              select(producaoBRUTA[grepl(producaoBRUTA$substancia.amb, pattern = subsAMB) == TRUE &
                                     grepl(producaoBRUTA$substancia.ral, pattern = subsRAL) == TRUE &
                                     grepl(producaoBRUTA$cpfcnpj, pattern = cpfcnpj) == TRUE &
                                     grepl(producaoBRUTA$municipio, pattern = municipio) == TRUE &
                                     grepl(producaoBRUTA$processo, pattern = processo) == TRUE &
                                     grepl(producaoBRUTA$mina, pattern = mina) == TRUE,], everything()) %>%
                group_by(ano, municipio) %>%
                summarise(soma = sum(
                  quantidade.producao.substancia.ajuste
                )),
              key = ano,
              value = soma
            )
          
          return(x)
        } else {
          if (volume == "venda.substancia") {
            x <-
              spread(
                select(producaoBRUTA[grepl(producaoBRUTA$substancia.amb, pattern = subsAMB) == TRUE &
                                       grepl(producaoBRUTA$substancia.ral, pattern = subsRAL) == TRUE &
                                       grepl(producaoBRUTA$cpfcnpj, pattern = cpfcnpj) == TRUE &
                                       grepl(producaoBRUTA$municipio, pattern = municipio) == TRUE &
                                       grepl(producaoBRUTA$processo, pattern = processo) == TRUE &
                                       grepl(producaoBRUTA$mina, pattern = mina) == TRUE,], everything()) %>%
                  group_by(ano, municipio) %>%
                  summarise(soma = sum(
                    quantidade.venda.substancia.ajuste
                  )),
                key = ano,
                value = soma
              )
            
            return(x)
            
          }
        }
      }
    }
  }


#_____producaoBRUTA_groupBY_PROCESSO
producaoBRUTA_groupBY_PROCESSO <-
  function(subsAMB = ".",
           subsRAL = ".",
           cpfcnpj = ".",
           municipio = ".",
           mina = ".",
           processo = ".",
           volume = "rom") {
    if (volume == "rom") {
      x <-
        spread(
          select(producaoBRUTA[grepl(producaoBRUTA$substancia.amb, pattern = subsAMB) == TRUE &
                                 grepl(producaoBRUTA$substancia.ral, pattern = subsRAL) == TRUE &
                                 grepl(producaoBRUTA$cpfcnpj, pattern = cpfcnpj) == TRUE &
                                 grepl(producaoBRUTA$municipio, pattern = municipio) == TRUE &
                                 grepl(producaoBRUTA$processo, pattern = processo) == TRUE &
                                 grepl(producaoBRUTA$mina, pattern = mina) == TRUE,], everything()) %>%
            group_by(ano, processo) %>%
            summarise(soma = sum(quantidade.producao.ajuste)),
          key = ano,
          value = soma
        )
      
      return(x)
      
    } else {
      if (volume == "venda") {
        x <-
          spread(
            select(producaoBRUTA[grepl(producaoBRUTA$substancia.amb, pattern = subsAMB) == TRUE &
                                   grepl(producaoBRUTA$substancia.ral, pattern = subsRAL) == TRUE &
                                   grepl(producaoBRUTA$cpfcnpj, pattern = cpfcnpj) == TRUE &
                                   grepl(producaoBRUTA$municipio, pattern = municipio) == TRUE &
                                   grepl(producaoBRUTA$processo, pattern = processo) == TRUE &
                                   grepl(producaoBRUTA$mina, pattern = mina) == TRUE,], everything()) %>%
              group_by(ano, processo) %>%
              summarise(soma = sum(quantidade.venda.ajuste)),
            key = ano,
            value = soma
          )
        
        return(x)
      } else {
        if (volume == "producao.substancia") {
          x <-
            spread(
              select(producaoBRUTA[grepl(producaoBRUTA$substancia.amb, pattern = subsAMB) == TRUE &
                                     grepl(producaoBRUTA$substancia.ral, pattern = subsRAL) == TRUE &
                                     grepl(producaoBRUTA$cpfcnpj, pattern = cpfcnpj) == TRUE &
                                     grepl(producaoBRUTA$municipio, pattern = municipio) == TRUE &
                                     grepl(producaoBRUTA$processo, pattern = processo) == TRUE &
                                     grepl(producaoBRUTA$mina, pattern = mina) == TRUE,], everything()) %>%
                group_by(ano, processo) %>%
                summarise(soma = sum(
                  quantidade.producao.substancia.ajuste
                )),
              key = ano,
              value = soma
            )
          
          return(x)
        } else {
          if (volume == "venda.substancia") {
            x <-
              spread(
                select(producaoBRUTA[grepl(producaoBRUTA$substancia.amb, pattern = subsAMB) == TRUE &
                                       grepl(producaoBRUTA$substancia.ral, pattern = subsRAL) == TRUE &
                                       grepl(producaoBRUTA$cpfcnpj, pattern = cpfcnpj) == TRUE &
                                       grepl(producaoBRUTA$municipio, pattern = municipio) == TRUE &
                                       grepl(producaoBRUTA$processo, pattern = processo) == TRUE &
                                       grepl(producaoBRUTA$mina, pattern = mina) == TRUE,], everything()) %>%
                  group_by(ano, processo) %>%
                  summarise(soma = sum(
                    quantidade.venda.substancia.ajuste
                  )),
                key = ano,
                value = soma
              )
            
            return(x)
            
          }
        }
      }
    }
  }



#_____producaoBRUTA_groupBY_SUBSTANCIA.AMB
producaoBRUTA_groupBY_SUBSTANCIA.AMB <-
  function(subsAMB = ".",
           subsRAL = ".",
           cpfcnpj = ".",
           municipio = ".",
           mina = ".",
           processo = ".",
           volume = "rom") {
    if (volume == "rom") {
      x <-
        spread(
          select(producaoBRUTA[grepl(producaoBRUTA$substancia.amb, pattern = subsAMB) == TRUE &
                                 grepl(producaoBRUTA$substancia.ral, pattern = subsRAL) == TRUE &
                                 grepl(producaoBRUTA$cpfcnpj, pattern = cpfcnpj) == TRUE &
                                 grepl(producaoBRUTA$municipio, pattern = municipio) == TRUE &
                                 grepl(producaoBRUTA$processo, pattern = processo) == TRUE &
                                 grepl(producaoBRUTA$mina, pattern = mina) == TRUE,], everything()) %>%
            group_by(ano, substancia.amb) %>%
            summarise(soma = sum(quantidade.producao.ajuste)),
          key = ano,
          value = soma
        )
      
      return(x)
      
    } else {
      if (volume == "venda") {
        x <-
          spread(
            select(producaoBRUTA[grepl(producaoBRUTA$substancia.amb, pattern = subsAMB) == TRUE &
                                   grepl(producaoBRUTA$substancia.ral, pattern = subsRAL) == TRUE &
                                   grepl(producaoBRUTA$cpfcnpj, pattern = cpfcnpj) == TRUE &
                                   grepl(producaoBRUTA$municipio, pattern = municipio) == TRUE &
                                   grepl(producaoBRUTA$processo, pattern = processo) == TRUE &
                                   grepl(producaoBRUTA$mina, pattern = mina) == TRUE,], everything()) %>%
              group_by(ano, substancia.amb) %>%
              summarise(soma = sum(quantidade.venda.ajuste)),
            key = ano,
            value = soma
          )
        
        return(x)
      } else {
        if (volume == "producao.substancia") {
          x <-
            spread(
              select(producaoBRUTA[grepl(producaoBRUTA$substancia.amb, pattern = subsAMB) == TRUE &
                                     grepl(producaoBRUTA$substancia.ral, pattern = subsRAL) == TRUE &
                                     grepl(producaoBRUTA$cpfcnpj, pattern = cpfcnpj) == TRUE &
                                     grepl(producaoBRUTA$municipio, pattern = municipio) == TRUE &
                                     grepl(producaoBRUTA$processo, pattern = processo) == TRUE &
                                     grepl(producaoBRUTA$mina, pattern = mina) == TRUE,], everything()) %>%
                group_by(ano, substancia.amb) %>%
                summarise(soma = sum(
                  quantidade.producao.substancia.ajuste
                )),
              key = ano,
              value = soma
            )
          
          return(x)
        } else {
          if (volume == "venda.substancia") {
            x <-
              spread(
                select(producaoBRUTA[grepl(producaoBRUTA$substancia.amb, pattern = subsAMB) == TRUE &
                                       grepl(producaoBRUTA$substancia.ral, pattern = subsRAL) == TRUE &
                                       grepl(producaoBRUTA$cpfcnpj, pattern = cpfcnpj) == TRUE &
                                       grepl(producaoBRUTA$municipio, pattern = municipio) == TRUE &
                                       grepl(producaoBRUTA$processo, pattern = processo) == TRUE &
                                       grepl(producaoBRUTA$mina, pattern = mina) == TRUE,], everything()) %>%
                  group_by(ano, substancia.amb) %>%
                  summarise(soma = sum(
                    quantidade.venda.substancia.ajuste
                  )),
                key = ano,
                value = soma
              )
            
            return(x)
            
          }
        }
      }
    }
  }



#_____producaoBRUTA_groupBY_TITULAR
producaoBRUTA_groupBY_TITULAR <-
  function(subsAMB = ".",
           subsRAL = ".",
           cpfcnpj = ".",
           municipio = ".",
           mina = ".",
           processo = ".",
           volume = "rom") {
    if (volume == "rom") {
      x <-
        spread(
          select(producaoBRUTA[grepl(producaoBRUTA$substancia.amb, pattern = subsAMB) == TRUE &
                                 grepl(producaoBRUTA$substancia.ral, pattern = subsRAL) == TRUE &
                                 grepl(producaoBRUTA$cpfcnpj, pattern = cpfcnpj) == TRUE &
                                 grepl(producaoBRUTA$municipio, pattern = municipio) == TRUE &
                                 grepl(producaoBRUTA$processo, pattern = processo) == TRUE &
                                 grepl(producaoBRUTA$mina, pattern = mina) == TRUE,], everything()) %>%
            group_by(ano, cpfcnpj) %>%
            summarise(soma = sum(quantidade.producao.ajuste)),
          key = ano,
          value = soma
        )
      
      return(x)
      
    } else {
      if (volume == "venda") {
        x <-
          spread(
            select(producaoBRUTA[grepl(producaoBRUTA$substancia.amb, pattern = subsAMB) == TRUE &
                                   grepl(producaoBRUTA$substancia.ral, pattern = subsRAL) == TRUE &
                                   grepl(producaoBRUTA$cpfcnpj, pattern = cpfcnpj) == TRUE &
                                   grepl(producaoBRUTA$municipio, pattern = municipio) == TRUE &
                                   grepl(producaoBRUTA$processo, pattern = processo) == TRUE &
                                   grepl(producaoBRUTA$mina, pattern = mina) == TRUE,], everything()) %>%
              group_by(ano, cpfcnpj) %>%
              summarise(soma = sum(quantidade.venda.ajuste)),
            key = ano,
            value = soma
          )
        
        return(x)
      } else {
        if (volume == "producao.substancia") {
          x <-
            spread(
              select(producaoBRUTA[grepl(producaoBRUTA$substancia.amb, pattern = subsAMB) == TRUE &
                                     grepl(producaoBRUTA$substancia.ral, pattern = subsRAL) == TRUE &
                                     grepl(producaoBRUTA$cpfcnpj, pattern = cpfcnpj) == TRUE &
                                     grepl(producaoBRUTA$municipio, pattern = municipio) == TRUE &
                                     grepl(producaoBRUTA$processo, pattern = processo) == TRUE &
                                     grepl(producaoBRUTA$mina, pattern = mina) == TRUE,], everything()) %>%
                group_by(ano, cpfcnpj) %>%
                summarise(soma = sum(
                  quantidade.producao.substancia.ajuste
                )),
              key = ano,
              value = soma
            )
          
          return(x)
        } else {
          if (volume == "venda.substancia") {
            x <-
              spread(
                select(producaoBRUTA[grepl(producaoBRUTA$substancia.amb, pattern = subsAMB) == TRUE &
                                       grepl(producaoBRUTA$substancia.ral, pattern = subsRAL) == TRUE &
                                       grepl(producaoBRUTA$cpfcnpj, pattern = cpfcnpj) == TRUE &
                                       grepl(producaoBRUTA$municipio, pattern = municipio) == TRUE &
                                       grepl(producaoBRUTA$processo, pattern = processo) == TRUE &
                                       grepl(producaoBRUTA$mina, pattern = mina) == TRUE,], everything()) %>%
                  group_by(ano, cpfcnpj) %>%
                  summarise(soma = sum(
                    quantidade.venda.substancia.ajuste
                  )),
                key = ano,
                value = soma
              )
            
            return(x)
            
          }
        }
      }
    }
  }


# Funções PRODUÇÃO BENEFICIADA ----
#_____producaoBENEFICIADA_GERAL
BeneficiadaPRODUCAO_GERAL <-
  function(subsAMB = ".",
           subsRAL = ".",
           cpfcnpj = ".",
           municipio = ".",
           usina = ".",
           produto = ".",
           volume = "producao") {
    if (volume == "producao") {
      x <-
        spread(
          select(producaoBENEFICIADA[grepl(producaoBENEFICIADA$substancia.amb, pattern = subsAMB) == TRUE &
                                       grepl(producaoBENEFICIADA$substancia.ral, pattern = subsRAL) == TRUE &
                                       grepl(producaoBENEFICIADA$cpfcnpj, pattern = cpfcnpj) == TRUE &
                                       grepl(producaoBENEFICIADA$municipio, pattern = municipio) == TRUE &
                                       grepl(producaoBENEFICIADA$produto.beneficiado, pattern = produto) == TRUE &
                                       grepl(producaoBENEFICIADA$usina, pattern = usina) == TRUE,], everything()) %>%
            group_by(ano) %>%
            summarise(soma = sum(quantidade.producao.ajuste)),
          key = ano,
          value = soma
        )
      
      return(x)
      
    } else {
      if (volume == "venda") {
        x <-
          spread(
            select(producaoBENEFICIADA[grepl(producaoBENEFICIADA$substancia.amb, pattern = subsAMB) == TRUE &
                                         grepl(producaoBENEFICIADA$substancia.ral, pattern = subsRAL) == TRUE &
                                         grepl(producaoBENEFICIADA$cpfcnpj, pattern = cpfcnpj) == TRUE &
                                         grepl(producaoBENEFICIADA$municipio, pattern = municipio) == TRUE &
                                         grepl(producaoBENEFICIADA$produto.beneficiado, pattern = produto) == TRUE &
                                         grepl(producaoBENEFICIADA$usina, pattern = usina) == TRUE,], everything()) %>%
              group_by(ano) %>%
              summarise(soma = sum(quantidade.venda.ajuste)),
            key = ano,
            value = soma
          )
        
        return(x)
      } else {
        if (volume == "producao.substancia") {
          x <-
            spread(
              select(producaoBENEFICIADA[grepl(producaoBENEFICIADA$substancia.amb, pattern = subsAMB) == TRUE &
                                           grepl(producaoBENEFICIADA$substancia.ral, pattern = subsRAL) == TRUE &
                                           grepl(producaoBENEFICIADA$cpfcnpj, pattern = cpfcnpj) == TRUE &
                                           grepl(producaoBENEFICIADA$municipio, pattern = municipio) == TRUE &
                                           grepl(producaoBENEFICIADA$produto.beneficiado, pattern = produto) == TRUE &
                                           grepl(producaoBENEFICIADA$usina, pattern = usina) == TRUE,], everything()) %>%
                group_by(ano) %>%
                summarise(soma = sum(
                  quantidade.producao.substancia.ajuste
                )),
              key = ano,
              value = soma
            )
          
          return(x)
        } else {
          if (volume == "venda.substancia") {
            x <-
              spread(
                select(producaoBENEFICIADA[grepl(producaoBENEFICIADA$substancia.amb, pattern = subsAMB) == TRUE &
                                             grepl(producaoBENEFICIADA$substancia.ral, pattern = subsRAL) == TRUE &
                                             grepl(producaoBENEFICIADA$cpfcnpj, pattern = cpfcnpj) == TRUE &
                                             grepl(producaoBENEFICIADA$municipio, pattern = municipio) == TRUE &
                                             grepl(producaoBENEFICIADA$produto.beneficiado, pattern = produto) == TRUE &
                                             grepl(producaoBENEFICIADA$usina, pattern = usina) == TRUE,], everything()) %>%
                  group_by(ano) %>%
                  summarise(soma = sum(
                    quantidade.venda.substancia.ajuste
                  )),
                key = ano,
                value = soma
              )
            
            return(x)
            
          }
        }
      }
    }
  }

#_____BeneficiadaPRODUCAO_groupBY_USINA
BeneficiadaPRODUCAO_groupBY_USINA <-
  function(subsAMB = ".",
           subsRAL = ".",
           cpfcnpj = ".",
           municipio = ".",
           usina = ".",
           produto = ".",
           volume = "producao") {
    if (volume == "producao") {
      x <-
        spread(
          select(producaoBENEFICIADA[grepl(producaoBENEFICIADA$substancia.amb, pattern = subsAMB) == TRUE &
                                       grepl(producaoBENEFICIADA$substancia.ral, pattern = subsRAL) == TRUE &
                                       grepl(producaoBENEFICIADA$cpfcnpj, pattern = cpfcnpj) == TRUE &
                                       grepl(producaoBENEFICIADA$municipio, pattern = municipio) == TRUE &
                                       grepl(producaoBENEFICIADA$produto.beneficiado, pattern = produto) == TRUE &
                                       grepl(producaoBENEFICIADA$usina, pattern = usina) == TRUE,], everything()) %>%
            group_by(ano, mina) %>%
            summarise(soma = sum(quantidade.producao.ajuste)),
          key = ano,
          value = soma
        )
      
      return(x)
      
    } else {
      if (volume == "venda") {
        x <-
          spread(
            select(producaoBENEFICIADA[grepl(producaoBENEFICIADA$substancia.amb, pattern = subsAMB) == TRUE &
                                         grepl(producaoBENEFICIADA$substancia.ral, pattern = subsRAL) == TRUE &
                                         grepl(producaoBENEFICIADA$cpfcnpj, pattern = cpfcnpj) == TRUE &
                                         grepl(producaoBENEFICIADA$municipio, pattern = municipio) == TRUE &
                                         grepl(producaoBENEFICIADA$produto.beneficiado, pattern = produto) == TRUE &
                                         grepl(producaoBENEFICIADA$usina, pattern = usina) == TRUE,], everything()) %>%
              group_by(ano, mina) %>%
              summarise(soma = sum(quantidade.venda.ajuste)),
            key = ano,
            value = soma
          )
        
        return(x)
      } else {
        if (volume == "producao.substancia") {
          x <-
            spread(
              select(producaoBENEFICIADA[grepl(producaoBENEFICIADA$substancia.amb, pattern = subsAMB) == TRUE &
                                           grepl(producaoBENEFICIADA$substancia.ral, pattern = subsRAL) == TRUE &
                                           grepl(producaoBENEFICIADA$cpfcnpj, pattern = cpfcnpj) == TRUE &
                                           grepl(producaoBENEFICIADA$municipio, pattern = municipio) == TRUE &
                                           grepl(producaoBENEFICIADA$produto.beneficiado, pattern = produto) == TRUE &
                                           grepl(producaoBENEFICIADA$usina, pattern = usina) == TRUE,], everything()) %>%
                group_by(ano, mina) %>%
                summarise(soma = sum(
                  quantidade.producao.substancia.ajuste
                )),
              key = ano,
              value = soma
            )
          
          return(x)
        } else {
          if (volume == "venda.substancia") {
            x <-
              spread(
                select(producaoBENEFICIADA[grepl(producaoBENEFICIADA$substancia.amb, pattern = subsAMB) == TRUE &
                                             grepl(producaoBENEFICIADA$substancia.ral, pattern = subsRAL) == TRUE &
                                             grepl(producaoBENEFICIADA$cpfcnpj, pattern = cpfcnpj) == TRUE &
                                             grepl(producaoBENEFICIADA$municipio, pattern = municipio) == TRUE &
                                             grepl(producaoBENEFICIADA$produto.beneficiado, pattern = produto) == TRUE &
                                             grepl(producaoBENEFICIADA$usina, pattern = usina) == TRUE,], everything()) %>%
                  group_by(ano, mina) %>%
                  summarise(soma = sum(
                    quantidade.venda.substancia.ajuste
                  )),
                key = ano,
                value = soma
              )
            
            return(x)
            
          }
        }
      }
    }
  }



#_____BeneficiadaPRODUCAO_groupBY_MUNICIPIO
BeneficiadaPRODUCAO_groupBY_MUNICIPIO <-
  function(subsAMB = ".",
           subsRAL = ".",
           cpfcnpj = ".",
           municipio = ".",
           usina = ".",
           produto = ".",
           volume = "producao") {
    if (volume == "producao") {
      x <-
        spread(
          select(producaoBENEFICIADA[grepl(producaoBENEFICIADA$substancia.amb, pattern = subsAMB) == TRUE &
                                       grepl(producaoBENEFICIADA$substancia.ral, pattern = subsRAL) == TRUE &
                                       grepl(producaoBENEFICIADA$cpfcnpj, pattern = cpfcnpj) == TRUE &
                                       grepl(producaoBENEFICIADA$municipio, pattern = municipio) == TRUE &
                                       grepl(producaoBENEFICIADA$produto.beneficiado, pattern = produto) == TRUE &
                                       grepl(producaoBENEFICIADA$usina, pattern = usina) == TRUE,], everything()) %>%
            group_by(ano, municipio) %>%
            summarise(soma = sum(quantidade.producao.ajuste)),
          key = ano,
          value = soma
        )
      
      return(x)
      
    } else {
      if (volume == "venda") {
        x <-
          spread(
            select(producaoBENEFICIADA[grepl(producaoBENEFICIADA$substancia.amb, pattern = subsAMB) == TRUE &
                                         grepl(producaoBENEFICIADA$substancia.ral, pattern = subsRAL) == TRUE &
                                         grepl(producaoBENEFICIADA$cpfcnpj, pattern = cpfcnpj) == TRUE &
                                         grepl(producaoBENEFICIADA$municipio, pattern = municipio) == TRUE &
                                         grepl(producaoBENEFICIADA$produto.beneficiado, pattern = produto) == TRUE &
                                         grepl(producaoBENEFICIADA$usina, pattern = usina) == TRUE,], everything()) %>%
              group_by(ano, municipio) %>%
              summarise(soma = sum(quantidade.venda.ajuste)),
            key = ano,
            value = soma
          )
        
        return(x)
      } else {
        if (volume == "producao.substancia") {
          x <-
            spread(
              select(producaoBENEFICIADA[grepl(producaoBENEFICIADA$substancia.amb, pattern = subsAMB) == TRUE &
                                           grepl(producaoBENEFICIADA$substancia.ral, pattern = subsRAL) == TRUE &
                                           grepl(producaoBENEFICIADA$cpfcnpj, pattern = cpfcnpj) == TRUE &
                                           grepl(producaoBENEFICIADA$municipio, pattern = municipio) == TRUE &
                                           grepl(producaoBENEFICIADA$produto.beneficiado, pattern = produto) == TRUE &
                                           grepl(producaoBENEFICIADA$usina, pattern = usina) == TRUE,], everything()) %>%
                group_by(ano, municipio) %>%
                summarise(soma = sum(
                  quantidade.producao.substancia.ajuste
                )),
              key = ano,
              value = soma
            )
          
          return(x)
        } else {
          if (volume == "venda.substancia") {
            x <-
              spread(
                select(producaoBENEFICIADA[grepl(producaoBENEFICIADA$substancia.amb, pattern = subsAMB) == TRUE &
                                             grepl(producaoBENEFICIADA$substancia.ral, pattern = subsRAL) == TRUE &
                                             grepl(producaoBENEFICIADA$cpfcnpj, pattern = cpfcnpj) == TRUE &
                                             grepl(producaoBENEFICIADA$municipio, pattern = municipio) == TRUE &
                                             grepl(producaoBENEFICIADA$produto.beneficiado, pattern = produto) == TRUE &
                                             grepl(producaoBENEFICIADA$usina, pattern = usina) == TRUE,], everything()) %>%
                  group_by(ano, municipio) %>%
                  summarise(soma = sum(
                    quantidade.venda.substancia.ajuste
                  )),
                key = ano,
                value = soma
              )
            
            return(x)
            
          }
        }
      }
    }
  }


#_____BeneficiadaPRODUCAO_groupBY_PRODUTO
BeneficiadaPRODUCAO_groupBY_PRODUTO <-
  function(subsAMB = ".",
           subsRAL = ".",
           cpfcnpj = ".",
           municipio = ".",
           usina = ".",
           produto = ".",
           volume = "producao") {
    if (volume == "producao") {
      x <-
        spread(
          select(producaoBENEFICIADA[grepl(producaoBENEFICIADA$substancia.amb, pattern = subsAMB) == TRUE &
                                       grepl(producaoBENEFICIADA$substancia.ral, pattern = subsRAL) == TRUE &
                                       grepl(producaoBENEFICIADA$cpfcnpj, pattern = cpfcnpj) == TRUE &
                                       grepl(producaoBENEFICIADA$municipio, pattern = municipio) == TRUE &
                                       grepl(producaoBENEFICIADA$produto.beneficiado, pattern = produto) == TRUE &
                                       grepl(producaoBENEFICIADA$usina, pattern = usina) == TRUE,], everything()) %>%
            group_by(ano, produto.beneficiado) %>%
            summarise(soma = sum(quantidade.producao.ajuste)),
          key = ano,
          value = soma
        )
      
      return(x)
      
    } else {
      if (volume == "venda") {
        x <-
          spread(
            select(producaoBENEFICIADA[grepl(producaoBENEFICIADA$substancia.amb, pattern = subsAMB) == TRUE &
                                         grepl(producaoBENEFICIADA$substancia.ral, pattern = subsRAL) == TRUE &
                                         grepl(producaoBENEFICIADA$cpfcnpj, pattern = cpfcnpj) == TRUE &
                                         grepl(producaoBENEFICIADA$municipio, pattern = municipio) == TRUE &
                                         grepl(producaoBENEFICIADA$produto.beneficiado, pattern = produto) == TRUE &
                                         grepl(producaoBENEFICIADA$usina, pattern = usina) == TRUE,], everything()) %>%
              group_by(ano, produto.beneficiado) %>%
              summarise(soma = sum(quantidade.venda.ajuste)),
            key = ano,
            value = soma
          )
        
        return(x)
      } else {
        if (volume == "producao.substancia") {
          x <-
            spread(
              select(producaoBENEFICIADA[grepl(producaoBENEFICIADA$substancia.amb, pattern = subsAMB) == TRUE &
                                           grepl(producaoBENEFICIADA$substancia.ral, pattern = subsRAL) == TRUE &
                                           grepl(producaoBENEFICIADA$cpfcnpj, pattern = cpfcnpj) == TRUE &
                                           grepl(producaoBENEFICIADA$municipio, pattern = municipio) == TRUE &
                                           grepl(producaoBENEFICIADA$produto.beneficiado, pattern = produto) == TRUE &
                                           grepl(producaoBENEFICIADA$usina, pattern = usina) == TRUE,], everything()) %>%
                group_by(ano, produto.beneficiado) %>%
                summarise(soma = sum(
                  quantidade.producao.substancia.ajuste
                )),
              key = ano,
              value = soma
            )
          
          return(x)
        } else {
          if (volume == "venda.substancia") {
            x <-
              spread(
                select(producaoBENEFICIADA[grepl(producaoBENEFICIADA$substancia.amb, pattern = subsAMB) == TRUE &
                                             grepl(producaoBENEFICIADA$substancia.ral, pattern = subsRAL) == TRUE &
                                             grepl(producaoBENEFICIADA$cpfcnpj, pattern = cpfcnpj) == TRUE &
                                             grepl(producaoBENEFICIADA$municipio, pattern = municipio) == TRUE &
                                             grepl(producaoBENEFICIADA$produto.beneficiado, pattern = produto) == TRUE &
                                             grepl(producaoBENEFICIADA$usina, pattern = usina) == TRUE,], everything()) %>%
                  group_by(ano, produto.beneficiado) %>%
                  summarise(soma = sum(
                    quantidade.venda.substancia.ajuste
                  )),
                key = ano,
                value = soma
              )
            
            return(x)
            
          }
        }
      }
    }
  }



#_____BeneficiadaPRODUCAO_groupBY_SUBSTANCIA.AMB
BeneficiadaPRODUCAO_groupBY_SUBSTANCIA.AMB <-
  function(subsAMB = ".",
           subsRAL = ".",
           cpfcnpj = ".",
           municipio = ".",
           usina = ".",
           produto = ".",
           volume = "producao") {
    if (volume == "producao") {
      x <-
        spread(
          select(producaoBENEFICIADA[grepl(producaoBENEFICIADA$substancia.amb, pattern = subsAMB) == TRUE &
                                       grepl(producaoBENEFICIADA$substancia.ral, pattern = subsRAL) == TRUE &
                                       grepl(producaoBENEFICIADA$cpfcnpj, pattern = cpfcnpj) == TRUE &
                                       grepl(producaoBENEFICIADA$municipio, pattern = municipio) == TRUE &
                                       grepl(producaoBENEFICIADA$produto.beneficiado, pattern = produto) == TRUE &
                                       grepl(producaoBENEFICIADA$usina, pattern = usina) == TRUE,], everything()) %>%
            group_by(ano, substancia.amb) %>%
            summarise(soma = sum(quantidade.producao.ajuste)),
          key = ano,
          value = soma
        )
      
      return(x)
      
    } else {
      if (volume == "venda") {
        x <-
          spread(
            select(producaoBENEFICIADA[grepl(producaoBENEFICIADA$substancia.amb, pattern = subsAMB) == TRUE &
                                         grepl(producaoBENEFICIADA$substancia.ral, pattern = subsRAL) == TRUE &
                                         grepl(producaoBENEFICIADA$cpfcnpj, pattern = cpfcnpj) == TRUE &
                                         grepl(producaoBENEFICIADA$municipio, pattern = municipio) == TRUE &
                                         grepl(producaoBENEFICIADA$produto.beneficiado, pattern = produto) == TRUE &
                                         grepl(producaoBENEFICIADA$usina, pattern = usina) == TRUE,], everything()) %>%
              group_by(ano, substancia.amb) %>%
              summarise(soma = sum(quantidade.venda.ajuste)),
            key = ano,
            value = soma
          )
        
        return(x)
      } else {
        if (volume == "producao.substancia") {
          x <-
            spread(
              select(producaoBENEFICIADA[grepl(producaoBENEFICIADA$substancia.amb, pattern = subsAMB) == TRUE &
                                           grepl(producaoBENEFICIADA$substancia.ral, pattern = subsRAL) == TRUE &
                                           grepl(producaoBENEFICIADA$cpfcnpj, pattern = cpfcnpj) == TRUE &
                                           grepl(producaoBENEFICIADA$municipio, pattern = municipio) == TRUE &
                                           grepl(producaoBENEFICIADA$produto.beneficiado, pattern = produto) == TRUE &
                                           grepl(producaoBENEFICIADA$usina, pattern = usina) == TRUE,], everything()) %>%
                group_by(ano, substancia.amb) %>%
                summarise(soma = sum(
                  quantidade.producao.substancia.ajuste
                )),
              key = ano,
              value = soma
            )
          
          return(x)
        } else {
          if (volume == "venda.substancia") {
            x <-
              spread(
                select(producaoBENEFICIADA[grepl(producaoBENEFICIADA$substancia.amb, pattern = subsAMB) == TRUE &
                                             grepl(producaoBENEFICIADA$substancia.ral, pattern = subsRAL) == TRUE &
                                             grepl(producaoBENEFICIADA$cpfcnpj, pattern = cpfcnpj) == TRUE &
                                             grepl(producaoBENEFICIADA$municipio, pattern = municipio) == TRUE &
                                             grepl(producaoBENEFICIADA$produto.beneficiado, pattern = produto) == TRUE &
                                             grepl(producaoBENEFICIADA$usina, pattern = usina) == TRUE,], everything()) %>%
                  group_by(ano, substancia.amb) %>%
                  summarise(soma = sum(
                    quantidade.venda.substancia.ajuste
                  )),
                key = ano,
                value = soma
              )
            
            return(x)
            
          }
        }
      }
    }
  }



#_____BeneficiadaPRODUCAO_groupBY_TITULAR
BeneficiadaPRODUCAO_groupBY_TITULAR <-
  function(subsAMB = ".",
           subsRAL = ".",
           cpfcnpj = ".",
           municipio = ".",
           usina = ".",
           produto = ".",
           volume = "producao") {
    if (volume == "producao") {
      x <-
        spread(
          select(producaoBENEFICIADA[grepl(producaoBENEFICIADA$substancia.amb, pattern = subsAMB) == TRUE &
                                       grepl(producaoBENEFICIADA$substancia.ral, pattern = subsRAL) == TRUE &
                                       grepl(producaoBENEFICIADA$cpfcnpj, pattern = cpfcnpj) == TRUE &
                                       grepl(producaoBENEFICIADA$municipio, pattern = municipio) == TRUE &
                                       grepl(producaoBENEFICIADA$produto.beneficiado, pattern = produto) == TRUE &
                                       grepl(producaoBENEFICIADA$usina, pattern = usina) == TRUE,], everything()) %>%
            group_by(ano, cpfcnpj) %>%
            summarise(soma = sum(quantidade.producao.ajuste)),
          key = ano,
          value = soma
        )
      
      return(x)
      
    } else {
      if (volume == "venda") {
        x <-
          spread(
            select(producaoBENEFICIADA[grepl(producaoBENEFICIADA$substancia.amb, pattern = subsAMB) == TRUE &
                                         grepl(producaoBENEFICIADA$substancia.ral, pattern = subsRAL) == TRUE &
                                         grepl(producaoBENEFICIADA$cpfcnpj, pattern = cpfcnpj) == TRUE &
                                         grepl(producaoBENEFICIADA$municipio, pattern = municipio) == TRUE &
                                         grepl(producaoBENEFICIADA$produto.beneficiado, pattern = produto) == TRUE &
                                         grepl(producaoBENEFICIADA$usina, pattern = usina) == TRUE,], everything()) %>%
              group_by(ano, cpfcnpj) %>%
              summarise(soma = sum(quantidade.venda.ajuste)),
            key = ano,
            value = soma
          )
        
        return(x)
      } else {
        if (volume == "producao.substancia") {
          x <-
            spread(
              select(producaoBENEFICIADA[grepl(producaoBENEFICIADA$substancia.amb, pattern = subsAMB) == TRUE &
                                           grepl(producaoBENEFICIADA$substancia.ral, pattern = subsRAL) == TRUE &
                                           grepl(producaoBENEFICIADA$cpfcnpj, pattern = cpfcnpj) == TRUE &
                                           grepl(producaoBENEFICIADA$municipio, pattern = municipio) == TRUE &
                                           grepl(producaoBENEFICIADA$produto.beneficiado, pattern = produto) == TRUE &
                                           grepl(producaoBENEFICIADA$usina, pattern = usina) == TRUE,], everything()) %>%
                group_by(ano, cpfcnpj) %>%
                summarise(soma = sum(
                  quantidade.producao.substancia.ajuste
                )),
              key = ano,
              value = soma
            )
          
          return(x)
        } else {
          if (volume == "venda.substancia") {
            x <-
              spread(
                select(producaoBENEFICIADA[grepl(producaoBENEFICIADA$substancia.amb, pattern = subsAMB) == TRUE &
                                             grepl(producaoBENEFICIADA$substancia.ral, pattern = subsRAL) == TRUE &
                                             grepl(producaoBENEFICIADA$cpfcnpj, pattern = cpfcnpj) == TRUE &
                                             grepl(producaoBENEFICIADA$municipio, pattern = municipio) == TRUE &
                                             grepl(producaoBENEFICIADA$produto.beneficiado, pattern = produto) == TRUE &
                                             grepl(producaoBENEFICIADA$usina, pattern = usina) == TRUE,], everything()) %>%
                  group_by(ano, cpfcnpj) %>%
                  summarise(soma = sum(
                    quantidade.venda.substancia.ajuste
                  )),
                key = ano,
                value = soma
              )
            
            return(x)
            
          }
        }
      }
    }
  }


# Funções Consumidores ----

consumidoresMINA_busca <-
  function(cpfcnpj = ".",
           titular = ".",
           ano = ".",
           mina = ".",
           subsAMB = ".",
           municipio = ".",
           nome.comprador = ".",
           municipio.comprador = ".",
           uso.destinacao = ".") {
    x <-
      select(consumidoresMINA[grepl(consumidoresMINA$cpfcnpj, pattern = cpfcnpj) == TRUE &
                                grepl(consumidoresMINA$titular, pattern = titular) == TRUE &
                                grepl(consumidoresMINA$ano, pattern = ano) == TRUE &
                                grepl(consumidoresMINA$mina, pattern = mina) == TRUE &
                                grepl(consumidoresMINA$substancias.amb.mina, pattern = subsAMB) == TRUE &
                                grepl(consumidoresMINA$municipio, pattern = municipio) == TRUE &
                                grepl(consumidoresMINA$nome.comprador, pattern = nome.comprador) == TRUE &
                                grepl(consumidoresMINA$municipio.comprador, pattern = municipio.comprador) == TRUE &
                                grepl(consumidoresMINA$uso.destinacao, pattern = uso.destinacao) == TRUE,], everything())
    return(x)
  }




consumidoresUSINA_busca <-
  function(cpfcnpj = ".",
           titular = ".",
           ano = ".",
           usina = ".",
           subsAMB = ".",
           municipio = ".",
           nome.comprador = ".",
           municipio.comprador = ".",
           uso.destinacao = ".",
           produto.beneficiado = ".") {
    x <-
      select(consumidoresUSINA[grepl(consumidoresUSINA$cpfcnpj, pattern = cpfcnpj) == TRUE &
                                 grepl(consumidoresUSINA$titular, pattern = titular) == TRUE &
                                 grepl(consumidoresUSINA$ano, pattern = ano) == TRUE &
                                 grepl(consumidoresUSINA$usina, pattern = usina) == TRUE &
                                 grepl(consumidoresUSINA$substancias.amb.usina, pattern = subsAMB) == TRUE &
                                 grepl(consumidoresUSINA$municipio, pattern = municipio) == TRUE &
                                 grepl(consumidoresUSINA$nome.comprador, pattern = nome.comprador) == TRUE &
                                 grepl(consumidoresUSINA$municipio.comprador, pattern = municipio.comprador) == TRUE &
                                 grepl(consumidoresUSINA$uso.destinacao, pattern = uso.destinacao) == TRUE &
                                 grepl(consumidoresUSINA$produto.beneficiado, pattern = produto.beneficiado) == TRUE,], everything())
    return(x)
  }
