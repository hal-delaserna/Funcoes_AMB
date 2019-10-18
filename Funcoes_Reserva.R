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

