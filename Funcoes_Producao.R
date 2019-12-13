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




# Avaliação de prioridades por critério de Pareto----



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
