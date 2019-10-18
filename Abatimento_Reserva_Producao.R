




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


