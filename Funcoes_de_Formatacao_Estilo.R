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
