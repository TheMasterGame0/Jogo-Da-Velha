# Feito por TheMasterGame0(GitHub)
#
ValoresTabuleiro = matrix(0,nrow=3,ncol=3)
TabuleiroJogador = matrix("-",nrow=3,ncol=3)

# Determina as condições de vitória.
Vitoria = function(ValoresTabuleiro){
  casos = c(colSums(ValoresTabuleiro), rowSums(ValoresTabuleiro), sum(diag(ValoresTabuleiro)), sum(ValoresTabuleiro[1,3] + ValoresTabuleiro[2,2] + ValoresTabuleiro[3,1]))
  if (any(casos == 3)){
    print("X ganhou!")
    return (FALSE)
  }else if (any(casos == -3)){
    print("O ganhou!")
    return (FALSE)
  }else{
    return (TRUE)
  }
}

#Determina se a jogada é válida.
Jogada = function(ValoresTabuleiro, X, Y){
  if (X>=1 & X<=3 & Y>= 1 & Y<=3){
    if (ValoresTabuleiro[X, Y] == 0){
      return (TRUE)
    }
    else{
      return (FALSE)
    }
  }else{
    return (FALSE)
  }
}

Vez = 1   #Quem começa
i = 0
#Loop principal utilizado para o funcionamento do jogo.
Ganhou = Vitoria(ValoresTabuleiro)
while (i<10 & Ganhou){
  print(TabuleiroJogador)
  if (Vez == 1){
    Letra = 'X'
    cat("É a vez do X:\n")
  }else{
    Letra = 'O'
    cat("É a vez do O:\n") 
  }
  X = as.integer(readline(prompt = "Linha: "))
  Y = as.integer(readline(prompt = "Coluna: "))
  if (Jogada(ValoresTabuleiro, X, Y)){
    ValoresTabuleiro[X, Y] = Vez
    TabuleiroJogador[X, Y] = Letra
    Vez = Vez*-1
    i = i+1
    Ganhou = Vitoria(ValoresTabuleiro)
  }
  else{
    print("Essa posição não é valida\n")
  }
}

# Retorna que o Jogo finalizou sem um vencedor (Velha)
if (Ganhou){
  print("O Jogo deu Velha!")
}
