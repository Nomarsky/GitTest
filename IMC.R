IMC <-
function(peso,estatura){
  Calculo=(peso/estatura^2)
  if (Calculo<18)
    return('Peso bajo')
  else if (Calculo<24.9)
    return('Normal')
  else if (Calculo<26.9)
    return('Sobrepeso')
  else if (Calculo<29.9)
    return('Obsesidad gado I')
  else if (Calculo<39.9)
    return('Obsesidad grado II')
  else 
    return('Obesidad mórbida')
}
