#Creamos una serie de funciones triviales

#Se obtiene con una función intuitiva el resto de una división introduciendo dividendo y divisor
resto<-function(dividendo,divisor){
  return(dividendo%%divisor)
}

#Te dirá si un número es divisible por otro cualquiera
es.divisible<-function(numero,divisor){
  if (numero%%divisor==0)
    return('Es divisible')
  else
    return('No es divisible')
  
}

#Calcula el IMC. Se debe tener en cuenta que el peso se introduce en kg y la estatura en m
IMC<-function(peso,estatura){
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

#Calcula el índice de grasa corporal
IGC<-function(peso,estatura,Edad,Sexo)
  return(1.2*peso/estatura^2+0.23*Edad-10.8*Sexo-5.4)