import math

#Ej 1

def imprimir_hola_mundo()->None:
    print("Hola Mundo")

def imprimir_un_verso()->None:
    print("No quiero quejarme de oreja en oreja\nFijarme si quien me aventaja se aleja\nNegar el reflejo que dejo en mi espejo\nNi alojar el rencor entre ceja y ceja")

def Raizde2()->float:
    res = round(math.sqrt(2),4)
    print(res)

def factorial_de_dos()->int:
    res = math.factorial(2)
    print(res)

def perimetro()->float:
    res = 2*(math.pi)
    print(res)

#Ej 2
def imprimir_saludo(nombre: str=None)->None:
    if nombre is None:
        nombre =  input ("Escribe tu nombre: ")
    return "Hola "+nombre

def raiz_cuadrada_de(numero: int)->float:
    res = round(math.sqrt(numero),2)
    return res

def fahrenheit_a_celcius(T: float) -> float:
    res = round(((T-32) * 5 / 9),2)
    return res

def imprimir_dos_veces(estribillo: str)-> str:
    return estribillo*2

def es_multiplo_de(n:int,m:int)->bool:
    while m==0:
        m = int(input("El segundo numero no puede ser cero\nPasa otro valor: "))
    return n%m==0

def es_par(n:int)->bool:
    return es_multiplo_de(n,2)

def cantidad_de_pizzas(comensales: int,min_cant_de_porciones: int)->int:
    res = math.ceil((comensales*min_cant_de_porciones)/8)
    return res

#Ej 3

def alguno_es_0(n1: float, n2: float) -> bool:
    return (n1==0) or (n2==0)

def ambos_son_0(n1:float,n2:float)->bool:
    return (n1==0) and (n2==0)

def es_nombre_largo(nombre:str)->bool:
    return 3<=len(nombre)<=8

def es_bisiesto(año:int)->bool:
    return (año%400==0) or ((año%4==0) and (año%100!=0))

#Ej 4

#altura en m, peso en kg
def peso_pino(altura:float)->int:
    if altura <=3:
        res = altura*300
    else:
        res = 900+(200*(altura-3))
    return res 

def es_peso_util(peso: int)->str:
    if 400<=peso<=1000:
        res = "Es un peso util para la empresa"
    else:
        res = "El peso no le sirve a la empresa"
    return res

def sirve_pino(altura:int)-> str:
    res = es_peso_util(peso_pino(altura))
    return res

#Ej 5

def devolver_el_doble_si_es_par(n:int)->int:
    if n%2==0:
        res = n*2
    else:
        res = n
    return res 

def devolver_valor_si_es_par_sino_el_que_sigue(n:int)->int:
    if n%2==0:
        res = n
    else:
        res = n+1
    return res

def devolver_el_doble_si_es_multiplo3_el_triple_si_es_multiplo9(n:int)->int:
    if es_multiplo_de(n,9):
        res = n*3
    elif es_multiplo_de(n,3):
        res = n*2
    else:
        res = n
    return res

def lindo_nombre(nombre:str)->str:
    if len(nombre)>=5:
        res = "Tu nombre tiene muchas letras!"
    else:
        res = "Tu nombre tiene menos de 5 caracteres"
    return res

def elRango(numero:int)->str:
    if numero<5:
        res = "Menos a 5"
    elif 10<=numero<=20:
        res = "Entre 10 y 20"
    else:
        res = "Mayor a 20"
    return res

def trabajo(sexo: str, edad: int)->str:
    if sexo=="M":
        if 18<=edad<=65:
            res = "Te toca trabajar"
        elif edad<18:
            res = "Anda de vacaciones"
        else:
            res = "Estas jubilado"
    elif sexo=="F":
        if 18<=edad<=60:
            res = "Te toca trabajar"
        elif edad<18:
            res = "Anda de vacaciones"
        else:
            res = "Estas jubilada"
    else: 
        input("Decidi tu sexo. F o M: ")
    return res

#Ej 6 

def numeros_1_al_10()->int:
    i = 1
    while i<=10:
        print(i)
        i+=1

def numeros_10_al_40()->int:
    i=10
    while i<=40:
        print(i)
        i+=2

def eco()->str:
    i=0
    while i<10:
        print("eco")
        i+=1

def cohete(cuenta:int)->None:
    while cuenta <= 0: 
        cuenta = int(input("Pasa un numero positivo mayor a 0: "))
    while cuenta>=1:
        print(cuenta)
        cuenta -=1
    print("Despegue")
    

def viaje_en_el_tiempo(añopar: int,añolle:int)->str:
    while añopar<=añolle:
        añolle = int(input("Ingrese un año de llegadas mas chico que el de partida: "))
    añopar-=1
    while añopar>=añolle:
        print("Viajo un año al pasado, estmaos en el año "+str(añopar))
        añopar -=1

def del1_10()->None:
    for i in range(1,11,1):
        print (i)

def del10_40()->None:
    for i in range (10,41,2):
        print(i)

def eco10()->str:
    for i in range(0,10,1):
        print("eco")

def cohete2(i:int)->None:
    while i<=0:
        i = int(input("Numero mayor a 0: "))
    for i in range(i,0,-1):
        print(i)
    print("Despegue")

