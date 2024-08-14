#Primera parte
#Ej 1
#1 
def pertenece(s:list[int],e: int)->bool:
    res:bool = False
    for i in range(0,len(s),1):
        if s[i]==e:
            res = True
    return res

def pertenece2(s:list[int],e: int)->bool:
    res:bool = False
    for i in s:
        if i==e:
            res = True
    return res

#2
def divide_a_todos(s:list[int],e:int)->bool:
    res:bool = True
    for i in range(0,len(s),1):
        if (not(s[i]%e==0)):
            res = False
    return res
def divide_a_todos2(s:list[int],e:int)->bool:
    res:bool = True
    for i in s:
        if not(i%e==0):
            res = False
    return res
def divide_a_todos3(s:list[int],e:int)->bool:
    res:bool = True
    indice = 0
    while indice < len(s):
        if not(s[indice]%e==0):
            res = False
        indice+=1
    return res
#3
def suma_total(s:list[int])->int:
    res:int = 0
    for i in range(0,len(s),1):
        res = res + s[i]
    return res

def suma_total2(s:list[int])->int:
    res:int = 0
    indice = 0
    while indice < len(s):
        res+=s[indice]
        indice+=1
    return res

#4
def ordenados(s:list[int])->bool:
    res:bool = True
    for i in range(0,len(s)-1,1):
        if not(s[i]<=s[i+1]):
            res = False
    return res

#5
def lista_palabras(s:list[str])->bool:
    res:bool = False
    for i in range(0,len(s),1):
        if len(s[i])>7:
            res = True
    return res

#6
def palindromo(texto:str)->bool:
    res:bool = True
#replace funciona con dos parametros (cadena_vieja,cadena_nueva) Busca en este caso texto el string que le pasemos en cadena vieja y lo reemplaza por el string de cadena nueva 
    texto = texto.replace(" ","").lower() 
    l = len(texto)
    for i in range(0,l//2,1):
        if texto[i]!=texto[l-1-i]:
            res = False
    return res

#7
def hay_numero(palabra:str)->bool:
    res:bool = False
    for caracter in palabra:
        if caracter>='0' and caracter <='9':
            res = True
    return res
def hay_minuscula(palabra:str)->bool:
    res:bool = False
    for caracter in palabra:
        if caracter>='a' and caracter <='z':
            res = True
    return res
def hay_mayuscula(palabra:str)->bool:
    res:bool = False
    for caracter in palabra:
        if caracter>='a' and caracter <='z':
            res = True
    return res
def fortaleza_contraseña(contra:str)->str:
    l = len(contra)
    if l<5:
        res = "ROJA"
    elif l>8 and hay_mayuscula(contra) and hay_minuscula(contra) and hay_numero(contra):
        res = "VERDE"
    else:
        res = "AMARILLA"
    return res

#8
def saldoActual(movimientos:list[(str,float)])->float:
    saldo:float = 0
    long:int = len(movimientos)
    for i in range (0,long,1):
        if (movimientos[i][0]=='I'):
            saldo+=movimientos[i][1]
        elif (movimientos[i][0]=='R'):
            saldo-=movimientos[i][1]
    return saldo

#9
def vocalesDistintas(palabra:str)->bool:
    palabra = palabra.lower()
    contador: int = 0
    res = False
    if (pertenece(palabra,'a')):
        contador+=1
    if (pertenece(palabra,'e')):
        contador+=1
    if (pertenece(palabra,'i')):
        contador +=1
    if (pertenece(palabra,'o')):
        contador +=1
    if (pertenece(palabra,'u')):
        contador+=1
    if contador>=3:
        res = True
    return res

#Segunda parte
#Ej 2

#1 

#En este ejercicio lista es un parametro tipo inout, es decir la funcion modifica la lista original y es cambiada. 
#Una vez que ejecuta esta funcion 'lista' ya esta modificada siempre.
def borraPosicionesPares(lista:list[int])->list[int]:
    for i in range(0,len(lista),1):
        if (i%2==0):
            lista[i]=0
    return lista

#2

#Este ejercicio lista es un parametro tipo in, es decir la funcion modifica la lista solo dentro de la funcion. 
#Fuera de la funcion lista no es tocada
def borraPosicionesPares2(lista:list[int])->list[int]:
    lista_copiada = lista.copy()
    for i in range(0,len(lista_copiada),1):
        if (i%2==0):
            lista_copiada[i]=0
    return lista_copiada

#3
def es_vocal(letra:chr)->bool:
    return (letra=='a') or (letra=='e') or (letra=='i') or (letra=='o') or (letra=='u') or (letra=='A') or (letra=='E') or (letra=='I') or (letra=='O') or (letra=='U')
def borraVocales(palabra:list[chr])->list[chr]:
    salida:str = ""
    for caracter in palabra:
        if es_vocal(caracter):
            salida = salida
        else:
            salida+=caracter
    return salida

#4
def reemplazaVocales(palabra:list[chr])->list[chr]:
    salida:list[chr] = []
    for caracter in palabra:
        if es_vocal(caracter):
            salida += "-"
        else:
            salida+=caracter
    return salida

#5
def da_vuelta_str(palabra:list[chr])->list[chr]:
    salida:list[chr] = []
    long = len(palabra)
    for i in range(0,long,1):
        salida += palabra[long-i-1]
    return salida

#6
def eliminar_repetidos(palabra:list[chr])->list[chr]:
    salida:list[chr]=[]
    long = len(palabra)
    for i in range(0,long,1):
        if (palabra[i] not in salida):
            salida.append(palabra[i])
    return salida

def eliminar_repetidos2(palabra:list[chr])->list[chr]:
    salida:list[chr]=[]
    for caracter in palabra:
        if caracter not in salida:
            salida.append(caracter)
    return salida

#Ej 3
def promedio(notas:list[int])->float:
    sumaNotas: int = 0
    for nota in notas:
        sumaNotas+=nota
    promedio:float = ((sumaNotas)/len(notas))
    return promedio
def todasMayoresa4(notas:list[int])->bool:
    res:bool = True
    for nota in notas:
        if nota<4:
            res = False
    return res
def aprobado(notas:list[int])->int:
    res:int = 3
    if promedio(notas)>= 7 and todasMayoresa4(notas):
        res = 1
    elif promedio(notas)<7 and todasMayoresa4(notas):
        res = 2
    return res

#Ej 4
#1
def listaDeEstudiantes()->list[str]:
    res:list[str] = []
    nombre = ""
    while nombre != "listo":
        nombre = input("Ingre un nombre: ")
        if nombre!="listo":
            res.append(nombre)


#2
def historialSUBE()->list[(chr,int)]:
    res:list[(chr,int)] = []
    monto:int = 0
    opcion:str = ""
    plataActual:int = 0
    while opcion != 'X':
        opcion = input("Elija una opcion C: cargar credito, D: descontar credito, X: Finalizar: ")
        if opcion=='C':
            monto = int(input("Elija un monto para cargar: "))
            res.append(('C',monto))
            plataActual+=monto
        if opcion=='D':
            monto = int(input("Elija un monto para descontar: "))
            res.append(('D',monto))
            plataActual-=monto
        print("Usted tiene " +str(plataActual) +" pesos")
    return res
#3
import random
def juego7medio()->list[int]:
    historialDeCartas:list[int]= []
    opcion:str='S'
    contador:float = 0
    while(opcion=='S' and contador<=7.5):
        print("-------------")
        num:int=random.choice([1,2,3,4,5,6,7,10,11,12])
        print("Te toco un "+str(num))
        historialDeCartas.append(num)
        if num>=10:
            contador+=0.5
        else:
            contador+= num

        if contador>7.5:
            print("Contador = "+str(contador)+ " => PERDISTE")
            opcion = 'X'
        elif contador==7.5:
            print("Contador = "+str(contador)+" => GANASTE")
            opcion = 'X'
        else: 
            print("Contador = "+str(contador)+ " => S(para seguir), X(para parar)")
            opcion = input()
    return historialDeCartas

#Ej 5
#1
def pertenece_a_cada_unoV1(lista:list[list[int]],e:int)->list[bool]:
    res=[]
    for i in range(0,len (lista),1):
        res.append(pertenece(lista[i],e))
    return res

def es_matriz(lista:list[list[int]])->bool:
    res: bool = True
    long = len(lista)
    long0 = len(lista[0])
    for i in range(0,len(lista),1):
        if long0!=len(lista[i]):
            res = False
    if (long==0 or long0==0):
            res = False
    return res

def filas_ordenadas(matriz:list[list[int]],res:list[bool])->None:
    res = []
    long = len(matriz)
    for i in range(0,long,1):
        if not ordenados(matriz[i]):
            res.append(False)
        else: 
            res.append(True)
    return res

import numpy as np
def elervarMatriz(dim:int,pot:int)->list[list[int]]:
    # LOS PARAMETROS 0 , 10 SON LOS NUMEROS ALEATORIOS QUE SE VAN A GENERAR DENTRO DE LA MATRIZ
    # SI LOS ELEVO A  1 VOY A VER QUE NUMEROS SE GENERAN
    matriz=np.random.randint(0,10,(dim,dim)) 
    return np.linalg.matrix_power(matriz,pot)
