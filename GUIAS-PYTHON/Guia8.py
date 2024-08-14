
def contar_lineas(nombre_archivo:str)->int:
    archivo = open ("archivo.txt",'r')
    sumador:int = 0
    for linea in archivo.readlines():
        sumador += 1
    archivo.close()
    return sumador

def existe_palabra(palabra:str,nombre_archivo:str)->bool:
    res:bool = False
    archivo = open ("archivo.txt",'r')
    for linea in archivo.readlines():
        if palabra in linea:
            res = True
    archivo.close()
    return res

def cantidad_apariciones(nombre_archivo:str,palabra:str)->int:
    res:int = 0
    palabra = palabra.lower()
    archivo = open ("archivo.txt",'r')
    contenido_del_archivo = archivo.read()
    palabras_del_archivo = contenido_del_archivo.split()
    for palabras_del_archivo in palabras_del_archivo:
        if palabra==palabras_del_archivo:
            res +=1
    archivo.close()
    return res

#Ejercicio 2
def clonar_sin_comentarios(nombre_archivo:str):
    archivo = open (nombre_archivo,'r')
    archivo_sin_comentarios = open ("clon.py",'w')
    lineas = archivo.readlines()
    for linea in lineas:
        if linea.strip()[0]!='#':
            archivo_sin_comentarios.write(linea)
    archivo.close()
    archivo_sin_comentarios.close()

#Ejercicio 3 
def invertir_lineas(nombre_archivo:str):
    archivo = open (nombre_archivo,'r')
    archivo_reverso = open ('reverso.txt','w')
    lineas = archivo.readlines()
    lineas[0] = lineas[0].rstrip('\n')
    lineas[-1] = lineas[-1]+'\n'
    lineas_al_reves = lineas[::-1] #SLICING ENTRE CORCHETOS MARCO START, STOP AND STEP 
    for linea in lineas_al_reves:
        archivo_reverso.write(linea)
    archivo.close
    archivo_reverso.close
    
#Ejercicio 4
def agregar_frase_al_final(nombre_archivo:str, frase:str):
    archivo = open(nombre_archivo,'a')
    archivo.write(frase)
    archivo.close()
    
#Ejercicio 5 
def agregar_frase_al_principio(nombre_archivo:str,frase:str):
    archivo = open(nombre_archivo, 'r+')
    contenido = archivo.read()
    archivo.seek(0,0)
    archivo.write(frase.rstrip('\r\n')+'\n'+contenido)
    archivo.close()

#Ejercicio 6 
def es_char_legible(caracter:str)->bool:
    res:bool = False
    if 'A'<=caracter<='Z' or 'a'<=caracter<='z' or '0'<=caracter<='9' or caracter=='_':
        res = True
    return res
def listar_palabras_de_archivo(nombre_archivo:str)-> list[str]:
    res:list[str] = []
    palabra_legible:str = ""
    archivo = open(nombre_archivo,'rb')
    bytes_contenido = archivo.read()
    for byte in bytes_contenido:
        caracter = chr(byte)
        if es_char_legible(caracter) :
            palabra_legible += caracter
        else:
            if len(palabra_legible)>=5:
                res.append(palabra_legible)
            palabra_legible = ""
    archivo.close()
    print(res)

#Ejercicio 7
def promedio_estudiante(nombre_archivo:str, lu:str)->float:
    archivo = open (nombre_archivo, 'r')
    estudiantes = archivo.readlines()
    contador:int = 0
    notaAcumulada:float = 0
    for estudiante in estudiantes:
        datos = estudiante.split(',')
        if datos[0]==lu:
            contador +=1
            notaAcumulada+= float(datos[3])
    promedio:float = notaAcumulada / contador
    archivo_promedio = open ('nombre_archivo_promedios.csv','a')
    archivo_promedio.write(lu + ',' + str(promedio) + '\n')
    archivo.close()
    archivo_promedio.close()
    return promedio

#---------------PILAS--------------------------
from codecs import utf_8_decode
from queue import LifoQueue as Pila
def pila_copy(p:Pila)->Pila:
    contenido = []
    contenido_aux = []
    pila_aux:Pila = Pila()
    copy:Pila = Pila()
    while not p.empty():
        contenido.append(p.get())
    for elem in contenido:
        pila_aux.put(elem)
    while not pila_aux.empty():
        contenido_aux.append(pila_aux.get())
    for elem in contenido_aux:
        copy.put(elem)
    return copy 

pila = Pila()
pila.queue = [5,6,7,8,9]
copiada = pila_copy(pila)
print(copiada.queue)
#Ejercicio 8 

import random
p = Pila()
def generar_nros_al_azar(cantidad:int,desde:int,hasta:int)->Pila:
    for i in range (0,cantidad,1):
        p.put(random.randint(desde,hasta))
    return p 

#Ejercicio 9

def cantidad_elementos(p:Pila)->int:
    pila_aux:Pila = pila_copy(p)
    contador:int = 0
    while not pila_aux.empty():
        pila_aux.get()
        contador +=1
    return contador

#Ejercicio 10
def buscar_el_maximo(p:Pila[int])->int:
    pila_aux:Pila[int] = pila_copy(p)
    contenido:list[int] = []
    maximo:int = 0
    while not pila_aux.empty():
        contenido.append(pila_aux.get())
    for elem in contenido:
        if elem >= maximo:
            maximo = elem
    return maximo

#Ejercicio 11
def esta_bien_balanceada(formula:str)->bool:
    res:bool = True
    p:Pila[str] = Pila()
    parentesis_abiertos:int = 0
    for letra in formula[::-1]:
        p.put(letra)
    while not p.empty():
        letra_sacada = p.get()
        if letra_sacada == '(':
            parentesis_abiertos +=1
        elif letra_sacada == ')':
            parentesis_abiertos -=1
        if parentesis_abiertos<0:
            res = False
    if parentesis_abiertos>0:
        res = False
    return res    

#Ejercicio 12
def evaluar_expresion(formula:str)->float:
    operandos = Pila()
    tokens=formula.split(" ")
    for token in tokens:
        if '0'<token<'9':
            operandos.put(token)
        elif token in ['+','-','*','/']:
            n2 = int(operandos.get())
            n1 = int(operandos.get()) 
            if token == '+':
                operandos.put(n1+n2)
            elif token == '-':
                operandos.put(n1-n2)
            elif token == '*':
                operandos.put(n1*n2)
            else:
                operandos.put(n1/n2)
    return operandos.get()

#---------------------------------COLAS--------------------------------
#Ejercicio 13 '''
from queue import Queue as Cola
c= Cola()
def cola_copy(c:Cola)->Cola:
    copy:Cola = Cola()
    contenido = []
    while not c.empty():
        contenido.append(c.get())
    for elem in contenido:
        c.put(elem)
    for elem in contenido:
        copy.put(elem)
    return copy

def generar_nros_al_azar_cola(cantidad:int,desde:int,hasta:int)->Cola[int]:
    for i in range(0,cantidad):
        c.put(random.randint(desde,hasta))
    return c

#Ejercicio 14
def cantidad_elementos_cola(c:Cola)->int:
    cola_aux:Cola = cola_copy(c)
    contador:int = 0
    while not cola_aux.empty():
        contador +=1
        cola_aux.get()
    return contador

#Ejercicio 15
def buscar_el_maximo_cola(c:Cola[int])->int:
    auxiliar = cola_copy(c)  
    maximo:int = 0
    while (not(auxiliar.empty())):
        elemento=auxiliar.get()
        if elemento>maximo:
            maximo = elemento
    return maximo

#Ejercicio 16
def bolillero()->Cola[int]:
    lista:list[int] = list(range(0,100,1))  
    random.shuffle(lista)
    cola:Cola[int] = Cola()
    for elem in lista:
        cola.put(elem)
    return cola
def carton_bingo()->list[int]:
    carton: list[int] = []
    for i in range(0,12,1):
        carton.append(random.randint(0,99))
    return carton
def jugar_carton_de_bingo(carton:list[int],bolillero:Cola[int])->int:
    jugados:int = 0
    num_marcados:int = 0
    bolillero_aux: Cola[int] = cola_copy (bolillero)
    while num_marcados < 12:
        bolilla_sacada = bolillero_aux.get()
        jugados +=1
        if bolilla_sacada in carton:
            num_marcados +=1
    print("El bolillero del juego es " + str(bolillero.queue))
    print("-----------------")
    print("El carton jugado es " + str(carton))
    print("--------------------------")
    return jugados

#Ejercicio 17
def n_pacientes_urgentes(c:Cola[(int,str,str)])->int:
    cola_aux:Cola[(int,str,str)] = cola_copy(c)
    contador:int = 0
    while not cola_aux.empty():
        paciente:(int,str,str) = cola_aux.get() # type: ignore
        if paciente[0] in [1,2,3]:
            contador +=1
    return contador

#Ejercicio 18
def atencion_a_clientes(c:Cola[(str,int,bool,bool)])->Cola[(str,int,bool,bool)]:
    cola_prioridad:Cola[(str,int,bool,bool)] = Cola()
    cola_preferencial: Cola[(str,int,bool,bool)] = Cola()
    cola_resto: Cola[(str,int,bool,bool)] = Cola()
    cola_ordenada: Cola[(str,int,bool,bool)] = Cola()
    cola_aux:Cola[(str,int,bool,bool)] = cola_copy (c)
    while not cola_aux.empty():
        cliente:(str,int,bool,bool) = cola_aux.get() # type: ignore
        if cliente[3]==True:
            cola_prioridad.put(cliente)
        elif cliente[2]==True:
            cola_preferencial.put(cliente)
        else:
            cola_resto.put(cliente)
    while not cola_prioridad.empty():
        cliente:(str,int,bool,bool) = cola_prioridad.get() # type: ignore
        cola_ordenada.put(cliente)
    while not cola_preferencial.empty():
        cliente:(str,int,bool,bool) = cola_preferencial.get() # type: ignore
        cola_preferencial.put(cliente)
    while not cola_resto.empty():
        cliente:(str,int,bool,bool) = cola_resto.get() # type: ignore
        cola_resto.put(cliente)
    return cola_resto

#--------------------------DICCIONARIO-------------------------------------
#Ejercicio 19
def agrupar_por_longitud(nombre_archivo:str)->dict:
    archivo = open(nombre_archivo,'r',encoding='UTF-8')
    res:dict[int] = {}
    lineas = archivo.readlines()
    for linea in lineas:
        palabras = linea.split()
        for palabra in palabras:
            longitud = len(palabra)
            if longitud in res.keys():
                res[longitud] +=1
            else:
                res[longitud] = 1
    archivo.close()
    return res

#Ejercicio 20
def calcular_promedio_por_estudiante(nombre_archivo_notas:str)->dict[str,float]:
    archivo = open(nombre_archivo_notas,'r',encoding='UTF-8')
    promedios:dict[str,float] = {}
    lineas = archivo.readlines()
    for linea in lineas:
        data = linea.rstrip('\n').split(',')
        lu:str = data[0]
        if lu not in promedios.keys():
            promedios[lu] = promedio_estudiante(nombre_archivo_notas,lu)
    archivo.close()
    return promedios

#Ejercicio  21
def la_palabra_mas_frecuente(nombre_archivo:str)->str:
    archivo = open(nombre_archivo,'r',encoding='UTF-8')
    lineas = archivo.readlines()
    frecuencia:dict[str] = {}
    for linea in lineas:
        palabras = linea.split()
        for palabra in palabras:
            if palabra  in frecuencia.keys():
                frecuencia[palabra]+=1
            else:
                frecuencia[palabra]=1
    archivo.close()
    pares:tuple[str,int] = frecuencia.items()
    max_aparaciones:tuple[str,int] = ("",0)
    for par in pares:
        if par[1]>max_aparaciones[1]:
            max_aparaciones = par
    return max_aparaciones[0]

#Ejercicio 22
historiales:dict[str,Pila[str]] = {}
def visitar_sitio(historiales:dict[str,Pila[str]],usuario:str,sitio:str):
    if usuario in historiales.keys():
        historiales[usuario].put(sitio)
    else:
        historiales[usuario] = Pila()
        historiales[usuario].put(sitio)
    print('Visitaste el sitio web: ' + sitio)
    return historiales
historiales:dict[str,Pila[str]] = {}
def navegar_atras(historiales:dict[str,Pila[str]],usuario:str):
    if usuario in historiales.keys(): 
        ultima = historiales[usuario].get()
        pagina = historiales[usuario].get()
        historiales[usuario].put(pagina)
        historiales[usuario].put(ultima)
        historiales[usuario].put(pagina)
        print('Estas en la pagina: ' + pagina)
    else:
        print('No tenes ningun historial')
    return historiales

#Ejercicio 23
inventario:dict[str,dict[str,str]] = {}

# INVENTARIO {PRODUCTO(CLAVE): INFORMACION(VALOR)}
# INFORMACION {NOMBRE : Name , PRECIO : price , CANTIDAD : amount}
PRICE:str = "PRICE"
AMOUNT:str = "AMOUNT"
def agregar_producto(inventario:dict[str,dict[str,str]],nombre:str,precio:int,cantidad:int)->dict[str,dict[str,str]]:
    informacion:dict[str,str] = {}
    informacion[PRICE] = str(precio)
    informacion[AMOUNT] = str(cantidad)
    inventario[nombre] = informacion
    return inventario

def actualizar_stock(inventario:dict[str,dict[str,str]],nombre:str,cantidad:int)->dict[str,dict[str,str]]:
    if nombre in inventario.keys():
        inventario[nombre][AMOUNT] = str(cantidad)
    return inventario

def actualizar_precios(inventario:dict[str,dict[str,str]],nombre:str,precio:int)->dict:
    if nombre in inventario.keys():
        inventario[nombre][PRICE] = str(precio)
    return inventario

def calcular_valor_inventario(inventario:dict[str,dict[str,str]])->float:
    resultado:float = 0
    valores = inventario.values()
    for valor in valores:
        resultado += int(valor[AMOUNT]) * int(valor[PRICE]) 
    return resultado
