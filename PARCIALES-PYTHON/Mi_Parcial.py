from queue import Queue as Cola


#Funciones auxiliares para ver que pasa en el duelo
def choque(estrategia1:str,estrategia2:str)->bool: 
    res:bool = False
    if estrategia1 == "me la banco y no me desvio" and estrategia2 == "me la banco y no me desvio":
        res = True
    return res 
def ambos_gallinas(estrategia1:str,estrategia2:str)->bool:
    res:bool = False
    if estrategia1 == "me desvio siempre" and estrategia2 == "me desvio siempre":
        res = True
    return res

def torneo_de_gallinas(estrategias: dict[str,str]) -> dict[str,int]:
    jugadores:list[str] = estrategias.keys()
    estrategia:list[str] = estrategias.values()
    contador:int = 0
    res:dict[str,int] = {}
    for jugador in jugadores:
        jugada:str = estrategias[jugador] 
        #Veo el duelo del jugador contra el resto
        for i in estrategia: 
            if choque(jugada,i):
                contador -=5
            elif ambos_gallinas(jugada,i):
                contador -=10
            else: #uno solo fue gallina y lo veo a continuacion
                if jugada == "me la banco y no me desvio":
                    contador +=10
                else: 
                    contador -=15
        #Aca le sumo puntos porque en el anterior ciclo for, tomo en cuenta su propio duelo por lo tanto le sumo los puntos que perdio por ese duelo. 
        if jugada == "me la banco y no me desvio": 
            contador += 5 
        else:
            contador += 10
        res[jugador] = contador #agrego al resultado el jugador con sus respesctivos puntos
        contador = 0 #reinicio el contador
    return res

# Ejercicio 2
#Funcion auxiliar que copia una cola
def cola_copy(c:Cola[tuple[str,str]])->Cola[tuple[str,str]]:
    copy:Cola[tuple[str,str]] = Cola()
    contenido:list[tuple[str,str]] = []
    while not c.empty():
        contenido.append(c.get())
    for elem in contenido:
        c.put(elem)
        copy.put(elem)
    return copy
#Funcion auxliar que me devuelve True si la persona es vip
def es_vip(persona:tuple[str,str])->bool:
    res:bool = False
    if persona[1] == "vip":
        res = True
    return res


def reordenar_cola_priorizando_vips(fila_clientes: Cola[tuple[str,str]]) -> Cola[str]:
    #Copio la cola ya que el parametro es de tipo in y si le aplico la funcion a la cola y luego quiero ver la cola original me la tiene que devolver.
    #Por lo tanto para no tener que restaurarla despues trabajo con una copia.
    cola_aux:Cola[tuple[str,str]] = cola_copy(fila_clientes) 
    res:Cola[str] = Cola()
    cola_vip:Cola[str] = Cola()
    cola_comun:Cola[str] = Cola()
    while not cola_aux.empty():
        persona_afiliado:tuple[str,str] = cola_aux.get()
        persona = persona_afiliado[0] #Consigo solamente el nombre para luego colocarlo en su respectiva cola
        if es_vip(persona_afiliado):
            cola_vip.put(persona)
        else:
            cola_comun.put(persona)
    #Vacio la cola vip y comun en orden de llegada y primero el vip para que salga primero de res 
    while not cola_vip.empty():  
        res.put(cola_vip.get())
    while not cola_comun.empty():
        res.put(cola_comun.get())
    return res

# Ejercicio 3
#Creo la funcion split que en mi caso solo separa de texto las palabras separadas por espacio, por salto de linea o por comas
def split_casero(content:str)->list[str]:
    resultado:list[str] = []
    palabra:str = ""
    i:int = 0
    termino:bool = None
    while i < len(content):
        if content[i] == " " or content[i] == "\n" or content[i] == ",":
            i += 1
        while i != (len(content)) and content[i] != " " and content[i] != "\n" and content[i] != ",":
            palabra+=content[i]
            termino = True
            i+=1
        if termino == True:
            resultado.append(palabra)
            palabra=""
            termino=False
    return resultado
#Funcion auxiliar que pasada una palabra me devuelve sus palindromos
def sufijos(palabra:str)->list[str]:
    res:list[str] = []
    sufijo:str = ""
    start:int = 0
    ultima_indice = len(palabra)-1
    while not palabra[ultima_indice] in res:
        for i in range(start,len(palabra)):
            sufijo += palabra[i]
        res.append(sufijo)
        sufijo = ""
        start += 1
    return res      
#Funcion auxiliar que me devuelve True si el sufijo es palindromo
def es_palindromo(sufijo:str)->bool:
    res:bool = True
    n:int = len(sufijo)
    for i in range(0,n//2,1):
        if sufijo[i] != sufijo[n-1-i]:
            res = False
    return res

def cuantos_sufijos_son_palindromos(texto: str) -> int:
    res:int = 0
    contador:int = 0
    listado:list[str] = split_casero(texto)
    for palabra in listado:
        listado_sufijos:list[str] = sufijos(palabra)
        for sufijo in listado_sufijos:
            if es_palindromo(sufijo):
                res +=1
    return res

# Ejercicio 4
#Funciones auxiliares que me devuelve True si el caracter es 'X' o 'O'
def es_X(letra:str)->bool:
    res:bool = False
    if letra == 'X':
        res = True
    return res
def es_O(letra:str)->bool:
    res:bool = False
    if letra == 'O':
        res = True
    return res

def quien_gano_el_tateti_facilito(tablero: list[list[str]]) -> int:
    res:int = 0
    for i in range(len(tablero)-2):#Voy fila por fila en el tablero (las i son las filas) #Pongo el -2 ya que si no gano hasta esa fila, no puede hacerlo en las siguientes
        for j in range(len(tablero)): #Recorro cada columna en la misma fila (las j son las columnas)
            if es_X(tablero[i][j]):
                if es_X(tablero[i+1][j]) and es_X(tablero[i+2][j]): #Chequeo si hay 3 'X' consecutivas en forma vertical
                    if res==2: #Es un chequeo si hubo trampa, ya que si es 2 el juego ya tuvo que terminar
                        res = 3
                    else:  #Si pasa los dos chequeos la ganadora es Ana
                        res = 1
            elif es_O(tablero[i][j]):
                if es_O(tablero[i+1][j]) and es_O(tablero[i+2][j]):  #Chequeo si hay 3 'O' consecutivas en forma vertical
                    if res==1:  #Es un chequeo si hubo trampa, ya que si es 2 el juego ya tuvo que terminar
                        res=3
                    else:#Si pasa los dos chequeos el ganador es Beto
                        res = 2
    return res
