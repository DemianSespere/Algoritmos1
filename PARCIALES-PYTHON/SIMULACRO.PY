#Ejercicio 1
#----------------MEJORES PRECIOS----------------------
def mejores_precios(super1: list[(str,float)],super2: list[(str,float)])->list[(str,float)]:
    res:list[(str,float)] = []
    for producto in range(0,len(super1),1):
        if super1[producto][1]<=super2[producto][1]:
            res.append(super1[producto])
        else: 
            res.append(super2[producto])
    return res

#Ejercicio 2
#----------------------SEGUIDILLA-----------------------
def seguidilla(calificaciones:list[int],nota_minima:int)->int:
    contador:int = 0
    res: int = 0
    long = len(calificaciones)
    for nota in range(0,long,1):
        if calificaciones[nota] >= nota_minima:
            contador +=1
        else:
            if contador>res:
                res = contador
            contador = 0
    if contador>res:
        res=contador
    return res

#Ejercicio 3
#--------------------POSICIONES PARES-------------------
def elem_en_pos_pares(matriz:list[list[int]],elem:int)->list[bool]:
    res:list[bool] = []
    indice:int = -1
    for lista in matriz:
        res.append(False)
        indice +=1
        for i in range(0,len(lista),1):
            if (lista[i]==elem) and (i%2==0):
                res[indice]=True
    return res

#Ejercicio 4
#------------------------MOLINETE---------------------------
def viajes_por_dia(viajes_diarios: dict[int,list[str]],usuarios:list[str])-> dict[str,int]:
    listas_de_pasajes = viajes_diarios.values()
    
    pasajeros:dict[str,int] = {}
    contador:int = 0
    for i in range(0,(len(usuarios))):
        for dia in listas_de_pasajes:
            if (usuarios[i]) in dia:
                contador+=1        
        pasajeros[usuarios[i]]=contador
        contador = 0
    return pasajeros
        
        