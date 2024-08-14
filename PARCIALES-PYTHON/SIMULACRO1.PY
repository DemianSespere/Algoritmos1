#Ejercicio 1 
#----------ULTIMA APARACIÓN-------------------
def ultima_aparacion(s:list[int],e:int)->int:
    for i in range(0,len(s),1):
        if s[i]==e:
            res = i
    return res

#Ejercicio 2 
#----------------------ELEMENTOS EXCLUSIVOS------------------------
def elementos_exclusivos(s:list[int],t:list[int])->list[int]:
    res:list[int] = []
    for i in range(0,len(s),1):
        if (s[i] not in t) and (s[i] not in res):
            res.append(s[i])
    for j in range(0,len(t),1):
        if (t[j] not in s) and (t[j] not in res):
            res.append(t[j])
    return res

#Ejercicio 3
#-----------------CONTAR TRADUCCIONES IGUALES--------------------------
def contar_traducciones_iguales(ingles:dict[str,str],aleman:dict[str,str])->int:
    res:int = 0
    pares_aleman = aleman.items()
    pares_ingles = ingles.items()
    for par in pares_aleman:
        if par in pares_ingles:
            res +=1
    return res

#Ejercicio 4
#-----------------CONVERTIR A DICCIONARIO-----------------------------------
def convertir_a_diccionario(lista:list[int])->dict[int,int]:
    res:dict[int,int] = {}
    for i in range(0,len(lista),1):
        if lista[i] not in res.keys():
            res[lista[i]] = 1
        else:
            res[lista[i]] +=1
    return res

