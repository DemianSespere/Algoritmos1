#Ejercicio 1
#------------------------N-ESIMA APARICION-------------------------
def ind_nesima_aparacion(s:list[int],n:int,elem:int)->int:
    contador:int=0
    res:int = 0
    for i in range(0,len(s),1):
        if s[i]==elem:
            contador += 1
            if contador == n:
                res = i
    if contador < n:
        res = -1
    return res

#Ejercicio 2
#--------------------MEZCLAR---------------------------
def mezclar(s1:list[int],s2:list[int])->list[int]:
    res:list[int] = []
    for i in range(0,len(s1),1):
        res.append(s1[i])
        res.append(1)
    for j in range(0,len(s2),1):
        res[j*2+1]=s2[j]
    return res

#Ejercicio 3
#----------------------CARRERAS------------------------------
def frecuencia_posiciones_por_caballo(caballos:list[int],carreras:dict[str,list[str]])->dict[str,list[int]]:
    resultado:dict[str,list[int]] = {}
    for caballo in caballos:
        posiciones_caballo = [0]*len(caballos)
        for posiciones in carreras.values():
            for i in range(len(posiciones)):
                if posiciones[i]==caballo:
                    posicion:int = i
            posiciones_caballo[posicion] = posiciones_caballo[posicion] + 1
        resultado[caballo]=posiciones_caballo
    return resultado


#Ejercicio 4 
#-----------------------MATRIZ CAPICUA--------------------------
def matriz_capicua(m:list[list[int]])->bool:
    res:bool = True
    for fila in m:
        n = len(fila)
        for i in range(n//2):
            if fila[i]!=fila[n-1-i]:
                res = False
    return res

m = [[1,2,2,1],[-5,6,6,-5],[0,1,2,1,0]]

