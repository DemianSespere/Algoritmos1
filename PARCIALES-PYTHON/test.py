def split_mio(content:str)->list[str]:
    res = []
    terminopal = None
    i = 0
    palabra = ""
    while i < len(content):
        if content[i] == " " or content[i] == "\n":
            i+=1
        else:
            while i != (len(content)) and content[i] != " " and content[i] != "\n":
                palabra += content[i]
                i +=1
                terminopal=True
        if terminopal == True:
            terminopal = False
            res.append(palabra)
            palabra = ""
    return res


print(split_mio("probando con comas para ver el codigo"))