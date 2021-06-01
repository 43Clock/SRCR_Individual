import openpyxl
from pathlib import Path
import re
from math import sin, cos,sqrt,atan2,radians

xlsx_file = Path('Data','dataset.xlsx')
object = openpyxl.load_workbook(xlsx_file)
sheet = object.active

allLines = []

adjacencias = {}#Rua -> Ruas adjacentes
contentores = {}#Rua -> Contentor(id,x,y,residuo,tipo,capacidade,quantidade)

def calculaDist(x1,y1,x2,y2):
    return sqrt((x2-x1)**2+(y2-y1)**2)

def closestStreet(street,listOfCloset):
    min = 999999999
    res = ""
    for k,v in contentores.items():
        if k != street and k not in listOfCloset:
            for container in v:
                for containerStreet in contentores[street]:
                    if calculaDist(container[1],container[2],containerStreet[1],containerStreet[2])<min:
                        min = calculaDist(container[1],container[2],containerStreet[1],containerStreet[2])
                        res = k
    return res

def distEntreRuas(a,b):
    contA = contentores[a]
    contB = contentores[b]
    min = 9999999999
    res = 0
    for ca in contA:
        for cb in contB:
            if calculaDist(ca[1],ca[2],cb[1],cb[2])<min:
                min = calculaDist(ca[1],ca[2],cb[1],cb[2])
    return round(min*100000)

#Read xlsx
for row in sheet.iter_rows(min_row = 1,max_row = 420):
    line = []
    for cell in row:
        line.append(cell.value)
    allLines.append(line)

allLines.pop(0)    



#Cria adjacentes
for line in allLines:
    street = line[4].split(':',1)
    if re.match(r".+\(.+\)",street[1])  :
        match = re.match(r"([a-zA-z0-9ºªáàâãéèêíïóôõöúçñÁÀÂÃÉÈÊÍÏÓÔÕÖÚÇÑ.\-, \\/]+)\(.+\:(.+)-(.+)\)",street[1])
        first = match.group(1).strip()
        second = match.group(2).strip()
        third = match.group(3).strip()
        if first not in adjacencias:
            adjacencias[first] = set()
        if second not in adjacencias:
            adjacencias[second] = set()
        if third not in adjacencias:
            adjacencias[third] = set()
        if first != second:
            adjacencias[first].add(second)  
            adjacencias[second].add(first)
        if first != third:
            adjacencias[first].add(third)
            adjacencias[third].add(first)
    else:
        s = re.split(r",",street[1].strip())
        if s[0].strip() not in adjacencias:
            adjacencias[s[0].strip()] = set();


#adiciona todas as ruas ao dic de contentores
for k in adjacencias.keys():
    contentores[k] = []

#Cria contentores
for line in allLines:
    street = re.match(r"\d+\ ?\:\ ?([a-zA-z0-9ºªáàâãéèêíïóôõöúçñÁÀÂÃÉÈÊÍÏÓÔÕÖÚÇÑ.\-, \\/]+)(\(.+\:(.+)-(.+)\))?",line[4]).group(1).strip()
    contentor = (int(line[2]),float(line[0]),float(line[1]),line[5],line[6],int(line[7]),int(line[8]),int(line[9]))
    if(re.search(r",",street)):
        street = re.split(",",street)[0].strip()
    contentores[street].append(contentor)

#Vê localizações que não tem contentores
toRemove = set()
for k,v in contentores.items():
    if len(v) == 0:
        toRemove.add(k)
        for ka,va in adjacencias.items():
            if ka == k:
                toRemove.add(k)
            else:
                if k in va:
                    va.remove(k)

#Remove as localizacoes que não tem contentores da lista de adjacencia e da lista de contentores
for item in toRemove:
    del adjacencias[item]
    del contentores[item]

#Preencher sets vazios do dicionario de adjacencia com as 3 ruas que tem os contentores mais proximos
for k,v in adjacencias.items():
    if len(v) == 0:
        closest = []
        for i in range(0,3):
            r = closestStreet(k,closest)
            closest.append(r)
        for item in closest:
            adjacencias[k].add(item)

#Se ha um arco de a->b tem de haver um de b->a
for k,v in adjacencias.items():
    for k2,v2 in adjacencias.items():
        if k != k2:
            if k in v2 and k2 not in v:
                adjacencias[k].add(k2)
            if k not in v2 and k2 in v:
                adjacencias[k2].add(k)


id_rua = {}

id = 1
for k in adjacencias.keys():
    id_rua[k] = id
    id+=1


#Criar dic com apenas A->B e não B->A
adjacencias_aux = {}
for k in adjacencias.keys():
    adjacencias_aux[k] = set()
    for v in adjacencias[k]:
        if v in adjacencias_aux:
            if k not in adjacencias_aux[v]:
                adjacencias_aux[k].add(v)


""" for k,v in adjacencias.items():
    print(f"{k}-{v}")

 """

#TODO FALTAM DISTANCIAS AQUI
with open("arcos.pl","w+") as f:
    f.write("%arco -> id,id\n")
    for k in adjacencias_aux.keys():
        for v in adjacencias_aux[k]:
            if len(v)>0:
                f.write(f"arco({id_rua[k]},{id_rua[v]},{distEntreRuas(k,v)}).\n")

with open("ruas.pl","w+") as f:
    f.write("%rua -> id, nome\n")
    for k,v in id_rua.items():
        f.write(f"rua({v},\'{k}\').\n")

with open("contentores.pl","w+") as f:
    f.write("%contentor -> id,x,y,residuo,tipo,capacidade,quantidade,total\n")
    for k in contentores.keys():
        for v in contentores[k]:
            f.write(f"contentor({v[0]},{v[1]},{v[2]},\'{v[3]}\',\'{v[4]}\',{v[5]},{v[6]},{v[7]}).\n")

with open("contentorRua.pl","w+") as f:
    f.write("%contentorRua-> idContentor,idRua\n")
    for k in contentores.keys():
        for v in contentores[k]:
            f.write(f"contentorRua({v[0]},{id_rua[k]}).\n")