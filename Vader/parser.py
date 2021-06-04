import openpyxl
from pathlib import Path
import re
from math import sin, cos,sqrt,atan2,radians

xlsx_file = Path('Data','dataset.xlsx')
object = openpyxl.load_workbook(xlsx_file)
sheet = object.active

dados = []

adjacencias = {}#Rua -> Ruas adjacentes
contentores = {}#Rua -> Contentor(id,x,y,residuo,tipo,capacidade,quantidade)
ponto_recolha = {}#Rua -> (id,residuo,quantidade)

def calculaDist(x1,y1,x2,y2):
    return sqrt((x2-x1)**2+(y2-y1)**2)

def quantidadeTipo(tipo,contentores):
    q = 0
    for c in contentores:
        if c[3] == tipo:
            q += c[7]
    return q
    
def ruaMaisProxima(street,listOfCloset):
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

def distEntreRuas(a,b,fator):
    contA = contentores[a]
    contB = contentores[b]
    min = 9999999999
    res = 0
    for ca in contA:
        for cb in contB:
            if calculaDist(ca[1],ca[2],cb[1],cb[2])<min:
                min = calculaDist(ca[1],ca[2],cb[1],cb[2])
    return round(min*fator)


#Read xlsx
for row in sheet.iter_rows(min_row = 1,max_row = 420):
    line = []
    for cell in row:
        line.append(cell.value)
    dados.append(line)

dados.pop(0)    

#Cria contentores
for linha in dados:
    street = re.match(r"\d+\ ?\:\ ?([a-zA-z0-9ºªáàâãéèêíïóôõöúçñÁÀÂÃÉÈÊÍÏÓÔÕÖÚÇÑ.\-, \\/]+)(\(.+\:(.+)-(.+)\))?",linha[4]).group(1).strip()
    contentor = (int(linha[2]),float(linha[0]),float(linha[1]),linha[5],linha[6],int(linha[7]),int(linha[8]),int(linha[9]))
    if(re.search(r",",street)):
        street = re.split(",",street)[0].strip()
    if street not in contentores:
        contentores[street] = set()
    contentores[street].add(contentor)

for k in contentores.keys():
    adjacencias[k] = set()

for line in dados:
    street = line[4].split(':',1)
    if re.match(r".+\(.+\)",street[1])  :
        match = re.match(r"([a-zA-z0-9ºªáàâãéèêíïóôõöúçñÁÀÂÃÉÈÊÍÏÓÔÕÖÚÇÑ.\-, \\/]+)\(.+\:(.+)-(.+)\)",street[1])
        first = match.group(1).strip()
        second = match.group(2).strip()
        third = match.group(3).strip()
        if first != second:
            if first in adjacencias and second in adjacencias: 
                adjacencias[first].add(second)  
                adjacencias[second].add(first)
        if first != third:
            if first in adjacencias and third in adjacencias: 
                adjacencias[first].add(third)
                adjacencias[third].add(first)





#Preencher adjacencias com todas as ruas
for k in contentores.keys():
    adjacencias[k] = set()


for k,v in adjacencias.items():
    if len(v) == 0:
        closest = []
        for i in range(0,2):
            r = ruaMaisProxima(k,closest)
            closest.append(r)
        for item in closest:
            adjacencias[k].add(item)
            
for k,v in adjacencias.items():
    for k2,v2 in adjacencias.items():
        if k != k2:
            if k in v2 and k2 not in v:
                adjacencias[k].add(k2)
            if k not in v2 and k2 in v:
                adjacencias[k2].add(k)

ordemAlf = []
for k in adjacencias.keys():
    ordemAlf.append(k)

ordemAlf.sort()

id_rua = {}

id = 1
for k in ordemAlf:
    id_rua[k] = id
    id+=1

adjacencias_aux = {}
for k in adjacencias.keys():
    adjacencias_aux[k] = set()
    for v in adjacencias[k]:
        if v in adjacencias_aux:
            if k not in adjacencias_aux[v]:
                adjacencias_aux[k].add(v)

for k in contentores.keys():
    s = set()
    for v in contentores[k]:
        s.add(v[3])
    ponto_recolha[k] = set()    
    for a in s:
        ponto_recolha[k].add((a,quantidadeTipo(a,contentores[k])))


with open("arcos.pl","w+") as f:
    f.write("%arco -> id,id,dist\n")
    for k in adjacencias_aux.keys():
        for v in adjacencias_aux[k]:
            if len(v)>0:
                f.write(f"arco({id_rua[k]},{id_rua[v]},{distEntreRuas(k,v,100000)}).\n")
    f.write(f"arco(0,{id_rua['R do Alecrim']},1).\n")
    f.write(f"arco(70,{id_rua['R Quintinha']},1).\n")
    f.write("%estimaDeposito -> id,Dist\n")

    f.write(f"estimaDeposito(0,{distEntreRuas('R do Alecrim','R Quintinha',10000)}).\n")
    for k in id_rua.keys():
        f.write(f"estimaDeposito({id_rua[k]},{distEntreRuas('R Quintinha',k,10000)}).\n")
    f.write(f"estimaDeposito(70,{distEntreRuas('R Quintinha','R Quintinha',10000)}).\n")

    f.write("%estimaGaragem -> id,Dist\n")
    f.write(f"estimaGaragem(0,{distEntreRuas('R do Alecrim','R do Alecrim',10000)}).\n")
    for k in id_rua.keys():
        f.write(f"estimaGaragem({id_rua[k]},{distEntreRuas('R do Alecrim',k,10000)}).\n")   
    f.write(f"estimaGaragem(70,{distEntreRuas('R do Alecrim','R Quintinha',10000)}).\n")          

with open("ruaIdNome.pl","w+") as f:
    f.write("%ruaIdNome -> id, nome\n")
    for k,v in id_rua.items():
        f.write(f"rua({v},\'{k}\').\n")

with open("pontosRecolha.pl","w+") as f:
    f.write("%pontoRecolha-> idRua,tipo,quantidade\n")
    for k in ponto_recolha.keys():
        for v in ponto_recolha[k]:
            f.write(f"pontoRecolha({id_rua[k]},'{v[0]}',{v[1]}).\n")
