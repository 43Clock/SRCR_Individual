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

#Preencher adjacencias com todas as ruas
for k in contentores.keys():
    adjacencias[k] = set()

for k in contentores.keys():
    closest = []
    for i in range(0,3):
        r = ruaMaisProxima(k,closest)
        closest.append(r)
    for item in closest:
        adjacencias[k].add(item)

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
                f.write(f"arco({id_rua[k]},{id_rua[v]},{distEntreRuas(k,v)}).\n")

with open("ruaIdNome.pl","w+") as f:
    f.write("%ruaIdNome -> id, nome\n")
    for k,v in id_rua.items():
        f.write(f"rua({v},\'{k}\').\n")

with open("pontosRecolha.pl","w+") as f:
    f.write("%pontoRecolha-> idRua,tipo,quantidade\n")
    for k in ponto_recolha.keys():
        for v in ponto_recolha[k]:
            f.write(f"pontoRecolha({id_rua[k]},'{v[0]}',{v[1]}).\n")
