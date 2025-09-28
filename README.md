# Análise do desempenho de sistemas computacionais a partir da base SPEC CPU 2017: 
#            Um estudo introdutório dos resultados de speed-int

## Autores:  
David Kauan Carneiro Pereira (25103306),  
Gustavo Sonntag Dorow (25100806),  
Johan Akin Araújo da Silva Rodrigues (25103312),  
Jonathan Tenório de Lima (25102233).  

Universidade Federal de Santa Catarina (UFSC), Departamento de Informática e Estatística (INE), Graduação em Sistemas de Informação.

---

## Introdução e objetivos

  O custo decrescente dos componentes dos sistemas computacionais, em especial os processado-
res, permitiu uma contínua e intensa evolução no desempenho das máquinas perante algoritmos que
resolvem problemas cada vez mais complexos. Os blocos construtores (building blocks) dos compu-
tadores modernos são virtualmente os mesmos que deram origem ao IAS, no entanto, as técnicas
para extrair o máximo desempenho tornaram-se altamente sofisticadas (STALLINGS, 2021).

  A microarquitetura de um sistema computacional compreende o conjunto de implementações inter-
nas de um processador que definem como as instruções (ISA) são executadas no nível de hardware
e, por isso mesmo, possui papel chave no desempenho dos computadores. Os benchmarks pos-
suem papel fundamental na evolução contínua da microarquitetura dos sistemas e compiladores
(LIMAYE; ADEGBIJA, 2018).

  Um dos benchmarks mais utilizados globalmente é fornecido pela Standard Performance Evalua-
tion Corporation (Standard Performance Evaluation Corporation (SPEC), 2024), cuja última versão
(SPEC CPU 2017) avaliou 43 benchmarks diferentes conforme a tabela 1.