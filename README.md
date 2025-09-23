Os resultados da limpesa dos dados deixou claro que o SPEC2017 tem uma tendência muito maior para processadores de servidor,
mas isso não atrapalha a ideia de mostrar o AMD vs Intel no trabalho.

### Baseline
  Configuração obrigatória para todos os participantes.
Usa parâmetros de compilação restritos e mais padronizados, para garantir comparabilidade entre diferentes processadores.
Mostra o desempenho "médio realista" sem otimizações agressivas.

### Result (ou Peak)
  Configuração mais livre, permitindo ajustes agressivos de compilador e sistema para espremer o máximo do hardware.
Representa o melhor desempenho possível (otimizações máximas).

## Benchmarks escolhidos (SPEC CPU2017), fora o Result Base e Result.

### 600.perlbench_s  
- **O que é:** Versão adaptada do interpretador **Perl 5**.  
- **O que mede:** Desempenho em **interpretação de scripts e manipulação de strings**.  
- **Relevância:** Mostra eficiência em workloads típicos de linguagens interpretadas, processamento de texto e aplicações web antigas.

---

### 602.gcc_s  
- **O que é:** Compilações de programas reais usando o compilador **GNU GCC**.  
- **O que mede:** Performance em **compilação de código C**.  
- **Relevância:** Indica a capacidade da CPU em workloads de compilação e transformação de código, importante em ambientes de desenvolvimento e build servers.

---

### 605.mcf_s  
- **O que é:** Implementa o problema de **fluxo de transporte** (*Min Cost Flow*).  
- **O que mede:** Algoritmos de otimização e programação linear.  
- **Relevância:** Muito sensível a **latência de memória e largura de banda**. CPUs com caches mais eficientes se destacam.

---

##  Por que esses três?  
- **600 (perlbench)** → mostra desempenho em código interpretado e manipulação de strings.  
- **602 (gcc)** → representa workloads **intensivos em CPU** no desenvolvimento de software.  
- **605 (mcf)** → testa **eficiência de memória** e comunicação entre caches.  

Essa combinação traz:
- Interpretação / Texto  
- Compilação / Processamento pesado  
- Otimização / Gargalo de memória
