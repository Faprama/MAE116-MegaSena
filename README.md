# MAE116 – Mega-Sena

Material didático e análise reprodutível para a disciplina **MAE116 – Noções de Estatística**.

O projeto utiliza resultados históricos da Mega-Sena para explorar conceitos introdutórios de probabilidade, variabilidade aleatória, comparação entre modelo probabilístico e dados observados e raciocínio estatístico.

O objetivo da atividade não é estudar loterias nem estratégias de aposta, mas utilizar um contexto familiar para desenvolver raciocínio probabilístico e estatístico, comparando probabilidades previstas por um modelo com frequências observadas em dados reais e discutindo a variabilidade esperada em sequências finitas de sorteios.

## Atividade online

**Versão HTML da atividade:**

https://faprama.github.io/MAE116-MegaSena/

## Objetivos

A atividade foi construída para discutir, entre outros temas:

- espaço amostral e equiprobabilidade;
- probabilidade de eventos em amostragem sem reposição;
- frequências relativas observadas;
- comparação entre probabilidades teóricas e frequências históricas;
- distribuição do número de dezenas pares;
- distribuição da menor e da maior dezena sorteada;
- média das seis dezenas;
- ocorrência de dezenas consecutivas;
- combinações específicas que podem parecer “mais” ou “menos” aleatórias;
- apostas múltiplas e número de combinações simples;
- relação entre apostas que acertam a Sena e combinações premiadas com Quina e Quadra;
- interpretação de uma região simultânea de referência obtida por simulação.

## Questão orientadora

> Até que ponto as frequências observadas nos resultados históricos da Mega-Sena são compatíveis com as probabilidades calculadas a partir de um modelo em que todos os sorteios de seis dezenas entre 01 e 60 são igualmente prováveis?

## Base de dados

A análise utiliza os resultados oficiais dos concursos da Mega-Sena divulgados pela Caixa Econômica Federal.

Na versão utilizada para esta atividade, a base contém **3.048 concursos**, realizados entre **11/03/1996 e 23/08/2026**.

Fonte oficial:

https://loterias.caixa.gov.br/Paginas/mega-sena.aspx

API utilizada pelo portal:

https://servicebus2.caixa.gov.br/portaldeloterias/api/megasena

Por questões de redistribuição, o arquivo `Mega-Sena.xlsx` não é incluído neste repositório.

Para reproduzir a análise, obtenha a base de dados na fonte oficial, salve-a com o nome

```text
Mega-Sena.xlsx
```

e coloque o arquivo na raiz do projeto.

O script `analise_megasena.R` espera encontrar esse arquivo nesse local e verifica automaticamente a presença das colunas necessárias e a consistência básica dos dados antes de realizar a análise.

## Estrutura do projeto

```text
MAE116-MegaSena/
├── DataSet-MegaSena.Rproj
├── analise_megasena.R
├── atividade_megasena.Rmd
├── index.html
├── README.md
├── .gitignore
├── LICENSE-MIT.txt
├── LICENSE-CC-BY-4.0.txt
└── resultados/
```

### Arquivos principais

`analise_megasena.R`

Realiza a leitura e validação da base de dados, os cálculos probabilísticos, as comparações entre modelo e dados, a simulação da região simultânea de referência e a geração das tabelas e figuras.

`atividade_megasena.Rmd`

Documento didático em R Markdown. O arquivo carrega `analise_megasena.R` e gera a versão HTML da atividade.

`index.html`

Versão HTML publicada da atividade.

`resultados/`

Diretório contendo tabelas e gráficos produzidos pelo script.

## Como reproduzir a análise

É necessário ter o **R** e o **RStudio** instalados.

Os pacotes utilizados são:

```r
readxl
dplyr
tidyr
lubridate
ggplot2
```

Caso ainda não estejam instalados:

```r
install.packages(
c(
"readxl",
"dplyr",
"tidyr",
"lubridate",
"ggplot2"
)
)
```

Depois de colocar `Mega-Sena.xlsx` na raiz do projeto, abra:

```text
DataSet-MegaSena.Rproj
```

no RStudio.

Em seguida, execute:

```r
source("analise_megasena.R")
```

O script:

1. verifica a estrutura e a consistência básica da base;
2. calcula probabilidades teóricas;
3. calcula frequências observadas;
4. executa a simulação utilizada na região simultânea de referência;
5. gera tabelas;
6. produz os gráficos na pasta `resultados/`.

Para gerar novamente o material didático em HTML, abra:

```text
atividade_megasena.Rmd
```

e utilize **Knit to HTML** no RStudio.

O projeto foi testado em uma sessão limpa do R, sem depender de objetos previamente existentes no Environment.

## Reprodutibilidade

A simulação utilizada para construir a região simultânea de referência usa uma semente fixa:

```r
set.seed(116)
```

Na versão atual são utilizadas **2.000 simulações**.

A fixação da semente permite reproduzir os resultados simulados utilizados na atividade.

## Modelo probabilístico

O modelo básico considera que todas as combinações de seis dezenas distintas entre 1 e 60 são igualmente prováveis.

O número de resultados possíveis é

\[
\binom{60}{6}
=
50.063.860.
\]

Uma dezena específica aparece em um sorteio com probabilidade

\[
\frac{6}{60}
=
0,10.
\]

A atividade compara probabilidades calculadas nesse modelo com as frequências observadas nos concursos históricos.

## Interpretação

Diferenças entre probabilidades teóricas e frequências históricas são esperadas em sequências finitas de sorteios.

A existência de diferenças entre as frequências observadas das 60 dezenas, por si só, não constitui evidência de que o modelo equiprovável seja inadequado.

A questão relevante é avaliar se a magnitude dessas diferenças é plausível em relação à variabilidade esperada sob o modelo.

Quando muitas características são examinadas simultaneamente, é necessário levar em conta essa multiplicidade. Por isso, a atividade utiliza uma região simultânea de referência obtida por simulação para as frequências das 60 dezenas.

## Apostas múltiplas

Uma aposta com \(m\) dezenas contém

\[
\binom{m}{6}
\]

combinações simples de seis dezenas.

Por exemplo:

| Dezenas escolhidas | Combinações simples |
|---:|---:|
| 6 | 1 |
| 7 | 7 |
| 8 | 28 |
| 10 | 210 |

A probabilidade de acertar a Sena cresce na mesma proporção que o número de combinações simples contidas na aposta.

Se uma aposta múltipla contém as seis dezenas sorteadas, ela pode gerar, além da Sena, combinações premiadas com Quina e Quadra.

Por exemplo, uma aposta de 8 dezenas que contém as seis dezenas sorteadas inclui:

- 1 Sena;
- 12 Quinas;
- 15 Quadras.

Esse exemplo permite distinguir duas perguntas diferentes:

- qual é a probabilidade de uma aposta de \(m\) dezenas conter 4, 5 ou 6 das dezenas sorteadas;
- quantas combinações premiadas com Sena, Quina e Quadra existem dentro de uma aposta múltipla.

## Uso didático

O material foi desenvolvido para uso na disciplina:

**MAE116 – Noções de Estatística**

2º semestre de 2026

Professores:

- Anatoli Iambartsev
- Fábio Machado

O objetivo principal não é estudar estratégias de loteria, mas utilizar um contexto conhecido para desenvolver raciocínio probabilístico e estatístico.

## Licença

Este repositório utiliza duas licenças.

### Código

O código-fonte, incluindo os arquivos `.R` e os trechos de código presentes nos arquivos `.Rmd`, é disponibilizado sob a **MIT License**.

Consulte:

```text
LICENSE-MIT.txt
```

### Material didático

Os textos, atividades, explicações, tabelas e demais conteúdos didáticos são disponibilizados sob a licença **Creative Commons Attribution 4.0 International (CC BY 4.0)**.

Consulte:

```text
LICENSE-CC-BY-4.0.txt
```

### Dados

Os dados da Mega-Sena não são redistribuídos neste repositório e não estão cobertos pelas licenças acima.

A fonte dos dados é a Caixa Econômica Federal.