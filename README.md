# MAE116 – Mega-Sena

Material didático e análise reprodutível para a disciplina **MAE116 – Noções de Estatística**, 2º semestre de 2026.

O projeto utiliza resultados históricos da Mega-Sena para explorar conceitos introdutórios de probabilidade, comparação entre modelo probabilístico e dados observados, variabilidade amostral e raciocínio estatístico.

## Atividade online

A versão HTML da atividade está disponível em:

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

A análise utiliza os resultados oficiais dos concursos da Mega-Sena divulgados
pela Caixa Econômica Federal.

Na versão utilizada para esta atividade, a base contém 3.048 concursos,
realizados entre 11/03/1996 e 23/08/2026.

Fonte oficial:

https://loterias.caixa.gov.br/Paginas/mega-sena.aspx

API utilizada pelo portal:

https://servicebus2.caixa.gov.br/portaldeloterias/api/megasena

Por questões de redistribuição, o arquivo `Mega-Sena.xlsx` não é incluído neste
repositório.

Para reproduzir a análise, obtenha a base de dados na fonte oficial, salve-a
com o nome

`Mega-Sena.xlsx`

e coloque o arquivo na raiz do projeto.

## Licença

O código-fonte deste repositório, incluindo os arquivos `.R` e trechos de código
nos arquivos `.Rmd`, é disponibilizado sob a licença MIT.

O conteúdo didático, incluindo textos, atividades, explicações, tabelas e
materiais produzidos para a disciplina, é disponibilizado sob a licença
Creative Commons Attribution 4.0 International (CC BY 4.0).

Os dados da Mega-Sena não fazem parte desta licença. A fonte dos dados é a
Caixa Econômica Federal.

## Estrutura do projeto

```text
MAE116-MegaSena/
├── DataSet-MegaSena.Rproj
├── analise_megasena.R
├── atividade_megasena.Rmd
├── README.md
├── .gitignore
└── resultados/