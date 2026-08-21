# Compilador de COOL
Um compilador para a linguagem de programação COOL (Classroom Object Oriented Language), feito em Haskell.

Esse projeto está sendo realizado como parte da disciplina de Compiladores na *UFF*.

## Fases do projeto

* __Fase 1: Analisador Léxico__ ✔
* __Fase 2: Analisador Sintático__ ✘
* __Fase 3: Analisador Semântico__ ✘
* __Fase 4: Geração de Código__ ✘

## Dependências
*[Nix](https://nixos.org/) usando [flakes](https://nixos.wiki/wiki/Flakes)

## Utilização

Para utilizar o compilador (por enquanto consistindo do analisador léxico), usando Nix:
```bash
nix run . local/do/arquivo.cl
```
