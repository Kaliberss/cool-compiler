# Compilador de COOL
Um compilador para a linguagem de programação COOL (Classroom Object Oriented Language), feito em Haskell.

Esse projeto está sendo realizado como parte da disciplina de Compiladores na *UFF*.

## Fases do projeto

* __Fase 1: Analisador Léxico__ ✔
* __Fase 2: Analisador Sintático__ ✔
* __Fase 3: Analisador Semântico__ ✘
* __Fase 4: Geração de Código__ ✘

## Dependências
* GHC
* Cabal
* Nix usando flakes (Opcional, mas recomendado: instala as dependências acima automaticamente)

## Utilização

Para utilizar o compilador (por enquanto consistindo do analisador léxico), usando Nix:
```bash
git clone https://github.com/Kaliberss/cool-compiler.git
cd cool-compiler
nix run . local/do/arquivo.cl
```

Para criar um ambiente de desenvolvimento com as ferramentas necessárias, usando Nix:

```bash
nix develop
```
Dentro do devshell (ou com as dependências instaladas globalmente), para compilar e executar:

```bash
cabal build
cabal run . local/do/arquivo.cl
``
