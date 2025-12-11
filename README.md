# BCC328 - Construção de Compiladores I

## Introdução

Este repositório contém a configuração básica do ambiente
de desenvolvimento para a disciplina BCC328 - Construção
de compiladores I. Este ambiente provê as seguintes
ferramentas:

- `cabal`: Gerenciamento de projetos e dependências Haskell.

- `alex`: Gerador de analisador léxico para Haskell.

- `happy`: Gerador de analisador sintático para Haskell.

- `wabt`: Conjunto de ferramentas para desenvolvimento WebAssembly.

- `riscv64-linux-gnu-gcc`: Compilador para RISC-V, utilizado como um assembler para o assembly gerado pelos compiladores desenvolvidos.

- `qemu`: Emulador de arquitetura RISC-V.

## Acessando o ambiente

Para utilizar o ambiente, execute os seguintes comandos na mesma pasta que
estão os arquivos `Dockerfile` e `docker-compose.yml`:

Na primeira execução do ambiente execute com a flag `--build` para criação
da imagem.

```
docker-compose up -d --build
```
```
```

Após a criação da imagem, você poderá inicializar o ambiente usando:

```
docker-compose up -d
```

Depois de inicializado, acesso o shell da imagem com o comando:

```
docker-compose exec haskell-dev bash
```

## Estrutura do projeto


Todo o código está definido dentro da pasta `src`. Vamos ver a estrutura das pastas:

- Automata: Implementação de algoritmos para análise léxica. Utilizado nas aulas para uma
  compreensão de como os geradores de analisadores léxicos são implementados.

- Exp: Implementação de um compilador de expressões. Essa implementação está completa.
  Tem diferentes analisadores sintáticos e gerador de código WASM e Risc-V.

- Line: Implementação de um compilador de uma linguagem com atribuição, read e print.

- Markup: Implementação de um gerador de slides a partir de arquivos markdown, utilizado
  nas primeiras aulas de Haskell, como uma revisão para a linguagem.

- PEG: Implementação de uma biblioteca para Parsing Expression Grammars.

- Parsing: Implementação de algoritmos de parsing (recursivo e outros algoritmos baseados em tabela)

- Utils: Utilidades usadas por demais pastas

- While: Implementação de um compilador para uma linguagem imperativa simples.

Muitas dessas pastas definem um executável que pode ser compilado e executado usando o cabal. Se você ver o arquivo `bcc328.cabal`, lá tem a especificação de vários executáveis. Por exemplo:

```
```
executable exp
    -- Import common warning flags.
    import:          common-opts

    -- .hs or .lhs file containing the Main module.
    main-is:          Exp.hs

    -- Modules included in this executable, other than Main.
    -- other-modules:

    -- LANGUAGE extensions used by modules in this package.
    -- other-extensions:

    -- Other library packages from which modules are imported.
    build-depends:
        base ^>=4.18.3.0,
        bcc328

    -- Directories containing source files.
    hs-source-dirs:   src/Exp

    -- Base language which the package is written in.
    default-language: Haskell2010
```
```


Mostra como é definido o executável do compilador de expressões. Você pode executar esse exemplo usando o comando

```
```
cabal run exp
```
```

que vai apresentar as diferentes opções de linha de comando para esse executável.

## Slides

A disciplina utiliza slides escritos utilizando o `org-mode` do Emacs. Para gerar um pdf destes slides,
sugiro a utilização do pandoc, um conversor entre diferentes formatos textuais.
Tendo o pandoc instalado em sua máquina e um compilador de LaTeX, o comando:

```
pandoc slide.org -t beamer -o slide.pdf
```

irá gerar o slide pdf para o arquivo `slide.org`.
