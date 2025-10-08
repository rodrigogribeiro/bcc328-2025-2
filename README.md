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
```


```
