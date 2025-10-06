FROM ubuntu:22.04

# Set environment variables
ENV DEBIAN_FRONTEND=noninteractive
ENV PATH="/root/.cabal/bin:/root/.ghcup/bin:$PATH"
ENV GHCUP_INSTALL_BASE_PREFIX="/root"

# Install system dependencies
RUN apt-get update && apt-get install -y \
    curl \
    wget \
    build-essential \
    libffi-dev \
    libgmp-dev \
    zlib1g-dev \
    libtinfo-dev \
    git \
    python3 \
    cmake \
    xz-utils \
    && rm -rf /var/lib/apt/lists/*

# Install RISC-V cross-compilation toolchain and QEMU emulator
RUN apt-get update && apt-get install -y \
    gcc-riscv64-linux-gnu \
    g++-riscv64-linux-gnu \
    binutils-riscv64-linux-gnu \
    qemu-user \
    qemu-user-static \
    qemu-system-riscv64 \
    && rm -rf /var/lib/apt/lists/*

# Install GHCup (Haskell toolchain installer)
RUN curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh

# Install GHC, cabal, and stack
RUN ghcup install ghc recommended
RUN ghcup install cabal recommended
RUN ghcup install stack recommended
RUN ghcup set ghc recommended

# Update cabal package list
RUN cabal update

# Install alex and happy (Haskell lexer and parser generators)
RUN cabal install alex happy

# Install WABT (WebAssembly Binary Toolkit)
WORKDIR /tmp
RUN wget https://github.com/WebAssembly/wabt/releases/download/1.0.34/wabt-1.0.34-ubuntu.tar.gz \
    && tar -xzf wabt-1.0.34-ubuntu.tar.gz \
    && cp wabt-1.0.34/bin/* /usr/local/bin/ \
    && rm -rf wabt-1.0.34* \
    && chmod +x /usr/local/bin/*

# Install Wasmtime
RUN wget https://github.com/bytecodealliance/wasmtime/releases/download/v25.0.1/wasmtime-v25.0.1-x86_64-linux.tar.xz \
    && tar -xf wasmtime-v25.0.1-x86_64-linux.tar.xz \
    && cp wasmtime-v25.0.1-x86_64-linux/wasmtime /usr/local/bin/ \
    && rm -rf wasmtime-v25.0.1* \
    && chmod +x /usr/local/bin/wasmtime

# Create workspace directory
WORKDIR /workspace

# Set up persistent volume mount point
VOLUME ["/workspace"]

# Expose common development ports
EXPOSE 8000 8080 3000

# Default command
CMD ["/bin/bash"]
