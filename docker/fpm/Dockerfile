FROM gcc:15 AS gcc

ENV DEBIAN_FRONTEND=noninteractive

FROM intel/fortran-essentials:latest AS oneapi

# Download and install FPM
RUN wget -O /usr/bin/fpm https://github.com/fortran-lang/fpm/releases/download/v0.12.0/fpm-0.12.0-linux-x86_64-gcc-12 && chmod 555 /usr/bin/fpm

# Install valgrind
RUN apt update && apt install -y valgrind
# Install perf (part of linux-tools-generic)
RUN apt update && apt install -y linux-tools-6.8.0-60-generic
RUN unlink /usr/bin/perf && ln -s /usr/lib/linux-tools/6.8.0-60-generic/perf /usr/bin/perf

# Install gfortran

# Install Miniconda with dependencies
RUN apt update && apt install -y wget bzip2 && \
    wget https://repo.anaconda.com/miniconda/Miniconda3-latest-Linux-x86_64.sh -O miniconda.sh && \
    bash miniconda.sh -b -p /opt/conda && \
    rm miniconda.sh
ENV PATH="/opt/conda/bin:$PATH"

RUN conda tos accept --override-channels --channel https://repo.anaconda.com/pkgs/main && \
    conda tos accept --override-channels --channel https://repo.anaconda.com/pkgs/r

# Update conda and install gfortran 15 from conda-forge
RUN conda update -n base -c defaults conda && \
    conda install -c conda-forge gfortran=15

# Install LCOV for accumulated code coverage reports
RUN apt install -y lcov
