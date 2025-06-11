FROM intel/fortran-essentials:latest AS oneapi

# Download and install FPM
RUN wget -O /usr/bin/fpm https://github.com/fortran-lang/fpm/releases/download/v0.12.0/fpm-0.12.0-linux-x86_64-gcc-12 && chmod 555 /usr/bin/fpm

# Install valgrind
RUN DEBIAN_FRONTEND=noninteractive apt update && apt install -y valgrind
