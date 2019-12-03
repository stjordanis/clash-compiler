# The builder stage downloads and installs ghdl and iverilog, which are
# needed by clash-testsuite. Separating the stages means the final docker
# image avoids containing extra dependencies, making it smaller.
#
# Versions of packages are given explicitly in this file. This means that any
# backports which change version info do not result in the Dockerfile building
# a slightly different image. If packages are to be updated, it must be done
# explicitly and the image rebuilt.
#
FROM ubuntu:18.04 AS builder

ARG DEBIAN_FRONTEND=noninteractive
ENV LANG=C.UTF-8 LC_ALL=C.UTF-8

RUN apt-get update                                                          \
 && apt-get install -y --no-install-recommends --no-install-suggests        \
      autoconf=2.69-11                                                      \
      bison=2:3.0.4.dfsg-1build1                                            \
      ca-certificates=20180409                                              \
      clang=1:6.0-41~exp4                                                   \
      curl=7.58.0-2ubuntu3.8                                                \
      flex=2.6.4-6                                                          \
      gnat=7ubuntu1                                                         \
      gperf=3.1-1                                                           \
      llvm-dev=1:6.0-41~exp5~ubuntu1                                        \
      make=4.1-9.1ubuntu1                                                   \
      zlib1g-dev=1:1.2.11.dfsg-0ubuntu2                                     \
 && apt-get autoremove -y --purge                                           \
 && apt-get clean                                                           \
 && rm -rf /var/lib/apt/lists/*                                             \
 && curl -L 'https://github.com/ghdl/ghdl/archive/v0.36.tar.gz'             \
      | tar xz                                                              \
 && curl -L 'https://github.com/steveicarus/iverilog/archive/v10_3.tar.gz'  \
      | tar xz

WORKDIR /ghdl-0.36/build

RUN ../configure --with-llvm-config --prefix=/opt                           \
 && make -j$(nproc)                                                         \
 && make install

WORKDIR /iverilog-10_3

RUN chmod a+x autoconf.sh                                                   \
 && ./autoconf.sh                                                           \
 && ./configure --prefix=/opt                                               \
 && make -j$(nproc)                                                         \
 && make install

# This stage copies the pre-built ghdl and iverilog from the previous stage
# into /opt and downloads the cabal-install and ghc versions to test against.
# These are not installed by default, as installing all versions we want to
# test against leads to significant bloat in the final image.
#
FROM ubuntu:18.04 AS runner

ARG DEBIAN_FRONTEND=noninteractive
ENV LANG=C.UTF-8 LC_ALL=C.UTF-8 PATH=/opt/bin:/opt/ghc/bin:/opt/cabal/bin:$PATH

COPY ppa-trusted-keys/* /etc/apt/trusted.gpg.d/
COPY --from=builder /opt/ /opt/

RUN echo "deb http://ppa.launchpad.net/hvr/ghc/ubuntu bionic main"          \
      > /etc/apt/sources.list.d/hvr-ghc-trusty.list                         \
 && apt-get update                                                          \
 && apt-get install -y --no-install-recommends --no-install-suggests        \
      dnsutils=1:9.11.3+dfsg-1ubuntu1.11                                    \
      gcc=4:7.4.0-1ubuntu2.3                                                \
      gettext=0.19.8.1-6ubuntu0.3                                           \
      git=1:2.17.1-1ubuntu0.4                                               \
      libc6-dev=2.27-3ubuntu1                                               \
      libgmp-dev=2:6.1.2+dfsg-2                                             \
      libgnat-7=7.4.0-1ubuntu1~18.04.1                                      \
      libtinfo-dev=6.1-1ubuntu1.18.04                                       \
      pkg-config=0.29.1-0ubuntu2                                            \
      sudo=1.8.21p2-3ubuntu1.1                                              \
 && apt-get autoremove -y --purge                                           \
 && apt-get clean                                                           \
 && rm /etc/apt/apt.conf.d/docker-clean                                     \
 && apt-get install -y --download-only                                      \
      cabal-install-2.4=2.4+git20181125.1.5e65672-6~18.04                   \
      cabal-install-3.0=3.0+git20190827.1.f2b2ab7-6~18.04                   \
      cabal-install-head=3.1+git20191115.2.eb2f764-6~18.04                  \
      ghc-8.4.4=8.4.4-9~18.04                                               \
      ghc-8.6.5=8.6.5-9~18.04                                               \
      ghc-8.8.1=8.8.1-12~18.04                                              \
      ghc-8.10.1=8.10.0.20191122+git.1.d092d85-16~18.04                     \
 && rm -Rf /etc/apt/sources.list

