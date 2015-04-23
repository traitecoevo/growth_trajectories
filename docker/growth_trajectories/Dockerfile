FROM r-base

RUN apt-get update \
  && apt-get install -y --no-install-recommends  \
    ca-certificates \
    curl \
    git \
    libcurl4-openssl-dev \
    pandoc \
    ssh \
  && apt-get clean

RUN install2.r --error  \
    BH \
    devtools \
    downloader \
    dplyr \
    plyr \
    quantreg \
    R6 \
    RColorBrewer \
    Rcpp \
    RCurl \
    rmarkdown \
    smatr \
    xtable

RUN installGithub.r  \
    richfitz/remake \
    traitecoevo/dockertest

RUN r -e 'dockertest:::copy_scripts_dir("/usr/local/bin")'

COPY local /local

RUN R CMD INSTALL /local/tree2 \
  && rm -rf /local

RUN r -e 'remake:::install_remake("/usr/local/bin")'

WORKDIR /root/growth_trajectories

CMD ["bash"]
