FROM rocker/binder:3.6.0
LABEL maintainer='Jason Geller'
USER root
COPY . ${HOME}
RUN chown -R ${NB_USER} ${HOME}
USER ${NB_USER}



RUN wget https://github.com/jgeller112/SF_Expt1/raw/master/DESCRIPTION && R -e "options(repos = list(CRAN = 'http://mran.revolutionanalytics.com/snapshot/2019-08-29/')); devtools::install_deps()"

RUN rm DESCRIPTION.1
