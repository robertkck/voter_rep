FROM rocker/binder:3.6.0
LABEL maintainer='Robert Kalcik'
USER root
COPY . ${HOME}
RUN chown -R ${NB_USER} ${HOME}
USER ${NB_USER}
RUN wget https://github.com/robertkck/voter_rep/raw/master/DESCRIPTION && R -e "options(repos = list(CRAN = 'http://mran.revolutionanalytics.com/snapshot/2019-06-25/')); devtools::install_deps()"
