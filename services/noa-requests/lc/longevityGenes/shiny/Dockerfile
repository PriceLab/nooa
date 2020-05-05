FROM rocker/shiny
WORKDIR /home/shiny
COPY tbl.summary.252x3.RData .
COPY installPrerequisites.R .
COPY lcGenes.R .
RUN R -f ./installPrerequisites.R .
CMD ["R", "-e", "shiny::runApp('/home/shiny/lcGenes.R', port=3838, host='0.0.0.0')"]





