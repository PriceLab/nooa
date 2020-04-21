FROM rocker/shiny
WORKDIR /home/shiny
RUN mkdir /home/shiny/data
COPY installPrerequisites.R .
RUN R -f ./installPrerequisites.R .
COPY packages/cyjShiny_1.0.15.tar.gz .
RUN R CMD INSTALL cyjShiny_1.0.15.tar.gz
COPY kengoShiny.R .
COPY tbls-curated-v2.RData .
RUN R -f ./installPrerequisites.R .
CMD ["R", "-e", "shiny::runApp('/home/shiny/kengoShiny.R', port=3838, host='0.0.0.0')"]
