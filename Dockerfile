FROM rocker/shiny:3.5.1

RUN apt-get update && apt-get install libcurl4-openssl-dev libv8-3.14-dev libx11-dev mesa-common-dev libglu1-mesa-dev -y &&\
    mkdir -p /var/lib/shiny-server/bookmarks/shiny

# Download and install library
RUN R -e "install.packages(c('shinydashboard', 'shinyjs', 'remotes', 'V8'))"

# Get R package from GitHub
RUN Rscript -e "remotes::install_github('SRHilz/GliomaAtlas3D')"

COPY Rprofile.site /usr/local/lib/R/etc/Rprofile.site

# make all app files readable (solves issue when dev in Windows, but building in Ubuntu)
RUN chmod -R a+rwx /usr/local/lib/R/etc

EXPOSE 3838

CMD ["R", "-e", "shiny::runApp('/usr/local/lib/R/site-library/GliomaAtlas3D/shinyApp')"]
