ID = 21apr2020
IMAGE_NAME = kengo_longevity_$(ID)

default:
	@echo targets:  shiny buildDocker

shiny:
	R -f kengoShiny.R


build:
	docker build -t $(IMAGE_NAME) -f Dockerfile .


bash:
	docker run -it \
           -p 3838:9012 \
           --entrypoint /bin/bash \
           $(IMAGE_NAME)

dockerRun:
	docker run -p $(TESTPORT):3838 $(IMAGE_NAME)

