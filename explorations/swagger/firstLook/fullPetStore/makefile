# depends on this virtual environment:
#   source ~/github/nooa/py3610/bin/activate

YAML=petstore.yaml

validate:
	docker run --rm -v ${PWD}:/local openapitools/openapi-generator-cli validate -i /local/$(YAML)

codeGen: codeGenServer codeGenClient

codeGenServer:
	docker run --rm -v ${PWD}:/local openapitools/openapi-generator-cli generate \
	  -i /local/$(YAML) \
          -g python-flask -o /local/out/python

codeGenClient:
	docker run --rm -v ${PWD}:/local openapitools/openapi-generator-cli generate \
           -i /local/$(YAML) \
           -g python -o /local/out/python

codeGenHelp:
	docker run --rm -v ${PWD}:/local openapitools/openapi-generator-cli help 

server:
	(export PYTHONPATH="out/python"; python3 out/python/openapi_server/__main__.py &)

browser:
	open http://localhost:8080/v1/pets

