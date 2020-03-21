# depends on this virtual environment:
#   source ~/github/nooa/py3610/bin/activate

validate:
	docker run --rm -v ${PWD}:/local openapitools/openapi-generator-cli validate -i /local/swagger.yaml

codeGen: codeGenServer codeGenClient

codeGenServer:
	docker run --rm -v ${PWD}:/local openapitools/openapi-generator-cli generate \
	  -i /local/swagger.yaml \
          -g python-flask -o /local/out/python

codeGenClient:
	docker run --rm -v ${PWD}:/local openapitools/openapi-generator-cli generate \
           -i /local/swagger.yaml \
           -g python -o /local/out/python

codeGenHelp:
	docker run --rm -v ${PWD}:/local openapitools/openapi-generator-cli help 

server:
	(export PYTHONPATH="out/python"; python3 out/python/openapi_server/__main__.py &)

pets:
	open http://localhost:8080/v1/pets

