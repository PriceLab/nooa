# depends on this virtual environment:
#   source ~/github/nooa/py3610/bin/activate

PyPATH="python-flask-server:python-client"
SERVER=python-flask-server/swagger_server/controllers/pets_controller.py
TESTER=python-flask-server/swagger_server/test/test_pets_controller.py


app:
	(export PYTHONPATH=$(PyPATH); python3 -i python-flask-server/swagger_server/__main__.py)


server:
	(export PYTHONPATH=$(PyPATH); python3 -i $(SERVER))

tester:
	(export PYTHONPATH=$(PyPATH); python3 $(TESTER))


client:
	(export PYTHONPATH=$(PyPATH); python3 python-client/test/test_pets_api.py)

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


runOpenAPIServer:
	(export PYTHONPATH="out/python"; python3 out/python/openapi_server/__main__.py)


pets:
	open http://localhost:8080/v1/pets

