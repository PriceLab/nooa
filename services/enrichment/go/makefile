default:
	@echo targets: run ps kill test


run:
	R -f runService.R &


ps:
	- ps -ef | grep runService.R | egrep -v grep

kill:
	- kill `ps -ef | grep runService.R | egrep -v grep| awk '{print $$2}'`

test:
	R -f test_pathwayEnrichmentService.R




