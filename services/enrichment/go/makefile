default:
	@echo targets: runService ps test


runService:
	R -f runService.R &


ps:
	- ps -ef | grep runService.R | egrep -v grep

kill:
	- kill `ps -ef | grep runService.R | egrep -v grep| awk '{print $$2}'`

test:
	R -f test_goService.R




