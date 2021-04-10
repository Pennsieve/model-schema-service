.PHONY: run console

run:
	sbt docker
	docker-compose up --remove-orphans

console:
	bin/run_console.sh