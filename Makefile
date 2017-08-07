build:
	npm run-script build
	rsync -vax --delete build/ /services/web/localhost/vis

.PHONY: build
