all: build

setup:
	npm install
	psc-package build

build:
	npm run sass
	npm run fonts
	npm run build
	npm run prepareSW

test: #setup
	npm run test

serve: build
	npm run serve

# since `build` and `test` are also a directory, it will not be built without this
# (or `make -B test`):
.PHONY: build test

clean:
	npm run clean

run:
	firefox public/index.html
