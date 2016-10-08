.PONY: all build deps image test

help: ## Show this help.
	@awk 'BEGIN {FS = ":.*?## "} /^[a-zA-Z_-]+:.*?## / {sub("\\\\n",sprintf("\n%22c"," "), $$2);printf "\033[36m%-20s\033[0m %s\n", $$1, $$2}' $(MAKEFILE_LIST)

all: test build ## Run the tests and build binaries.

build: ## Build binaries.
	elm-make src/**/*.elm

deps: ## Install dependencies.
	elm-package install --yes && cd tests && elm-package install --yes

test: ## Run tests.
	elm-test tests/Main.elm
