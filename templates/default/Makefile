# DESCRIPTION:
# A Makefile for orchestrating `purus-pkg` to:
#
#   - fetch dependencies in the `./purus-modules`
#
#   - copy the current project sources into`./purus-modules`
#
#   - compile all the sources in `./purus-modules`
# USAGE:
# Execute
#
# ```
# make
# ```
#
# and the top-most rule will call all the required rules to do the above tasks.
#

.PHONY: clean

PURUS_FILES=$(shell find src -name "*.purs")
PURUS_OUTPUT_FILES=$(addprefix purus-modules/,$(PURUS_FILES))

# Copy the src files into the purus-modules directory when they are modified
purus-modules/output: purus-modules $(PURUS_OUTPUT_FILES)
	purus-pkg build

# Copy the src files into the purus-modules directory when they are modified
purus-modules/src/%.purs: src/%.purs
	cp $^ purus-modules/

# Rule to install the dependencies whenever the package.json is edited
purus-modules: package.json
	purus-pkg install

# Rule to clean up the dependencies
clean:
	rm -rf purus-modules
