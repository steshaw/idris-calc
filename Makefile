PROG := calc
PKG := calc.ipkg

.PHONY: all
all: build

.PHONY: build
build:
	idris --build $(PKG)

.PHONY: clean
clean:
	idris --clean $(PKG)

.PHONY: check
check:
	idris --checkpkg $(PKG)

.PHONY: doc
doc:
	idris --mkdoc $(PKG)
