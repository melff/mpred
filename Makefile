SRCDIR := pkg

PACKAGE := $(shell Rscript -e 'cat(read.dcf(file="${SRCDIR}/DESCRIPTION",fields="Package"))')
VERSION := $(shell Rscript -e 'cat(read.dcf(file="${SRCDIR}/DESCRIPTION",fields="Version"))')

ARCHIVE := $(PACKAGE)_$(VERSION).tar.gz

.PHONY: describe
describe:
	cat ${SRCDIR}/DESCRIPTION

.PHONY: show
show:
	echo "Package ${PACKAGE} version ${VERSION}"

.PHONY: roxygenize
roxygenize:
	Rscript -e 'roxygen2::roxygenize("${SRCDIR}")'

.PHONY: build
build:
	echo "Building ${ARCHIVE}"
	R CMD build $(SRCDIR)

.PHONY: check-dir
check-dir: 
	echo "Checking package ${PACKAGE} version ${VERSION} in ${SRCDIR}"
	R CMD check $(SRCDIR)

.PHONY: check
check: 
	R CMD check $(ARCHIVE)

.PHONY: install-dir
install-dir:
	echo "Installing package ${PACKAGE} version ${VERSION} from ${SRCDIR}"
	R CMD INSTALL $(SRCDIR)

.PHONY: install
install:
	R CMD INSTALL $(ARCHIVE)

.PHONY: check-cran
check-cran:
	R CMD check --as-cran $(ARCHIVE)

.PHONY: rhub-valgrind
rhub-valgrind:
	Rscript -e 'rhub::check_with_valgrind("${ARCHIVE}",show_status = FALSE)'

.PHONY: rhub-sanitizers
rhub-sanitizers:
	Rscript -e 'rhub::check_with_sanitizers("${ARCHIVE}",show_status = FALSE)'

.PHONY: rhub-macos
rhub-macos:
	Rscript -e 'rhub::check_on_macos("${ARCHIVE}",show_status = FALSE)'

.PHONY: rhub-windows
rhub-windows:
	Rscript -e 'rhub::check_on_windows("${ARCHIVE}",show_status = FALSE)'

.PHONY: rhub-for-cran
rhub-for-cran:
	Rscript -e 'rhub::check_for_cran("${ARCHIVE}",show_status = FALSE)'
