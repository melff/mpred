SRCDIR := pkg

PACKAGE := $(shell Rscript --vanilla -e 'cat(read.dcf(file="${SRCDIR}/DESCRIPTION",fields="Package"))')
VERSION := $(shell Rscript --vanilla -e 'cat(read.dcf(file="${SRCDIR}/DESCRIPTION",fields="Version"))')

ARCHIVE := $(PACKAGE)_$(VERSION).tar.gz

.PHONY: all
all: build install

.PHONY: describe
describe:
	cat ${SRCDIR}/DESCRIPTION

.PHONY: show
show:
	echo "Package ${PACKAGE} version ${VERSION}"

.PHONY: build
build-wo-vignettes:
	echo "Building ${ARCHIVE}"
	R CMD build --no-build-vignettes $(SRCDIR)

build: roxygenize
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

.PHONY: install-dir-clean
install-dir-clean:
	echo "Installing package ${PACKAGE} version ${VERSION} from ${SRCDIR}"
	R CMD INSTALL --pre-clean $(SRCDIR)

.PHONY: install	
install:
	R CMD INSTALL $(ARCHIVE)

.PHONY: check-cran
check-cran:
	R CMD check --as-cran --run-donttest $(ARCHIVE)

roxygenize:
	Rscript --vanilla -e 'roxygen2::roxygenize(package.dir="${SRCDIR}")'

check-reverse:
	rm -rfv depends/$(PACKAGE)_*
	cp -v $(ARCHIVE) depends/ 
	cp -rfv $(ARCHIVE) depends/*.Rcheck 
	Rscript --vanilla -e 'options(repos="https://ftp.gwdg.de/pub/misc/cran/"); tools::check_packages_in_dir("depends",reverse="all",Ncpus=8); tools::check_packages_in_dir_details("depends")'

clean-reverse:
	rm -rfv depends/*
