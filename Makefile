SRCDIR := pkg

PACKAGE := $(shell Rscript --vanilla -e 'cat(read.dcf(file="${SRCDIR}/DESCRIPTION",fields="Package"))')
VERSION := $(shell Rscript --vanilla -e 'cat(read.dcf(file="${SRCDIR}/DESCRIPTION",fields="Version"))')

ARCHIVE := $(PACKAGE)_$(VERSION).tar.gz

RCHKIMG := kalibera-rchk-master-def.simg

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

.PHONY: rchk
rchk:
	cp $(ARCHIVE) /tmp
	cd rchk; \
	singularity run $(RCHKIMG) $(PACKAGE) /tmp/$(ARCHIVE)
	echo "========================================================" > rchk.out
	date >> rchk.out
	cat rchk/lib/$(PACKAGE)/libs/$(PACKAGE).so.*check >> rchk.out
	echo "========================================================" >> rchk.out
	cat rchk.out

roxygenize:
	Rscript --vanilla -e 'roxygen2::roxygenize(package.dir="${SRCDIR}")'

win-builder-devel:
	curl -T $(ARCHIVE) ftp://win-builder.r-project.org/R-devel/

win-builder-release:
	curl -T $(ARCHIVE) ftp://win-builder.r-project.org/R-release/

win-builder-oldrelease:
	curl -T $(ARCHIVE) ftp://win-builder.r-project.org/R-oldrelease/

rhub-valgrind:
	Rscript --vanilla -e 'rhub::check_with_valgrind("${ARCHIVE}",show_status = FALSE)'

rhub-sanitizers:
	Rscript --vanilla -e 'rhub::check_with_sanitizers("${ARCHIVE}",show_status = FALSE)'

rhub-macos:
	Rscript --vanilla -e 'rhub::check_on_macos("${ARCHIVE}",show_status = FALSE)'

rhub-windows:
	Rscript --vanilla -e 'rhub::check_on_windows("${ARCHIVE}",show_status = FALSE)'

rhub-for-cran:
	Rscript --vanilla -e 'rhub::check_for_cran("${ARCHIVE}",show_status = FALSE)'

rhub-rchk:
	Rscript --vanilla -e 'rhub::check("${ARCHIVE}",platform="ubuntu-rchk",show_status = FALSE)'

rhub-windows-devel:
	Rscript --vanilla -e 'rhub::check("${ARCHIVE}",platform="windows-x86_64-devel",show_status = FALSE)'

rhub-windows-oldrel:
	Rscript --vanilla -e 'rhub::check("${ARCHIVE}",platform="windows-x86_64-oldrel",show_status = FALSE)'

rhub-debian-clang-devel:
	Rscript --vanilla -e 'rhub::check("${ARCHIVE}",platform="debian-clang-devel",show_status = FALSE)'

rhub-fedora-clang-devel:
	Rscript --vanilla -e 'rhub::check("${ARCHIVE}",platform="fedora-clang-devel",show_status = FALSE)'

rhub-list-checks:
  Rscript -e 'rhub::list_my_checks("memisc@elff.eu")'

check-reverse:
	rm -rfv depends/$(PACKAGE)_*
	cp -v $(ARCHIVE) depends/ 
	cp -rfv $(ARCHIVE) depends/*.Rcheck 
	Rscript --vanilla -e 'options(repos="https://ftp.gwdg.de/pub/misc/cran/"); tools::check_packages_in_dir("depends",reverse="all",Ncpus=8); tools::check_packages_in_dir_details("depends")'

clean-reverse:
	rm -rfv depends/*
