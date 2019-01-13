SHELL := /bin/bash
PACKAGE=yspec
VERSION=$(shell grep Version DESCRIPTION |awk '{print $$2}')
TARBALL=${PACKAGE}_${VERSION}.tar.gz
PKGDIR=.
CHKDIR=.

covr: 
	Rscript inst/script/covr.R

pkgdown:
	Rscript -e 'pkgdown::build_site()'
	cp vignettes/*.pdf docs/articles

readme:
	Rscript -e 'rmarkdown::render("README.Rmd")'

vignette:
	Rscript -e 'rmarkdown::render("inst/doc/vignette.Rmd")'

ec:
	echo ${VERSION}

data:
	Rscript --vanilla inst/test_data/data.R

all:
	make data
	make doc
	make build
	make install

test:
	make install
	Rscript -e 'testthat:::test_dir("tests")'

.PHONY: doc
doc:
	Rscript -e 'library(devtools); document()'

build:
	R CMD build --md5 $(PKGDIR)

install:
	R CMD INSTALL --install-tests ${TARBALL}

install-build:
	R CMD INSTALL --build --install-tests ${TARBALL}

check:
	make data
	make doc
	make build
	R CMD CHECK ${TARBALL} -o ${CHKDIR}

travis:
	make build
	R CMD check --no-manual ${TARBALL} -o ${CHKDIR}

clean: 
	rm *.pdf
	rm *.yml
	rm tests/testthat/*.pdf
