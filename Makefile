SHELL := /bin/bash
PACKAGE=yspec
VERSION=$(shell grep Version DESCRIPTION |awk '{print $$2}')
TARBALL=${PACKAGE}_${VERSION}.tar.gz
PKGDIR=.
CHKDIR=.

.PHONY: vignettes
vignettes:
	Rscript --vanilla inst/script/vignettes.R

covr: 
	Rscript inst/script/covr.R

pkgdown:
	Rscript -e 'pkgdown::build_site()'
	cp vignettes/*.pdf docs/articles

readme:
	Rscript -e 'rmarkdown::render("README.Rmd")'

ec:
	echo ${VERSION}

data:
	Rscript --vanilla inst/test_data/data.R

all:
	make data
	make doc
	make build
	make install

package: 
	make data
	make doc
	make vignettes
	make build
	make install
	
test:
	make install
	Rscript -e 'testthat:::test_dir("tests")'

.PHONY: doc
doc:
	Rscript -e 'library(devtools); document()'

build:
	R CMD build --md5  --no-build-vignettes $(PKGDIR)

install:
	R CMD INSTALL --install-tests ${TARBALL}

install-build:
	R CMD INSTALL --build --install-tests ${TARBALL}

check:
	make data
	make build
	R CMD CHECK ${TARBALL} -o ${CHKDIR}

travis:
	make build
	R CMD check --no-manual ${TARBALL} -o ${CHKDIR}

clean: 
	rm *.pdf
	rm *.yml
	rm tests/testthat/*.pdf
