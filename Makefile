SHELL := /bin/bash
PACKAGE=yspec
VERSION=$(shell grep Version DESCRIPTION |awk '{print $$2}')
TARBALL=${PACKAGE}_${VERSION}.tar.gz
PKGDIR=.
CHKDIR=.

testn: 
	Rscript inst/validation/latest.R

# development cycle - bumps the version and tags based on version
bump-dev:
	Rscript -e 'usethis::use_version("dev")'

tag-version:
	git tag $(VERSION)
	git push origin $(VERSION)

# also can run make package to build vignettes
all:
	make data
	make doc
	make build
	make install

.PHONY: doc
doc:
	Rscript -e 'library(devtools); document()'

build:
	R CMD build --md5  --no-build-vignettes $(PKGDIR)

build-vignettes:
	R CMD build --md5 $(PKGDIR)

package: 
	make data
	make doc
	make build-vignettes
	make install

install:
	R CMD INSTALL --install-tests ${TARBALL}

check:
	make data
	make build
	R CMD CHECK  --ignore-vignettes ${TARBALL} -o ${CHKDIR}

check-package:
	make data
	make build-vignettes
	R CMD CHECK ${TARBALL} -o ${CHKDIR}

test:
	make install
	Rscript -e 'testthat::test_local()'

spelling:
	make doc
	Rscript -e "spelling::spell_check_package('.')"

covr: 
	Rscript inst/script/covr.R

pkgdown:
	Rscript -e 'options(pkgdown.internet = FALSE); pkgdown::build_site()'

readme:
	Rscript -e 'rmarkdown::render("README.Rmd")'

ec:
	echo ${VERSION}

data:
	Rscript inst/test_data/data.R

install-build:
	R CMD INSTALL --build --install-tests ${TARBALL}

clean: 
	rm *.pdf
	rm *.yml
	rm *.yaml
	rm tests/testthat/*.pdf
