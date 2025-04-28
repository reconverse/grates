RDEVEL := Rdevel

.PHONY: doc pkg install check cran test manual revdep site clean readme

doc: pkg/README.md
	PKG_BUILD_EXTRA_FLAGS=false R -s -e "roxygen2::roxygenize('pkg', load_code = roxygen2::load_pkgload)"

pkg: doc
	rm -f *.tar.gz
	R CMD build pkg

install: pkg
	R CMD INSTALL *.tar.gz

check: pkg
	R CMD check *.tar.gz
	TZ=NZ R CMD check *.tar.gz

cran: pkg
	${RDEVEL} CMD check --as-cran *.tar.gz
	TZ=NZ ${RDEVEL} CMD check *.tar.gz

test: doc
	R -s -e "testthat::test_local('pkg')"

manual: doc
	R CMD Rd2pdf --force -o manual.pdf ./pkg

revdep: pkg
	rm -rf revdep
	mkdir revdep
	mv *.tar.gz revdep
	R -s -e "out <- tools::check_packages_in_dir('revdep',reverse=list(which='most')); print(summary(out)); saveRDS(out, file='revdep/output.RDS')"

readme: README.md

pkg/README.md: pkg/README.Rmd
	cd pkg; Rscript -e "litedown::fuse('README.Rmd', '.md')"

README.md: pkg/README.md
	cp pkg/README.md README.md


site: install
	mkdir -p sitebuild
	rm -rf sitebuild/* docs
	cp -r site/* sitebuild/
	cd sitebuild; Rscript -e "litedown::fuse('_footer.Rmd', '.md')"
	cd sitebuild; Rscript -e "litedown::fuse_site()"
	cd sitebuild; rm -f *.Rmd *.yml _*
	cp -r sitebuild docs
	rm -rf sitebuild
	xdg-open docs/index.html

clean:
	rm -rf *.Rcheck
	rm -rf revdep
	rm -f *.tar.gz
	rm -f manual.pdf
	rm -f pkg/src/*.o
	rm -f pkg/src/*.so

