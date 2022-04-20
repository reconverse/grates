doc:
	R -s -e "roxygen2::roxygenize('.', load_code = roxygen2::load_pkgload)"

pkg: doc
	R CMD build .

install: pkg
	R CMD INSTALL *.tar.gz

check: pkg
	R CMD check *.tar.gz

cran: pkg
	R CMD check --as-cran *.tar.gz

test: doc
	R -s -e "tinytest::build_install_test('.')"

manual: doc
	R CMD Rd2pdf --force -o manual.pdf .

revdep: pkg
	rm -rf revdep
	mkdir revdep
	mv *.tar.gz revdep
	R -s -e "out <- tools::check_packages_in_dir('revdep',reverse=list(which='most'),Ncpus=3); print(summary(out)); saveRDS(out, file='revdep/output.RDS')"

clean:
	rm -rf *.Rcheck
	rm -rf revdep
	rm -f *.tar.gz
	rm -f manual.pdf
