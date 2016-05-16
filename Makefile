
all: check

check: build
	R CMD check *.tar.gz

build: tests clean docs
	R CMD build .

tests:
	R --slave -e "library(devtools); devtools::use_testthat()"

docs:
	R --slave -e "library(devtools); devtools::document()"
	R CMD Rd2pdf . --no-preview -o BayesPref.pdf

clean:
	rm -f *.tar.gz
	rm -f BayesPref.pdf
