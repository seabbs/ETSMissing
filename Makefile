default: all

all: build_data  build_package build_vignettes README.md build_site build_review check_package git_commit

#Update data
.PHONY: build_data
build_data:
		cd data-raw && make


#build update documents and build package
.PHONY: build_package
build_package:
     Rscript -e 'devtools::document()'
		 Rscript -e 'devtools::install()'

#update readme
README.md: README.Rmd
		Rscript -e 'rmarkdown::render("README.Rmd")'
		rm README.html


#Update vignettes
.PHONY: build_vignettes
build_vignettes:
		cd vignettes && make


#Update peer review
.PHONY: build_review
build_review:
		cd peer-review && make

## Check package locally
.PHONY: check_package
check_package:
		Rscript -e "devtools::check()"

#build pkgdown site
build_site:
		Rscript -e 'pkgdown::build_site()'
		mkdir -p docs/articles/results/paper/figs
		cp -r vignettes/results/paper/figs docs/articles/results/paper/


#Commit updates
.PHONY: git_commit
git_commit:
		git add --all
		git commit -m "$(message)"
		git push
