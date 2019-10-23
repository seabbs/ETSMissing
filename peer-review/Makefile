default: all

all: $(COVER) $(REVIEW)

COVER=cover_letter/cover_letter.html

REVIEW=review_response/review_response.html

$(COVER): cover_letter.Rmd
	R -e 'rmarkdown::render("cover_letter.Rmd", c("html_document", "word_document"), output_dir = "cover_letter", output_options = list())'

$(REVIEW): review_response.Rmd
	R -e 'rmarkdown::render("review_response.Rmd", c("html_document", "word_document"), output_dir = "review_response", output_options = list())'


