default: all
RDIR = .
VIG_DRAFTS_DIR = $(RDIR)/drafts
RESOURCES = $(RDIR)/resources/*

#update analysis_plan
ANALYSIS_PLAN = $(VIG_DRAFTS_DIR)/analysis_plan/analysis_plan.html
#update paper
PAPER = $(VIG_DRAFTS_DIR)/paper/paper.html

all: $(ANALYSIS_PLAN) $(PAPER)

$(ANALYSIS_PLAN): analysis_plan.Rmd $(RESOURCES)
			R -e 'rmarkdown::render("analysis_plan.Rmd", c("html_document", "word_document"), output_dir = "drafts/analysis_plan", output_options = list())'

$(PAPER): paper.Rmd $(RESOURCES)
			R -e 'rmarkdown::render("paper.Rmd", c("html_document", "word_document"), output_dir = "drafts/paper", output_options = list())'



