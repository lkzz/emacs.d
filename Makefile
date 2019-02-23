EMACS_DIR = ~/.emacs.d
ELPA_DIR = $(EMACS_DIR)/elpa
CORE_DIR = $(EMACS_DIR)/core

clean:
	@rm -rf $(ELPA_DIR)
	@rm -rf $(CORE_DIR)/*.elc
	@rm -rf $(EMACS_DIR)/custom.el $(EMACS_DIR)/.emacs.desktop $(EMACS_DIR)/auto-save-list
	@find . -type f -maxdepth 1 -name "*~" | xargs rm
	@find . -type f -maxdepth 1 -name ".?*" | grep -v .DS_Store  | grep -v .gitignore | grep -v .gitmodules | xargs rm
	@echo "make clean done."

compile:

install:
	@emacs --batch -l $(EMACS_DIR)/init.el
	@echo "make install done."

.PHONY:clean compile install
