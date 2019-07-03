EMACS_DIR = ~/.emacs.d
ELPA_DIR = $(EMACS_DIR)/elpa
CORE_DIR = $(EMACS_DIR)/core

clean:
	@rm -rf $(ELPA_DIR)
	@rm -rf $(CORE_DIR)/*.elc
	@rm -rf $(EMACS_DIR)/custom.el $(EMACS_DIR)/.emacs.desktop $(EMACS_DIR)/auto-save-list
	@find . -maxdepth 1 -type f -name "*~" | xargs rm
	@find . -maxdepth 1 -type f -name ".?*" | grep -v .DS_Store  | grep -v .gitignore | grep -v .gitmodules | xargs rm
	@rm -rf projectile* places recentf
	@echo "make clean done."

compile:

install:
	@emacs --batch -l $(EMACS_DIR)/init.el
	@echo "make install done."

.PHONY:clean compile install
