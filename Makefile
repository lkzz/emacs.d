EMACS_DIR = ${CURDIR}
ELPA_DIR = $(EMACS_DIR)/elpa
CORE_DIR = $(EMACS_DIR)/core

default: install

clean:
	@rm -rf $(ELPA_DIR)
	@rm -rf $(CORE_DIR)/*.elc
	@rm -rf $(EMACS_DIR)/custom.el $(EMACS_DIR)/.emacs.desktop $(EMACS_DIR)/auto-save-list
	@find . -maxdepth 1 -type f -name "*~" | xargs rm
	@find . -maxdepth 1 -type f -name ".?*" | grep -v .DS_Store  | grep -v .gitignore | grep -v .gitmodules | xargs rm
	@rm -rf projectile* places recentf transient
	@echo "make clean done."

install_rime:
	@wget https://github.com/rime/librime/releases/download/1.5.3/rime-1.5.3-osx.zip
	@unzip rime-1.5.3-osx.zip -d ~/.emacs.d/librime
	@rm -rf rime-1.5.3-osx.zip

install: install_rime
	@emacs --batch -l $(EMACS_DIR)/init.el
	@echo "make install done."

profile:
	emacs -Q -l site-lisp/profile-dotemacs.el --eval "(setq profile-dotemacs-file \
        (setq load-file-name \"$(abspath init.el)\"))" -f profile-dotemacs

.PHONY:clean profile install
