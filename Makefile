EMACS_DIR = ${CURDIR}
ELPA_DIR = $(EMACS_DIR)/elpa
CORE_DIR = $(EMACS_DIR)/core
CACHE_DIR = $(EMACS_DIR)/cache

default: install

clean:
	@rm -rf $(ELPA_DIR)
	@rm -rf $(CORE_DIR)/*.elc
	@rm -rf $(CACHE_DIR)/*.el*
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

install_go_tools:
	@go get -u golang.org/x/tools/...
	@go get -u golang.org/x/tools/gopls
	@go get -u github.com/gogo/protobuf/gogoproto
	@go get -u github.com/golang/protobuf/proto
	@go get -u github.com/gogo/protobuf/protoc-gen-gofast
	@go get -u github.com/gogo/protobuf/protoc-gen-gogo
	@go get -u github.com/golang/protobuf/protoc-gen-go
	@go get -u github.com/godoctor/godoctor
	@go get -u github.com/ramya-rao-a/go-outline
	@go get -u github.com/cweill/gotests/...
	@go get -u github.com/smartystreets/goconvey

.PHONY:clean install
