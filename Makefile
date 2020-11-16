EMACS_DIR = ${CURDIR}
CORE_DIR = $(EMACS_DIR)/core
CACHE_DIR = $(EMACS_DIR)/cache
STRAIGHT_DIR =$(EMACS_DIR)/straight
QUELPA_DIR=$(CACHE_DIR)/quelpa
ELPA_DIR = $(CACHE_DIR)/elpa

default: install

clean:
	@rm -rf $(ELPA_DIR)
	@rm -rf $(STRAIGHT_DIR)
	@rm -rf $(QUELPA_DIR)
	@rm -rf $(CORE_DIR)/*.elc
	@rm -rf $(CACHE_DIR)/*.el*
	@rm -rf $(EMACS_DIR)/custom.el $(EMACS_DIR)/.emacs.desktop $(EMACS_DIR)/auto-save-list
	@find . -maxdepth 1 -type f -name "*~" | xargs rm
	@find . -maxdepth 1 -type f -name ".?*" | grep -v .DS_Store  | grep -v .gitignore | grep -v .gitmodules | xargs rm
	@rm -rf projectile* places recentf transient
	@echo "make clean done."

install:
	@emacs --batch -l $(EMACS_DIR)/init.el
	@echo "make install done."

install_lsp_server:
	@go get -u golang.org/x/tools/gopls # golang
	@pip install cmake-language-server	# cmake
	@npm install vue-language-server -g # vue
	@npm install -g pyright             # python

install_tools:
	@brew install coreutils direnv ripgrep
	@brew install prettier # formatter
	@brew install golangci/tap/golangci-lint
	@echo "install tools done."

install_go_tools:
	@go get -u golang.org/x/tools/...
	@go get -u github.com/gogo/protobuf/gogoproto
	@go get -u github.com/golang/protobuf/proto
	@go get -u github.com/gogo/protobuf/protoc-gen-gofast
	@go get -u github.com/gogo/protobuf/protoc-gen-gogo
	@go get -u github.com/golang/protobuf/protoc-gen-go
	@go get -u github.com/godoctor/godoctor
	@go get -u github.com/ramya-rao-a/go-outline
	@go get -u github.com/cweill/gotests/...
	@go get -u github.com/smartystreets/goconvey
	@go get -u github.com/golang/mock/mockgen@v1.4.4

.PHONY:clean install install_tools install_lsp_server
