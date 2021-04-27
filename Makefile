EMACS_DIR = ${CURDIR}
CORE_DIR = $(EMACS_DIR)/core
CACHE_DIR = $(EMACS_DIR)/cache
STRAIGHT_DIR =$(CACHE_DIR)/straight
ELN_CACHE_DIR=$(CACHE_DIR)/eln
QUELPA_DIR=$(CACHE_DIR)/quelpa
ELPA_DIR = $(CACHE_DIR)/elpa

default: install

clean:
	@rm -rf $(ELPA_DIR)
	@rm -rf $(STRAIGHT_DIR)
	@rm -rf $(ELN_CACHE_DIR)
	@rm -rf $(QUELPA_DIR)
	@rm -rf $(CORE_DIR)/*.elc
	@rm -rf $(CACHE_DIR)/*.el*
	@rm -rf $(EMACS_DIR)/custom.el $(EMACS_DIR)/.emacs.desktop $(EMACS_DIR)/auto-save-list
	@find . -maxdepth 1 -type f -name "*~" | xargs rm -rf
	@find . -maxdepth 1 -type f -name ".?*" | grep -v .DS_Store  | grep -v .gitignore | grep -v .gitmodules | xargs rm -rf
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
	@go get golang.org/x/tools/...
	@go get golang.org/x/tools/gopls@latest
	@go get github.com/gogo/protobuf/gogoproto
	@go get github.com/golang/protobuf/proto
	@go get github.com/gogo/protobuf/protoc-gen-gofast
	@go get github.com/gogo/protobuf/protoc-gen-gogo
	@go get github.com/golang/protobuf/protoc-gen-go
	@go get github.com/godoctor/godoctor
	@go get github.com/ramya-rao-a/go-outline
	@go get github.com/cweill/gotests/...
	@go get github.com/smartystreets/goconvey
	@go get github.com/golang/mock/mockgen@v1.5.0
	@go get rsc.io/2fa

.PHONY:clean install install_tools install_lsp_server
