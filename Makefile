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

optimize:
	# 加速gui emacs启动，https://emacs-china.org/t/topic/6453/6
	@defaults write org.gnu.Emacs Emacs.ToolBar -string no
	@defaults write org.gnu.Emacs Emacs.ScrollBar -string no
	@defaults write org.gnu.Emacs Emacs.MenuBar -string no

install_lsp_server:
	@go install golang.org/x/tools/gopls@latest # golang
	@pip install cmake-language-server	# cmake
	@npm install vue-language-server -g # vue
	@npm install -g pyright             # python
	@npm i -g bash-language-server      # bash
	@npm i -g typescript typescript-language-server # Javascript

install_tools:
	# utils
	@brew install coreutils direnv sqlite hub gpg2 coreutils gnu-tar mplayer direnv libtool git-delta
	# lsp-bridge
	@pip install epc
	# search tool
	@brew install ripgrep grep exa zstd fd
	# language
	@brew install golangci-lint shellcheck aspell languagetool clang-format
	@echo "install tools done."

install_go_tools:
	@go install google.golang.org/protobuf/cmd/protoc-gen-go@latest
	@go install github.com/gogo/protobuf/protoc-gen-gogo@latest
	@go install google.golang.org/grpc/cmd/protoc-gen-go-grpc@latest
	@go install github.com/godoctor/godoctor@latest
	@go install github.com/ramya-rao-a/go-outline@latest
	@go install github.com/smartystreets/goconvey@latest
	@go install github.com/golang/mock/mockgen@latest
	@go install rsc.io/2fa@latest
	@go install github.com/cweill/gotests/...@latest
	@go install honnef.co/go/tools/cmd/staticcheck@latest
	@go install github.com/go-delve/delve/cmd/dlv@latest
	@go install github.com/zmb3/gogetdoc@latest
	@go install github.com/josharian/impl@latest
	@go install github.com/fatih/gomodifytags@latest
	@go install github.com/davidrjenni/reftools/cmd/fillstruct@latest
	@go install github.com/google/wire/cmd/wire@latest
	# @go install git.bilibili.co/golang-lint/bilibodyclose@latest
	# @go install git.bilibili.co/golang-lint/bilidurationcheck/cmd/durationcheck@latest
	# @go install git.bilibili.co/golang-lint/bilisqlclosecheck@latest
	# @go install git.bilibili.co/golang-lint/bililoopclosure@latest
	# @go install git.bilibili.co/golang-lint/biliautomaxprocs@latest
	@go install mvdan.cc/gofumpt@latest # A stricter gofmt
	@go install go.uber.org/automaxprocs@latest

.PHONY:clean install install_tools install_lsp_server
