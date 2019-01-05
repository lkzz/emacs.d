;; init-yasnippet.el --- Initialize yasnippet configurations.
;;; init-yasnippet.el --- Initialize yasnippet configurations. -*- lexical-binding: t; -*-

;; Copyright (C) 2017-2019  Kevin Leung

;;; This file is NOT part of GNU Emacs

;;; License

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Setup yasnippet
;; Refer: https://github.com/abelardojarab/emacs-config/blob/master/.emacs.d/setup/setup-yasnippet.el

;;; Code:

;; Add the following code to emacs startup file.

;; (require 'init-yasnippet)

(use-package yasnippet
  :defer t
  :ensure t
  :diminish yas-minor-mode
  :hook (after-init . yas-global-mode))

(use-package yasnippet-snippets
  :defer t
  :ensure t
  :after (yasnippet)
  :ensure t)

(provide 'init-yasnippet)
;;; init-yasnippet.el ends here
