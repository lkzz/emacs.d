;;; pyim-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "pyim" "pyim.el" (23168 11294 974944 905000))
;;; Generated autoloads from pyim.el

(defvar pyim-isearch-mode nil "\
Non-nil if Pyim-Isearch mode is enabled.
See the `pyim-isearch-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `pyim-isearch-mode'.")

(custom-autoload 'pyim-isearch-mode "pyim" nil)

(autoload 'pyim-isearch-mode "pyim" "\
pyim isearch mode.

\(fn &optional ARG)" t nil)

(autoload 'pyim-hanzi2pinyin "pyim" "\
将汉字字符串转换为对应的拼音字符串, 如果 `shou-zi-mu' 设置为t,转换仅得到拼音
首字母字符串。当 `return-list' 设置为 t 时，返回一个拼音列表，这个列表包含词条的一个
或者多个拼音（词条包含多音字时）；如果 `ignore-duo-yin-zi' 设置为t, 遇到多音字时，
只使用第一个拼音，其它拼音忽略；当 `adjust-duo-yin-zi' 设置为t时，pyim-hanzi2pinyin
会使用 pyim 已安装的词库来校正多音字，但这个功能有一定的限制:

1. pyim 普通词库中不存在的词条不能较正
2. 多音字校正速度比较慢，实时转换会产生卡顿。

BUG: 当 `string' 中包含其它标点符号，并且设置 `separator' 时，结果会包含多余的连接符：
比如： '你=好' --> 'ni-=-hao'

\(fn STRING &optional SHOU-ZI-MU SEPARATOR RETURN-LIST IGNORE-DUO-YIN-ZI ADJUST-DUO-YIN-ZI)" nil nil)

(autoload 'pyim-hanzi2pinyin-simple "pyim" "\
简化版的 `pyim-hanzi2pinyin', 不处理多音字。

\(fn STRING &optional SHOU-ZI-MU SEPARATOR RETURN-LIST)" nil nil)

(autoload 'pyim-dicts-manager "pyim" "\
pyim 词库管理器。

\(fn)" t nil)

;;;***

;;;### (autoloads nil nil ("pyim-devtools.el" "pyim-pkg.el" "pyim-probe.el"
;;;;;;  "pyim-pymap.el") (23168 11294 978639 104000))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; pyim-autoloads.el ends here
