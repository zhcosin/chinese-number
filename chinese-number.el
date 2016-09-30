;;; chinese-number.el --- Convert numbers between Arabic and Chinese formats

;; * Header
;; Copyright (c) 2015, zhcosin

;; Author: zhcosin<zhcosin@163.com>
;; URL: https://github.com/zhcosin/chinese-number
;; Created: 2016-09-21

;;; License:

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Chinese-number is a package for converting number format between
;; Arabic and Chinese.
;;
;; If you want to convert a Arabic number to chinese, you can:
;;
;;     M-x cnum-convert-arabic-number-to-chinese
;;
;; and input the Arabic number, or you may want to convert a number
;; from Chinese to Arabic, you can:
;;
;;     M-x cnum-convert-chinese-number-to-arabic
;;
;; If you want use this converting in your elisp code, then you can
;; call the following two function:
;;
;;     cnum--convert-arabic-number-to-chinese
;;
;; and
;;
;;     cnum--convert-chinese-number-to arabic
;;
;;; Installation:
;;
;; Chinese-number lives in a Git repository. To obtain it, do
;;
;;     git clone https://github.com/zhcosin/chinese-number.git
;;
;; Move chinese-number to ~/.emacs.d/chinese-number (or somewhere
;; else in the `load-path'). Then add the following lines to ~/.emacs:
;;
;;     (add-to-list 'load-path "~/.emacs.d/chinese-number")
;;     (require 'chinese-number)
;;
;;.
;;; 算法介绍
;;
;;  阿拉伯数字转换为中文
;;     最基本也是最核心的概念就是数字(digit)与权(weight)，分别将数字与权转换为对应的中文，再作字符串
;; 连接即可(这是最基本的转换操作)，但是零这个特殊的数字是需要特殊处理的，这个后面再描述。
;;     但这只适合10000以下的数，在中文中，数字还有更高级的权，上了10000就是1万，10000个1万就是1亿，
;; （在英文中这个更高级的权是千)，所以这个转换是有两个层次的，低层次的转换以10为基数，高层次的转换以
;; 万为基数，但这两个层次的转换所用的算法是一致的，都是反复将阿拉伯数字除以基数，得到商和余数(称为切
;; 片，slice)，然后对余数利用基本的转换操作进行转换，将其结果与对商的递归转换所得结果进行连接即可。
;;     零的规则如下:
;;     1. 如果某位数字为零，则不带权。
;;     1. 从个位数开始的连续任意个数的零，忽略之。
;;     1. 数字中间的连续任意个数的零，只转换为一个零，并且不带权.
;;     1. 对于一个切片，如果其数字小于基的十分之一，需要在前端补零(数字本来就是零除外)。

;;; Code:

(defvar cnum-chinese-use-lowercase t)
(defvar cnum--lowercase-chinese-number-list (list "零" "一" "二" "三" "四" "五" "六" "七" "八" "九"))
(defvar cnum--uppercase-chinese-number-list (list "零" "壹" "贰" "叁" "肆" "伍" "陆" "柒" "捌" "玖"))
(defvar cnum--lowercase-chinese-weight-list (list "" "十" "百" "千"))
(defvar cnum--uppercase-chinese-weight-list (list "" "拾" "百" "仟"))
(defvar cnum--high-level-weight-list (list "" "万" "亿"))

(defun cnum--get-chinese-number-by-index (index)
  ;; 从中文数表中根据索引找出相应的中文数字
  (nth index (if cnum-chinese-use-lowercase cnum--lowercase-chinese-number-list cnum--uppercase-chinese-number-list)))

(defun cnum--get-chinese-weight (index)
  ;; 从中文权表中根据索引找出相应的中文权
  (nth index (if cnum-chinese-use-lowercase cnum--lowercase-chinese-weight-list cnum--uppercase-chinese-weight-list)))

(defun cnum--get-chinese-high-level-weight (index)
  ;; 从中文高阶权表根据索引找出相应的中文权
  (nth index cnum--high-level-weight-list))

(defun cnum--get-chinese-number-and-weight-general (number-index weight-index fun-to-convert-number fun-to-get-weight)
  ;; 转换切片的一般过程，根据传入的转换函数分别转换数字和权，并将结果作字符串连接，对数字为零的，忽略权
  (if (= number-index 0)
      (funcall fun-to-convert-number 0)
    (concat (funcall fun-to-convert-number number-index) (funcall fun-to-get-weight weight-index))))

(defun cnum--get-chinese-number-and-weight (number-index weight-index)
  ;; 调用转换切片的一般过程，指定了单一数字转换函数和低阶权转换函数，以实现转换10000以下的数
  (cnum--get-chinese-number-and-weight-general number-index weight-index 'cnum--get-chinese-number-by-index 'cnum--get-chinese-weight))

(defun cnum--get-chinese-number-and-weight-high (number weight-index base disable-zero is-last-slice)
  ;; 调用转换切片的一般过程，指定了转换10000以下的数字的过程为数位转换过程，同时指定高阶权转换函数为转换权的函数，以实现10000以上的高阶转换，包含对零的特殊处理
  (let ((full-result
	 (cnum--get-chinese-number-and-weight-general number weight-index 'cnum--convert-arabic-number-less-than-10000-to-chinese 'cnum--get-chinese-high-level-weight)))
    (cond ((= number 0) (if disable-zero "" full-result))
	  ((< number (/ base 10)) (if is-last-slice full-result (concat (cnum--get-chinese-number-by-index 0) full-result)))
	  (t full-result))))

(defun cnum--get-chinese-number-weight (number weight-index base disable-zero is-last-slice)
  ;; 在 cnum--get-get-chinese-number-and-weight 的基础上实现对零的特殊处理
  (if (= number 0)
      (if disable-zero "" (cnum--get-chinese-number-by-index 0))
    (cnum--get-chinese-number-and-weight number weight-index)))

(defun cnum--convert-to-chinese-general-iter (number index slice-index next-is-zero base fun-to-convert-small-number)
  ;; 递归转换，将原数除以基数(10或10000)的余数作基本转换，将其与对商的递归转换结果作字符连接，是最核心的转换过程
  (let ((this-slice (% number base))
	(next-slice (/ number base)))
  (if (= next-slice 0)
      (funcall fun-to-convert-small-number number index base (if (= slice-index 0) nil next-is-zero) t)
    (concat (cnum--convert-to-chinese-general-iter next-slice (+ index 1) (+ slice-index 1) (= this-slice 0) base fun-to-convert-small-number)
	    (funcall fun-to-convert-small-number this-slice index base next-is-zero nil)))))

(defun cnum--convert-arabic-number-less-than-10000-to-chinese-iter (number index next-is-zero)
  ;; 指定基数为10，实现1000以内的数的迭代转换
  (cnum--convert-to-chinese-general-iter number index 0 next-is-zero 10 'cnum--get-chinese-number-weight))

(defun cnum--convert-arabic-number-to-chinese-iter (number index next-is-zero)
  ;; 指定基数为10000，实现任何正整数的迭代转换
  (cnum--convert-to-chinese-general-iter number index 0 next-is-zero 10000 'cnum--get-chinese-number-and-weight-high))

(defun cnum--convert-arabic-number-less-than-10000-to-chinese (number)
  ;; 转换10000以下的数的转换
  (cnum--convert-arabic-number-less-than-10000-to-chinese-iter number 0 t))

(defun cnum--convert-arabic-number-to-chinese (number)
  ;; 将阿拉伯数字转换为中文的API
  (cnum--convert-arabic-number-to-chinese-iter number 0 t))

(defun cnum--convert-chinese-number-to-arbic (number)
  ;; 将中文数字转换为阿拉伯数字的API
  ;; TODO: implement.
  0)

;;;###autoload
(defun cnum-convert-arabic-number-to-chinese (number)
  ;; 供 emacs 调用的命令，将阿拉伯数字转换为中文
  "convert a number in Arabic format to Chinese."
  (interactive "nInput the Arabic number: ")
  (message "%d = %s" number (cnum--convert-arabic-number-to-chinese number)))

;;;###autoload
(defun cnum-convert-chinese-number-to-arabic (number)
  ;; 供 emacs 调用的命令，将中文数字转换为阿拉伯数字
  "convert a number in Chinese format to Arabic."
  (interactive "sInput the Chinese number: ")
  (message "NO SUPPORT: The converting from chinese to arabic is not support now!"))

;; for test
;;(cnum-convert-arabic-number-to-chinese 0)
;;(cnum-convert-arabic-number-to-chinese 1230)
;;(cnum-convert-arabic-number-to-chinese 380070500)

(provide 'chinese-number)
;;; chinese-number.el ends here
