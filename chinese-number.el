;;; chinese-number.el --- A package for converting number format between Arabic and Chinese.

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
;; If you want to convert a Arabic number to chinese, you can:
;;
;;     M-x cnum--convert-arabic-number-to-chinese
;;
;; and input the Arbic number, or you may want to convert a number
;; from Chinese to Arabic, you can:
;;
;;     M-x cnum--convert-chinese-number-to-arabic
;;
;; If you want use this converting in your elisp code, then you can
;; call the following two function:
;;
;;     cnum-convert-arabic-number-to-chinese
;;
;; and
;;
;;     cnum-convert-chinese-number-to arabic
;;.

(defvar cnum-chinese-use-lowercase t)
(defvar cnum-lowercase-chinese-number-list (list "零" "一" "二" "三" "四" "五" "六" "七" "八" "九"))
(defvar cnum-uppercase-chinese-number-list (list "零" "壹" "贰" "叁" "肆" "伍" "陆" "柒" "捌" "玖"))
(defvar cnum-lowercase-chinese-weight-list (list "" "十" "百" "千"))
(defvar cnum-uppercase-chinese-weight-list (list "" "拾" "百" "仟"))
(defvar cnum-high-level-weight-list (list "" "万" "亿"))


(defun cnum-get-chinese-number-by-index (index)
  (nth index (if cnum-chinese-use-lowercase cnum-lowercase-chinese-number-list cnum-uppercase-chinese-number-list)))

(defun cnum-get-chinese-weight (index)
  (nth index (if cnum-chinese-use-lowercase cnum-lowercase-chinese-weight-list cnum-uppercase-chinese-weight-list)))

(defun cnum-get-chinese-high-level-weight (index)
  (nth index cnum-high-level-weight-list))

(defun cnum-get-chinese-number-and-weight-general (number-index weight-index fun-to-convert-number fun-to-get-weight)
  (if (= number-index 0)
      (funcall fun-to-convert-number 0)
    (concat (funcall fun-to-convert-number number-index) (funcall fun-to-get-weight weight-index))))

(defun cnum-get-chinese-number-and-weight (number-index weight-index)
  (cnum-get-chinese-number-and-weight-general number-index weight-index 'cnum-get-chinese-number-by-index 'cnum-get-chinese-weight))

(defun cnum-get-chinese-number-and-weight-high (number weight-index base disable-zero is-last-slice)
  (let ((full-result
	 (cnum-get-chinese-number-and-weight-general number weight-index 'cnum-convert-arabic-number-less-than-10000-to-chinese 'cnum-get-chinese-high-level-weight)))
    (cond ((= number 0) (if disable-zero "" full-result))
	  ((< number (/ base 10)) (if is-last-slice full-result (concat (cnum-get-chinese-number-by-index 0) full-result)))
	  (t full-result))))

(defun cnum-get-chinese-number-weight (number weight-index base disable-zero is-last-slice)
  (if (= number 0)
      (if disable-zero "" (cnum-get-chinese-number-by-index 0))
    (cnum-get-chinese-number-and-weight number weight-index)))

(defun cnum-convert-to-chinese-general-iter (number index slice-index next-is-zero base fun-to-convert-small-number)
  (let ((this-slice (% number base))
	(next-slice (/ number base)))
  (if (= next-slice 0)
      (funcall fun-to-convert-small-number number index base (if (= slice-index 0) nil next-is-zero) t)
    (concat (cnum-convert-to-chinese-general-iter next-slice (+ index 1) (+ slice-index 1) (= this-slice 0) base fun-to-convert-small-number)
	    (funcall fun-to-convert-small-number this-slice index base next-is-zero nil)))))

(defun cnum-convert-arabic-number-less-than-10000-to-chinese-iter (number index next-is-zero)
  (cnum-convert-to-chinese-general-iter number index 0 next-is-zero 10 'cnum-get-chinese-number-weight))

(defun cnum-convert-arabic-number-to-chinese-iter (number index next-is-zero)
  (cnum-convert-to-chinese-general-iter number index 0 next-is-zero 10000 'cnum-get-chinese-number-and-weight-high)
  )

(defun cnum-convert-arabic-number-less-than-10000-to-chinese (number)
  (cnum-convert-arabic-number-less-than-10000-to-chinese-iter number 0 t))

(defun cnum-convert-arabic-number-to-chinese (number)
  (cnum-convert-arabic-number-to-chinese-iter number 0 t))

(defun cnum-convert-chinese-number-to-arbic (number)
  ;; TODO: implement.
  0)

(defun cnum--convert-arabic-number-to-chinese (number)
  "convert a number in Arabic format to Chinese."
  (interactive "nInput the Arabic number: ")
  (message "The chinese format for number %d is %s" number (cnum-convert-arabic-number-to-chinese number)))

(defun cnum--convert-chinese-number-to-arabic (number)
  "convert a number in Chinese format to Arabic."
  (interactive "sInput the Chinese number: ")
  (message "NO SUPPORT: The converting from chinese to arabic is not support now!"))

;; for test
;;(cnum--convert-arabic-number-to-chinese 0)
;;(cnum--convert-arabic-number-to-chinese 1230)
;;(cnum--convert-arabic-number-to-chinese 380070500)

(provide 'chinese-number)
