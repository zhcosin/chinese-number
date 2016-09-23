(defvar cnum-chinese-number-list (list "零" "一" "二" "三" "四" "五" "六" "七" "八" "九"))
(defvar cnum-traditinal-chinese-number-list (list "零" "壹" "贰" "叁" "肆" "伍" "陆" "柒" "捌" "玖"))
(defvar cnum-weight-list (list "" "十" "百" "千"))
(defvar cnum-high-level-weight-list (list "" "万" "亿"))


(defun cnum-get-chinese-number-by-index (index)
  (nth index cnum-chinese-number-list))

(defun cnum-get-chinese-weight (index)
  (nth index cnum-weight-list))

(defun cnum-get-chinese-high-level-weight (index)
  (nth index cnum-high-level-weight-list))

(defun cnum-get-chinese-number-and-weight-general (number-index weight-index fun-to-convert-number fun-to-get-weight)
  (if (= number-index 0)
      (funcall fun-to-convert-number 0)
    (concat (funcall fun-to-convert-number number-index) (funcall fun-to-get-weight weight-index))))

(defun cnum-get-chinese-number-and-weight (number-index weight-index)
  (cnum-get-chinese-number-and-weight-general number-index weight-index 'cnum-get-chinese-number 'cnum-get-chinese-weight))

(defun cnum-get-chinese-number-and-weight-high (number weight-index)
  (cnum-get-chinese-number-and-weight-general number weight-index 'cnum-convert-to-chinese-for-number-less-than-10000 'cnum-get-chinese-high-level-weight))

(defun cnum-get-chinese-number-weight (number weight-index disable-zero)
  (if (= number 0)
      (if disable-zero "" (get-chinese-number 0))
    (cnum-get-chinese-number-and-weight number weight-index)))

(defun cnum-convert-to-chinese-general-iter (number index next-is-zero base fun-to-convert-small-number iter-depth)
  (let ((real-number (% number base)))
  (if (< number base)
      (funcall fun-to-convert-small-number number index (if (= iter-depth 0) nil next-is-zero))
    (concat (cnum-convert-to-chinese-general-iter (/ number base) (+ index 1) (= real-number 0) base fun-to-convert-small-number (+ iter-depth 1))
	    (funcall fun-to-convert-small-number real-number index next-is-zero)))))

(defun cnum-convert-to-chinese-for-number-less-than-10000-iter (number index next-is-zero)
  (cnum-convert-to-chinese-general-iter number index next-is-zero 10 'cnum-get-chinese-number-weight 0))

(defun cnum-convert-to-chinese-for-number-iter (number index next-is-zero)
  (cnum-convert-to-chinese-general-iter number index next-is-zero 10000 'cnum-get-chinese-number-and-weight-high 0)
  )

(defun cnum-convert-to-chinese-for-number-less-than-10000 (number)
  (cnum-convert-to-chinese-for-number-less-than-10000-iter number 0 t))

(defun cnum-convert-to-chinese-for-number (number)
  (cnum-convert-to-chinese-for-number-iter number 0 t))

(defun cnum-convert-number-to-chinese (number)
  "convert a number from Arabic to Chinese format."
  (interactive "nInput the Arabic number: ")
  (message "The chinese format for number %d is %s" number (cnum-convert-to-chinese-for-number number)))

;; for test
(convert-number-to-chinese 0)
(convert-number-to-chinese 1230)
(convert-number-to-chinese 380070500)

(provide 'chinese-number)
