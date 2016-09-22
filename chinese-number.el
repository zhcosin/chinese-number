(defvar chinese-number-table (list "零" "一" "二" "三" "四" "五" "六" "七" "八" "九"))
(defvar traditinal-chinese-number-table (list "零" "壹" "贰" "叁" "肆" "伍" "陆" "柒" "捌" "玖"))
(defvar weight-table (list "" "十" "百" "千"))
(defvar heavy-weight-table (list "" "万" "亿"))


(defun get-chinese-number (number)
  (nth number chinese-number-table))

(defun get-chinese-weight (number)
  (nth number weight-table))

(defun get-chinese-heavy-weight (number)
  (nth number heavy-weight-table))

(defun get-chinese-number-and-weight-general (number weight-index fun-to-convert-number fun-to-get-weight)
  (if (= number 0)
      (funcall fun-to-convert-number 0)
    (concat (funcall fun-to-convert-number number) (funcall fun-to-get-weight weight-index))))

(defun get-chinese-number-and-weight (number weight-index)
  (get-chinese-number-and-weight-general number weight-index 'get-chinese-number 'get-chinese-weight))

(defun get-chinese-number-and-weight-high (number weight-index next-is-zero)
  (get-chinese-number-and-weight-general number weight-index 'chinese-small-number 'get-chinese-heavy-weight))

(defun get-chinese-number-weight (number weight-index disable-zero)
  (if (= number 0)
      (if disable-zero "" (get-chinese-number 0))
    (get-chinese-number-and-weight number weight-index))
  )

(defun chinese-number-general-iter (number index next-is-zero base fun-to-convert-small-number iter-depth)
  (let ((real-number (% number base)))
  (if (< number base)
      (funcall fun-to-convert-small-number number index (if (= iter-depth 0) nil next-is-zero))
    (concat (chinese-number-general-iter (/ number base) (+ index 1) (= real-number 0) base fun-to-convert-small-number (+ iter-depth 1))
	    (funcall fun-to-convert-small-number real-number index next-is-zero)))))

(defun chinese-small-number-iter (number index next-is-zero)
  (chinese-number-general-iter number index next-is-zero 10 'get-chinese-number-weight 0))

(defun chinese-high-number-iter (number index next-is-zero)
  (chinese-number-general-iter number index next-is-zero 10000 'get-chinese-number-and-weight-high 0)
  )

(defun chinese-small-number (number)
  (chinese-small-number-iter number 0 t))

(defun chinese-high-number (number)
  (chinese-high-number-iter number 0 t))

(defun convert-number-to-chinese (number)
  "convert a number from Arbic to Chinese format."
  (interactive "nInput the Arbic number: ")
  (message "The chinese format for number %d is %s" number (chinese-high-number number)))

;; for test
;;(convert-number-to-chinese 0)
;;(convert-number-to-chinese 1230)
;;(convert-number-to-chinese 380070500)

(provide 'chinese-number)
