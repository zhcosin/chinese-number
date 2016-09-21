
(setq chinese-number-table (list "零" "一" "二" "三" "四" "五" "六" "七" "八" "九"))
(setq traditinal-chinese-number-table (list "零" "壹" "贰" "叁" "肆" "伍" "陆" "柒" "捌" "玖"))
(setq weight-table (list "" "十" "百" "千"))
(setq heavy-weight-table (list "" "万" "亿"))

(defun get-chinese-number-from-list (number number-table)
  (if (= number 0)
      (car number-table)
    (get-chinese-number-from-list (- number 1) (cdr number-table))))

(defun get-chinese-number (number)
  (get-chinese-number-from-list number chinese-number-table))

(defun get-chinese-weight-from-list (number weight-table)
  (if (= number 0)
      (car weight-table)
    (get-chinese-weight-from-list (- number 1) (cdr weight-table))))

(defun get-chinese-weight (number)
  (get-chinese-weight-from-list number weight-table))

(defun get-chinese-heavy-weight (number)
  (get-chinese-weight-from-list number heavy-weight-table))

(defun get-chinese-number-and-weight-general (number weight-index fun-to-convert-number fun-to-get-weight)
  (if (= number 0)
      (fun-to-convert-number 0)
    (concat (funcall fun-to-convert-number number) (funcall fun-to-get-weight weight-index))))

(defun get-chinese-number-and-weight (number weight-index)
  (get-chinese-number-and-weight-general number weight-index 'get-chinese-number 'get-chinese-weight))

(defun get-chinese-number-and-weight-high (number weight-index next-is-zero)
  (get-chinese-number-and-weight-general number weight-index 'chinese-small-number 'get-chinese-heavy-weight))

(defun get-chinese-number-weight (number weight-index next-is-zero)
  (if (= number 0)
      (if next-is-zero "" (get-chinese-number 0))
    (get-chinese-number-and-weight number weight-index))
  )

(defun chinese-number-general-iter (number index next-is-zero base fun-to-convert-small-number)
  (let ((real-number (% number base)))
  (if (< number base)
      (funcall fun-to-convert-small-number number index next-is-zero)
    (concat (chinese-number-general-iter (/ number base) (+ index 1) (= real-number 0) base fun-to-convert-small-number)
	    (funcall fun-to-convert-small-number real-number index next-is-zero)))))

(defun chinese-small-number-iter (number index next-is-zero)
  (chinese-number-general-iter number index next-is-zero 10 'get-chinese-number-weight))

(defun chinese-high-number-iter (number index next-is-zero)
  (chinese-number-general-iter number index next-is-zero 10000 'get-chinese-number-and-weight-high)
  )

(defun chinese-small-number (number)
  (chinese-small-number-iter number 0 t))

(defun chinese-high-number (number)
  (chinese-high-number-iter number 0 t))

(chinese-small-number 1230)
(chinese-high-number 380070500)

(provide 'chinese-number)
