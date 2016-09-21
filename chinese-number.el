
(setq chinese-number-table (list "零" "一" "二" "三" "四" "五" "六" "七" "八" "九"))
(setq traditinal-chinese-number-table (list "零" "壹" "贰" "叁" "肆" "伍" "陆" "柒" "捌" "玖"))
(setq weight-table (list "" "十" "百" "千" "万" "亿"))

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

(defun get-chinese-number-and-weight (number weight-index)
  (if (= number 0)
      (get-chinese-number 0)
    (concat (get-chinese-number number) (get-chinese-weight weight-index)))
    )

(defun get-chinese-number-weight (number weight-index next-is-zero)
  (if (= number 0)
      (if next-is-zero "" (get-chinese-number 0))
    (get-chinese-number-and-weight number weight-index))
  )

(defun chinese-small-number-iter (number index next-is-zero)
  (let ((real-number (% number 10)))
  (if (< number 10)
      (get-chinese-number-weight number index next-is-zero)
    (concat (chinese-small-number-iter (/ number 10) (+ index 1) (= real-number 0))
	    (get-chinese-number-weight real-number index next-is-zero)))))

(defun chinese-small-number (number)
  (chinese-small-number-iter number 0 t))

(chinese-small-number 1209)
