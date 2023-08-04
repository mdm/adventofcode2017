(require :uiop)

(defun read-input (filename)
    (with-open-file (stream filename)
        (read-line stream nil)))

(defun parse-input (input)
    (map 'list #'(lambda (c) (digit-char-p c)) input))

(defun rot-left(n l)
    (append (nthcdr n l) (butlast l (- (length l) n))))

(defun solve-captcha (input n)
    (let ((rotated (rot-left n input)))
        (reduce #'+
            (map 'list #'(lambda (pair)
                (if (= (first pair) (second pair)) (first pair) 0)) (mapcar #'list input rotated)))))

(defun part1 (filename)
    (solve-captcha (parse-input (read-input filename)) 1)) 

(defun part2 (filename)
    (let ((input (parse-input (read-input filename))))
        (solve-captcha input (/ (length input) 2))))

(defun day01 ()
    (format t "~d~%" (part1 (car (uiop:command-line-arguments))))
    (format t "~d~%" (part2 (car (uiop:command-line-arguments)))))

(day01)
