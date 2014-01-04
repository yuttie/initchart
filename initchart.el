;;; initchart.el --- Emacs' init process performance visualization

;; Copyright (C) 2013  Yuta Taniguchi

;; Author: Yuta Taniguchi <yuta.taniguchi.y.t@gmail.com>
;; Keywords: init, performance analysis, visualization

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; initchart.el provides macros and functions to measure and visualize a init
;; process of Emacs.
;; 
;; First, one need to record the execution time of some primary functions such
;; as 'load' and 'require'.  Use the macro 'initchart-record-execution-time-of'
;; at the beginning of your init.el to register functions of concern, and then
;; launch an Emacs process as usual.  Execution time of the registered functions
;; will be logged in the '*Messages*' buffer.
;; 
;; Then, you can visualize these logs by invoking the command
;; 'initchart-visualize-init-sequence', which will ask you the filepath to save
;; an output SVG image.
;; 
;; Example:
;;   (require 'initchart)
;;   (initchart-record-execution-time-of load file)
;;   (initchart-record-execution-time-of require feature)

;;; Code:

(defmacro initchart-record-execution-time-of (fn arg)
  `(defadvice ,fn (around ,(intern (concat "initchart-record-execution-time-of-" (symbol-name fn))) activate compile)
     (let ((start-time (current-time)))
       ad-do-it
       (let ((end-time (current-time)))
         (message "exec-time: %s(%s) %f %f" (symbol-name ',fn) ,arg (float-time start-time) (float-time end-time))))))

(defun initchart-visualize-init-sequence ()
  ""
  (interactive)
  (flet ((parse (line)
                (and (string-match "^exec-time: \\([^ ]+\\) \\([^ ]+\\) \\([^ ]+\\)$" line)
                     `(,(match-string 1 line)
                       ,(string-to-number (match-string 2 line))
                       ,(string-to-number (match-string 3 line)))))
         (log< (x y) (< (nth 1 x) (nth 1 y)))
         (inside (x y)
                 (let ((a (nth 1 x))
                       (b (nth 2 x))
                       (c (nth 1 y))
                       (d (nth 2 y)))
                   (and (<= c a) (<= b d))))
         (mktree (logs)
                 (let ((stack '((root . ()))))
                   (dolist (log (sort logs #'log<))
                     ;; find the parent of the current log
                     (while (not (or (eq (caar stack) 'root)
                                     (inside log (caar stack))))
                       (let ((subtree (pop stack))
                             (parent  (car stack)))
                         (setcdr parent (append (cdr parent) (list subtree)))))
                     ;; make a new node for the current log
                     (let ((new-node `(,log . ())))
                       (push new-node stack)))
                   (while (not (eq (caar stack) 'root))
                     (pop stack))
                   (car stack)))
         (depth (tree)
                (let ((subtrees (cdr tree)))
                  (1+ (apply #'max
                             (cons 0 (mapcar #'depth subtrees))))))
         (render (log-tree)
                 (let* ((top-level-nodes (cdr log-tree))
                        (time-min        (nth 1 (car (car top-level-nodes))))
                        (time-max        (nth 2 (car (car (last top-level-nodes)))))
                        (level-max       (depth log-tree))
                        (offset          time-min)
                        (scale           (/ 2000 (- time-max time-min))))
                   (flet ((render-log (log level)
                                      (let* ((name       (nth 0 log))
                                             (start-time (nth 1 log))
                                             (end-time   (nth 2 log))
                                             (x          (* scale (- start-time offset)))
                                             (y          (* 1.1 level))
                                             (width      (* scale (- end-time start-time))))
                                        (format "<g><rect x=\"%fpx\" y=\"%fem\" width=\"%f\" height=\"1.1em\" fill=\"silver\"></rect><text x=\"%fpx\" y=\"%fem\">%s</text></g>"
                                                x y width        ; rect
                                                x (+ y 1.0) name ; text
                                                ))))
                     (mapconcat #'identity
                                `(,(format "<svg xmlns=\"http://www.w3.org/2000/svg\" version=\"1.1\" baseProfile=\"full\" width=\"%fpx\" height=\"%fem\">"
                                           (* scale (- time-max time-min))
                                           (* 1.1 level-max))
                                  ,@(let ((stack    (mapcar (lambda (node) (cons node 0)) (cdr log-tree)))
                                          (rendered '()))
                                      (while (not (null stack))
                                        (let* ((node-and-level (pop stack))
                                               (node           (car node-and-level))
                                               (level          (cdr node-and-level))
                                               (log            (car node))
                                               (subtrees       (mapcar (lambda (x) (cons x (1+ level))) (cdr node))))
                                          (push (render-log log level) rendered)
                                          (setq stack (append subtrees stack))))
                                      rendered)
                                  "</svg>")
                                "\n")))))
    (set-buffer "*Messages*")
    (let* ((lines (split-string (buffer-substring-no-properties (point-min) (point-max)) "\n" t))
           (logs (delq nil (mapcar #'parse lines)))
           (log-tree (mktree logs))
           (fp (read-file-name "SVG filename:")))
      (with-temp-buffer
        (insert (render log-tree))
        (when (file-writable-p fp)
          (write-region (point-min) (point-max) fp))))))

(add-hook 'after-init-hook
          (lambda ()
            (message "exec-time: init %f %f"
                     (float-time before-init-time)
                     (float-time after-init-time))))

(provide 'initchart)

;;; initchart.el ends here
