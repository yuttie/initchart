;;; initchart.el --- Emacs' init process performance visualization  -*- lexical-binding: t; -*-

;; Copyright (C) 2013  Yuta Taniguchi

;; Author: Yuta Taniguchi <yuta.taniguchi.y.t@gmail.com>
;; Keywords: init, performance analysis, visualization
;; Version: 0.1.0

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
;; will be logged in the '*initchart*' buffer.
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

(eval-when-compile
  (require 'cl-lib))

(defun initchart-log (name start-time end-time &optional sub-name)
  (with-current-buffer "*initchart*"
    (insert (format "exec-time: %s %f %f\n"
                    (if sub-name (format "%s(%s)" name sub-name) name)
                    (float-time start-time)
                    (float-time end-time)))))

(defun initchart-log-init ()
  (when (and before-init-time after-init-time)
    (initchart-log "init" before-init-time after-init-time)
    t))

(defmacro initchart-record-execution-time-of (fn arg)
  `(defadvice ,fn (around ,(intern (concat "initchart-record-execution-time-of-" (symbol-name fn))) activate compile)
     (let ((start-time (current-time)))
       ad-do-it
       (let ((end-time (current-time)))
         (initchart-log (symbol-name ',fn)
                        start-time
                        end-time
                        ,arg)))))

(defun initchart-visualize-init-sequence (&optional fp)
  ""
  (interactive)
  (cl-labels ((parse (line)
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
              (find-node (name tree)
                         (if (and (listp (car tree))
                                  (equal (caar tree) name))
                             tree
                           (let ((children (cdr tree))
                                 (found nil))
                             (while (and (not found)
                                         (not (null children)))
                               (let* ((subtree (pop children))
                                      (res (find-node name subtree)))
                                 (when res (setq found res))))
                             found)))
              (render (log-tree)
                      (let* ((top-level-nodes (cdr log-tree))
                             (time-min        (nth 1 (car (car top-level-nodes))))
                             (time-max        (nth 2 (car (car (last top-level-nodes)))))
                             (level-max       (depth log-tree))
                             (offset          time-min)
                             (scale           1000))  ; 1 millisecond == 1 px
                        (cl-flet ((render-log (log level)
                                              (let* ((name       (nth 0 log))
                                                     (start-time (nth 1 log))
                                                     (end-time   (nth 2 log))
                                                     (x          (* scale (- start-time offset)))
                                                     (y          (* 1.1 level))
                                                     (width      (* scale (- end-time start-time))))
                                                (format "<g><rect x=\"%.3fpx\" y=\"%.1fem\" width=\"%f\" height=\"1.1em\" fill=\"hsl(%f, 100%%, 35%%)\"/><text x=\"%.3fpx\" y=\"%.1fem\">%s</text></g>"
                                                        x y width (* 240 (exp (* -0.01 width)))  ; rect
                                                        x (+ y 1.0) name  ; text
                                                        ))))
                          (mapconcat #'identity
                                     `(,(format "<svg xmlns=\"http://www.w3.org/2000/svg\" version=\"1.1\" baseProfile=\"full\" width=\"%fpx\" height=\"%.1fem\">"
                                                (* scale (- time-max time-min))
                                                (* 1.1 level-max))
                                       "<style>"
                                       "  line.major { stroke: black; stroke-width: 2; }"
                                       "  line.minor { stroke: gray;  stroke-width: 1; stroke-dasharray: 5, 5; }"
                                       "  text.major,"
                                       "  text.minor { visibility: visible; }"
                                       "  rect { opacity: 0.5; }"
                                       "  text { visibility: hidden; }"
                                       "  rect:hover { opacity: 1; stroke: black; stroke-width: 2px; }"
                                       "  rect:hover + text { visibility: visible; }"
                                       "</style>"
                                       ,@(mapcar (lambda (i)
                                                   (let ((x (/ (* 1000 i) 10)))
                                                     (concat
                                                      (format "<line class=\"minor\" x1=\"%dpx\" y1=\"%.1fem\" x2=\"%dpx\" y2=\"%.1fem\"/>"
                                                              x 0
                                                              x (* 1.1 level-max))
                                                      (format "<text class=\"minor\" x=\"%dpx\" y=\"%.1fem\">%dms</text>"
                                                              x (* 1.1 level-max)
                                                              x))))
                                                 (number-sequence 0 (ceiling (* 10 (- time-max offset)))))
                                       ,@(mapcar (lambda (i)
                                                   (let ((x (* 1000 i)))
                                                     (format "<line class=\"major\" x1=\"%dpx\" y1=\"%.1fem\" x2=\"%dpx\" y2=\"%.1fem\"/>"
                                                             x 0
                                                             x (* 1.1 level-max))))
                                                 (number-sequence 0 (ceiling (- time-max offset))))
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
                                       "<script><![CDATA["
                                       "    var rects = document.querySelectorAll('rect');"
                                       "    for (var i = 0; i < rects.length; ++i) {"
                                       "        rects[i].addEventListener('mousemove', function(e) {"
                                       "            var t = e.target.parentNode.querySelector('text');"
                                       "            t.setAttribute('x', e.pageX + 16);"
                                       "            t.setAttribute('y', e.pageY + 16);"
                                       "        });"
                                       "    }"
                                       "]]></script>"
                                       "</svg>")
                                     "\n")))))
    (let* ((lines (split-string (with-current-buffer "*initchart*"
                                  (buffer-substring-no-properties (point-min) (point-max)))
                                "\n"
                                t))
           (logs (delq nil (mapcar #'parse lines)))
           (log-tree (mktree logs))
           (fp (or fp (read-file-name "SVG filename:"))))
      (with-temp-buffer
        (let* ((init-node (find-node "init" log-tree))
               (init-tree `(root . (,init-node))))
          (insert (render init-tree)))
        (when (file-writable-p fp)
          (write-region (point-min) (point-max) fp))))))

(add-hook 'after-init-hook #'initchart-log-init)

(get-buffer-create "*initchart*")

(provide 'initchart)

;;; initchart.el ends here
