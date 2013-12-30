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
         (log< (x y) (< (cadr x) (cadr y)))
         (render (logs)
                 (let* ((times (mapcar #'cadr logs))
                        (time-min (apply #'min times))
                        (time-max (apply #'max times))
                        (offset time-min)
                        (scale (/ 2000 (- time-max time-min)))
                        (y 0)
                        )
                   (flet ((render-log (log)
                                      (let* ((name (car log))
                                             (start-time (cadr log))
                                             (end-time (caddr log))
                                             (str (format "<g><rect x=\"%fpx\" y=\"%fem\" width=\"%f\" height=\"1.1em\" fill=\"silver\"></rect><text x=\"%fpx\" y=\"%fem\">%s</text></g>"
                                                          (* scale (- start-time offset))
                                                          y
                                                          (* scale (- end-time start-time))
                                                          (* scale (- start-time offset))
                                                          (+ y 1.0)
                                                          name)))
                                        (incf y 1.1)
                                        str)))
                     (mapconcat #'identity
                                `(,(format "<svg xmlns=\"http://www.w3.org/2000/svg\" version=\"1.1\" baseProfile=\"full\" width=\"%fpx\" height=\"%fem\">"
                                           2000
                                           (* 1.1 (length logs)))
                                  ,@(mapcar #'render-log logs)
                                  "</svg>")
                                "\n")))))
    (set-buffer "*Messages*")
    (let* ((lines (split-string (buffer-substring-no-properties (point-min) (point-max)) "\n" t))
           (logs (delq nil (mapcar #'parse lines)))
           (sorted-logs (sort logs #'log<))
           (fp (read-file-name "SVG filename:")))
      (with-temp-buffer
        (insert (render sorted-logs))
        (when (file-writable-p fp)
          (write-region (point-min) (point-max) fp))))))

(add-hook 'after-init-hook
          (lambda ()
            (message "exec-time: init %f %f"
                     (float-time before-init-time)
                     (float-time after-init-time))))

(provide 'initchart)

;;; initchart.el ends here
