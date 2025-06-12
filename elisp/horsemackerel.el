(require 'json)

(defvar emacs-out-process-counter 0
  "Counter to ensure unique names when launching process")

(defun generate-buffer-json ()
  (json-encode
   `((protocol . "horsemackerel0")
     (payload . ((buffer_contentns . ,(buffer-string))
		 (line_number . ,(line-number-at-pos))
		 (column_number . ,(current-column))
		 (file_path . ,(buffer-file-name))
		 (region_beginning . ,(region-beginning))
		 (region_end . ,(region-end))
		 (region_beginning_line . ,(line-number-at-pos (region-beginning)))
		 (region_beginning_column . ,(save-excursion (goto-char (region-beginning)) (current-column)))
		 (region_end_line . ,(line-number-at-pos (region-end)))
		 (region_end_column . ,(save-excursion (goto-char (region-end)) (current-column))))))))

(defun dump-buffer-info-to-fixed-file ()
  (interactive)
  (let* ((json (generate-buffer-json))
	 (process-name (format "emacs-out-process-%d"
			       (setq emacs-out-process-counter (1+ emacs-out-process-counter)))))
    (start-process process-name nil "/tmp/horsemackerel")
    (process-send-string process-name json)
    (process-send-eof process-name)))

(global-set-key (kbd "C-c j") 'dump-buffer-info-to-fixed-file)
