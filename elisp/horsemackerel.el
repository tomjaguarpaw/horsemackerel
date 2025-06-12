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
  (let ((process-connection-type nil)) ; use pipe
    (let* ((json (generate-buffer-json))
           (proc (start-process "emacs-out-process" "*emacs-out-log*" "/tmp/horsemackerel")))
      (process-send-string proc json)
      (process-send-eof proc))))

(global-set-key (kbd "C-c j") 'dump-buffer-info-to-fixed-file)
