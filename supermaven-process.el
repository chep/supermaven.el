(require 'json)

(defconst supermaven-buffer "*supermaven-messages*")
(defcustom supermaven-blob-path "/home/chep/outils/supermaven/sm-agent"
  "Path to the supermaven blob"
  :group 'supermaven)

(defvar supermaven-process nil)
(defvar supermaven-compl-callback nil)
(defvar supermaven-current-state-id -1)
(defvar supermaven-current-compl "")

(defun supermaven-filter(process string)
  (let ((buf(get-buffer-create "*supermaven*")))
	(save-excursion
	  (set-buffer buf)
	  (goto-char (point-max))
	  (insert string)))

  (while (string-match "SM-MESSAGE" string)
	(let ((offset (string-match "SM-MESSAGE" string))
		  (next (string-match "SM-MESSAGE" string 10)))
	  (when offset
		(supermaven-process (json-parse-string (substring string 11 (if next next (length string))))))
	  (if next
		  (setq string (substring string next))
		(setq string "")))))

(defun supermaven-process-start(compl-callback)
  (when supermaven-process
	(delete-process supermaven-process))
  (setq supermaven-compl-callback compl-callback)
  (setq supermaven-process (make-process :name "supermaven"
										 :command (list supermaven-blob-path "stdio")
										 :connection-type 'pipe
										 :filter 'supermaven-filter)))

(defun supermaven-greetings()
  (let ((json (json-serialize #s(hash-table test equal data ("kind" "greeting")))))
	(process-send-string supermaven-process (concat json "\n")))
  )

(defun supermaven-process(json)
  (cond ((string= (gethash "kind" json) "response")
		 (supermaven-update-state-id json))
		((string= (gethash "kind" json) "metadata")
		 (supermaven-update-metadata json))
		((string= (gethash "kind" json) "activation_request")
		 (save-excursion
		   (set-buffer (get-buffer-create supermaven-buffer))
		   (goto-char (point-max))
		   (insert (concat "Activation request : "
						   (gethash "activateUrl" json) "\n"))))
		((string= (gethash "kind" json) "activation_success")
		 (save-excursion
		   (set-buffer (get-buffer-create supermaven-buffer))
		   (goto-char (point-max))
		   (insert "Supermaven was activated successfully.\n")))
		))

(defun supermaven-send(message)
	(process-send-string supermaven-process (concat message "\n"))
)

(defun supermaven-update-state-id(json)
  (let ((state-id (string-to-number (gethash "stateId" json))))
	(if (= state-id supermaven-current-state-id)
		(let ((items (gethash "items" json)))
		  (seq-doseq (item items)
			(cond ((string= (gethash "kind" item) "text")
				   (setq supermaven-current-compl (concat supermaven-current-compl (gethash "text" item))))
				  ((string= (gethash "kind" item) "end")
				   (when supermaven-compl-callback
					 (supermaven-compl-callback supermaven-current-compl)
					 (setq supermaven-current-compl ""
						   supermaven-current-state-id (1+ supermaven-current-state-id)))))))
	  (progn
		(setq supermaven-current-compl ""
			  supermaven-current-state-id state-id)
		(supermaven-update-state-id json)))))


(defun supermaven-update-metadata(json))

(provide 'supermaven-process)
