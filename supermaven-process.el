
(defvar supermaven-process nil)

(defconst supermaven-buffer "*supermaven-messages*")

(defun supermaven-filter(process string)
  (let ((buf(get-buffer-create "*supermaven*")))
	(save-excursion
	  (set-buffer buf)
	  (goto-char (point-max))
	  (insert string)))

  (when (string-prefix-p "SM-MESSAGE" string)
	(let ((json (json-parse-string (substring string 10))))
	  (supermaven-process json)))
)

(defun supermaven-process-start()
  (when supermaven-process
	(delete-process supermaven-process))
  (setq supermaven-process (make-process :name "supermaven"
										 :command '("/home/chep/outils/supermaven/sm-agent" "stdio")
										 :connection-type 'pipe
										 :filter 'supermaven-filter)))

(defun supermaven-greetings()
  (let ((json (json-serialize #s(hash-table test equal data ("kind" "greeting")))))
	(process-send-string supermaven-process (concat json "\n")))
  )

(defun supermaven-process(json)
  (cond ((string= (gethash "kind" json) "response")
		 (supermaver-update-state-id json))
		((string= (gethash "kind" json) "metadata")
		 (supermaver-update-metadata json))
		((string= (gethash "kind" json) "activation_request")
		 (save-excursion
		   (set-buffer (get-buffer-create supermaven-messages))
		   (goto-char (point-max))
		   (insert (concat "Activation request : "
						   (gethash "activateUrl" json) "\n"))))
		((string= (gethash "kind" json) "activation_success")
		 (save-excursion
		   (set-buffer (get-buffer-create supermaven-messages))
		   (goto-char (point-max))
		   (insert "Supermaven was activated successfully.\n")))
		))

(defun supermaven-send(message)
	(process-send-string supermaven-process (concat message "\n"))
)

(defun supermaver-update-state-id(json))

(defun supermaver-update-metadata(json))


(provide 'supermaven-process)
