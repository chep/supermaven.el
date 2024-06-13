(require 'supermaven-process)

(defvar supermaven-next-id 0)

(defvar-local supermaven-overlay nil
  "Overlay for Supermaven completion.")

(defface supermaven-overlay-face
  '((t :inherit shadow))
  "Face for Supermaven overlay.")


(defun supermaven-start()
  (interactive)
  (supermaven-process-start 'supermaven-compl-callback)
  (setq supermaven-next-id 0))

(defun supermaven-use-free()
  (interactive)
  (process-send-string supermaven-process
					   (concat (json-serialize #s(hash-table test equal data ("kind" "use_free_version")))
							   "\n")))

(defun supermaven-complete()
  (interactive)
  (supermaven-on-update (current-buffer) t))

(defun supermaven-on-update (buffer do-send-file)
  (let* ((hash (make-hash-table :test 'equal))
		 (updates))
	(puthash "newId" (number-to-string supermaven-next-id) hash)
	(setq supermaven-next-id (+ 1 supermaven-next-id))
	(puthash "kind" "state_update" hash)

	;; always cursor
	(let ((cursor (make-hash-table :test 'equal)))
	  (puthash "kind" "cursor_update" cursor)
	  (puthash "path" (buffer-file-name buffer) cursor)
	  (puthash "offset" (point) cursor)
	  (setq updates (vector cursor)))

	(when do-send-file
	  (let ((file (make-hash-table :test 'equal)))
		(puthash "kind" "file_update" file)
		(puthash "path" (buffer-file-name buffer) file)
		(with-current-buffer buffer
		  (puthash "content" (buffer-substring-no-properties (point-min) (point-max)) file))
		(setq updates (vconcat updates (vector file)))))

	(puthash "updates" updates hash)

	(supermaven-send (json-serialize hash))))

(defun supermaven-compl-callback(completion)
  (supermaven-clear-overlay)
  (let* ((ov (supermaven-get-overlay))
		 (tail (buffer-substring (point) (line-end-position)))
		 (p-completion (concat (propertize completion 'face 'supermaven-overlay-face) tail)))
	(move-overlay ov (point) (line-end-position))

	(if (eolp)
        (progn
          (overlay-put ov 'after-string "") ; make sure posn is correct
		  (put-text-property 0 1 'cursor t p-completion)
		  (overlay-put ov 'display "")
          (overlay-put ov 'after-string p-completion))
	  (overlay-put ov 'display (substring p-completion 0 1))
      (overlay-put ov 'after-string (substring p-completion 1)))
	(overlay-put ov 'completion completion)
	(overlay-put ov 'start (point))))


(defun supermaven-get-overlay ()
  "Create or get overlay for Supermaven."
  (unless (overlayp supermaven-overlay)
    (setq supermaven-overlay (make-overlay 1 1 nil nil t)))
  supermaven-overlay)

(defun supermaven-clear-overlay ()
  (when (overlayp supermaven-overlay)
	(message "clear")
	(delete-overlay supermaven-overlay)))

(defun supermaven-accept-completion ()
  (interactive)
  (let* ((ov (supermaven-get-overlay))
		 (completion (overlay-get ov 'completion)))
	(insert completion)
	(supermaven-clear-overlay)))

(defun supermaven-reject-completion ()
  (interactive)
  (supermaven-clear-overlay))

(defun supermaven-complete ()
  (interactive)
  (supermaven-on-update (current-buffer) t))
