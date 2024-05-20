(require 'supermaven-process)

(defvar supermaven-next-id 0)

(defun supermaven-start()
  (interactive)
  (supermaven-process-start)
  (setq supermaven-next-id 0))

(defun supermaven-use-free()
  (interactive)
  (process-send-string supermaven-process
					   (concat (json-serialize #s(hash-table test equal data ("kind" "use_free_version")))
							   "\n")))


(defun supermaven-on-update (buffer)
  (let ((hash (make-hash-table :test 'equal))
		(file (make-hash-table :test 'equal))
		(cursor (make-hash-table :test 'equal)))
	(puthash "newId" (number-to-string supermaven-next-id) hash)
	(setq supermaven-next-id (+ 1 supermaven-next-id))
	(puthash "kind" "state_update" hash)

	(puthash "kind" "file_update" file)
	(puthash "path" (buffer-file-name buffer) file)
	(with-current-buffer buffer
	  (puthash "content" (buffer-substring-no-properties (point-min) (point-max)) file))

	(puthash "kind" "cursor_update" cursor)
	(puthash "path" (buffer-file-name buffer) cursor)
	(puthash "offset" (point) cursor)

	(puthash "updates" (vector file cursor) hash)

	(supermaven-send (json-serialize hash))
	))
