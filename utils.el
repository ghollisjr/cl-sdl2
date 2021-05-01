(defun fixcpp ()
  (interactive)
  (setq last-kbd-macro
        "\C-f\206\206\206\202\"\206\"\C-a\206\206\202")
  (goto-char (point-min))
  (replace-string "*" " :pointer ")
  (goto-char (point-min))
  (replace-string "const" "")
  (goto-char (point-min))
  (replace-string "," ")(")
  (goto-char (point-min))
  (replace-string ";" ")")
  (goto-char (point-min))
  (replace-regexp "^" "(defcfun ")
  (goto-char (point-min))
  (replace-string "SDLCALL" "")
  (goto-char (point-min))
  (replace-string "extern DECLSPEC" "")
  (goto-char (point-min))
  (replace-string "(void)" "")
  (goto-char (point-min))
  ;; (let ((nlines
  ;;        (count-lines (point-min) (point-max))))
  ;;   (dotimes (i nlines)
  ;;     (call-last-kbd-macro)))
  )


;; (goto-char (point-min)) ; go to start of buffer
;; (goto-char (point-max)) ; go to end of buffer
