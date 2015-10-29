(setq powerline-arrow-shape 'arrow)

;(custom-set-faces
; '(mode-line ((t (:foreground "#040404" :background "#bdbdbd" :box nil))))
; '(mode-line-inactive ((t (:foreground "#f9f9f9" :background "#666666" :box nil)))))

(custom-set-faces
 '(mode-line ((t (:box nil))))
 '(mode-line-inactive ((t (:box nil)))))

(let ((mlf (list "%e"
                    '(:eval (concat
                             (powerline-evil-mode      'left      nil  )
                             (powerline-lcl            'left      nil  )
                             (powerline-rmw            'left      nil  )
                             (powerline-buffer-id      'left      nil               powerline-color2  )
                             (powerline-row            nil        powerline-color2  powerline-color2  )
                             (powerline-make-text      " :"       powerline-color2  powerline-color2 )
                             (powerline-column         nil        powerline-color2  powerline-color2 )
                             (powerline-make-text      " "        powerline-color2  powerline-color2 )
                             (powerline-major-mode     'left      powerline-color1  powerline-color1)
                             (powerline-make-text      " "        powerline-color1  powerline-color2 )
                             (powerline-make-text      " "        nil  powerline-color2 )
                             (powerline-minor-modes    nil        nil powerline-color2)
                             (powerline-make-text      " "        nil powerline-color2 )
                             (powerline-percent        'left      powerline-color2  powerline-color2  )
                             (powerline-vc             nil        nil  powerline-color1)
                             (powerline-make-text      " "        nil powerline-color2 ))))))
  (setq-default mode-line-format mlf)
  (setq mode-line-format mlf))
