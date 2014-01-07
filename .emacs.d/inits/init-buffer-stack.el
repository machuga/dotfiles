(customize-set-variable 'buffer-stack-untracked (quote ("KILL" 
                                                        "*Compile-Log*" "*Compile-Log-Show*"
                                                        "*Warnings*" "*Help*"
                                                        "*Group*" 
                                                        "*Completions*" 
                                                        "*Messages*" 
                                                        "*Packages*"
                                                        "*magit-process*" "*magit-commit*" "*magit-edit-log"
                                                        "*Ediff Registry*")))
(global-set-key (kbd "C-l") 'buffer-stack-up)
(global-set-key (kbd "C-h") 'buffer-stack-down)
