(defhydra hydra-window (:color red :hint nil)
  "
Move        Split        Windows        Misc
-------------------------------------------------------
_h_: Left   _|_: VSplit   _s_: Swap     _i_: Maximize
_j_: Right  ___: HSplit   _dw_: Delete  _t_: Transpose
_k_: Up     _v_: VSplit*              _u_: Undo
_l_: Down   _x_: HSplit*              _r_: Redo
                                  _q_: Close

Dividers
-------------------------------------------------------
_H_: Divider Left
_J_: Divider Right
_K_: Divider Up
_L_: Divider Down
  "
  ("h" windmove-left)
  ("j" windmove-down)
  ("k" windmove-up)
  ("l" windmove-right)

  ("H" hydra-move-splitter-left)
  ("J" hydra-move-splitter-down)
  ("K" hydra-move-splitter-up)
  ("L" hydra-move-splitter-right)
  ("|" (lambda ()
         (interactive)
         (split-window-right)
         (windmove-right)))
  ("_" (lambda ()
         (interactive)
         (split-window-below)
         (windmove-down)))
  ("v" split-window-right)
  ("x" split-window-below)

  ("u" winner-undo)
  ("r" winner-redo)

  ("s" ace-swap-window)

  ("dw" delete-window)
  ("i" ace-delete-other-windows)
  ("t" transpose-frame)

  ("q" nil))

(defhydra hydra-mpd (:columns 2)
  ("p" (progn (save-window-excursion
                (async-shell-command "mpc toggle" (get-buffer-create "*tmp*"))))
   "Play/Pause")
  ("/" mingus-search "Search")
  ("s" mingus "Show Mingus")
  ("<" (progn (require 'mpc) (mpc-prev)) "Previous")
  (">" (progn (require 'mpc) (mpc-next)) "Next")
  ("q" nil "Quit"))

(defhydra hydra-rectangle (:body-pre (rectangle-mark-mode 1)
                                     :color pink
                                     :hint nil
                                     :post (deactivate-mark))
  "
  ^_k_^       _w_ copy      _o_pen       _N_umber-lines
_h_   _l_     _y_ank        _t_ype       _e_xchange-point
  ^_j_^       _d_ kill      _c_lear      _r_eset-region-mark
^^^^          _u_ndo        _g_ quit     ^ ^
"
  ("k" rectangle-previous-line)
  ("j" rectangle-next-line)
  ("h" rectangle-backward-char)
  ("l" rectangle-forward-char)
  ("d" kill-rectangle)                    ;; C-x r k
  ("y" yank-rectangle)                    ;; C-x r y
  ("w" copy-rectangle-as-kill)            ;; C-x r M-w
  ("o" open-rectangle)                    ;; C-x r o
  ("t" string-rectangle)                  ;; C-x r t
  ("c" clear-rectangle)                   ;; C-x r c
  ("e" rectangle-exchange-point-and-mark) ;; C-x C-x
  ("N" rectangle-number-lines)            ;; C-x r N
  ("r" (if (region-active-p)
           (deactivate-mark)
         (rectangle-mark-mode 1)))
  ("u" undo nil)
  ("g" nil))      ;; ok
