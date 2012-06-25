(delq 'fill erc-modules)
(setq erc-autojoin-channels-alist '(("irc.prison.net" "#avendar"))
      erc-timestamp-only-if-changed-flag nil
      erc-timestamp-format "[%H:%M:%S] "
      erc-insert-timestamp-function 'erc-insert-timestamp-left)
(erc :server "irc.freenode.net" :full-name "Zane Shelby" :nick "zanes")
(erc :server "irc.prison.net" :full-name "Zane Shelby" :nick "vaeshir")


