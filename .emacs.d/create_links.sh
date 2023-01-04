#!/bin/bash



rm ~/.emacs.d/early-init.el
rm ~/.emacs.d/init.el

ln -s ~/dotfiles/.emacs.d/early-init.el ~/.emacs.d/early-init.el

ln -s ~/dotfiles/.emacs.d/init.el ~/.emacs.d/init.el
