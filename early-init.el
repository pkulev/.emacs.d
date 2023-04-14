;; -*- lexical-binding: t -*-
;; This file was tangled (automatically generated) from `readme.org'

(require 'tool-bar)

(setq load-prefer-newer t)
(when (display-graphic-p)
  (scroll-bar-mode 0))
(tool-bar-mode 0)
(menu-bar-mode 0)
(blink-cursor-mode 0)
(setq frame-resize-pixelwise t)
(add-to-list 'default-frame-alist '(fullscreen . maximized))
