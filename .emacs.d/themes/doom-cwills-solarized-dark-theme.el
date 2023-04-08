;;; doom-cwills-solarized-dark-theme.el --- a dark variant of Solarized -*- lexical-binding: t; no-byte-compile: t; -*-
;;
;; Added: July 15, 2019 (#303)
;; Author: ema2159 <https://github.com/ema2159>
;; Maintainer:
;; Source: https://github.com/bbatsov/solarized-emacs
;; Source: https://ethanschoonover.com/solarized
;;
;;; Commentary:
;;; Code:

(require 'doom-themes)


;;
;;; Variables

(defgroup doom-cwills-solarized-dark-theme nil
  "Options for the `doom-cwills-solarized-dark' theme."
  :group 'doom-themes)

(defcustom doom-cwills-solarized-dark-brighter-modeline nil
  "If non-nil, more vivid colors will be used to style the mode-line."
  :group 'doom-cwills-solarized-dark-theme
  :type 'boolean)

(defcustom doom-cwills-solarized-dark-brighter-comments nil
  "If non-nil, comments will be highlighted in more vivid colors."
  :group 'doom-cwills-solarized-dark-theme
  :type 'boolean)

(defcustom doom-cwills-solarized-dark-brighter-text nil
  "If non-nil, default text will be brighter."
  :group 'doom-cwills-solarized-dark-theme
  :type 'boolean)

(defcustom doom-cwills-solarized-dark-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 8px padding to the mode-line.
Can be an integer to determine the exact padding."
  :group 'doom-cwills-solarized-dark-theme
  :type '(choice integer boolean))


;;
;;; Theme definition

(def-doom-theme doom-cwills-solarized-dark
  "A dark theme inspired by VS Code Solarized Dark"

  ;; name        default   256       16
  ((bg         '("#002b36" "#002b36" "brightwhite" ))
   (fg         (if doom-cwills-solarized-dark-brighter-text
                   '("#BBBBBB" "#BBBBBB" "brightwhite")
                 '("#839496" "#839496" "brightwhite")))

   ;; These are off-color variants of bg/fg, used primarily for `solaire-mode',
   ;; but can also be useful as a basis for subtle highlights (e.g. for hl-line
   ;; or region), especially when paired with the `doom-darken', `doom-lighten',
   ;; and `doom-blend' helper functions.
   (bg-alt     '("#00212B" "#00212B" "white"       ))
   (fg-alt     '("#657b83" "#657b83" "white"       ))

   ;; These should represent a spectrum from bg to fg, where base0 is a starker
   ;; bg and base8 is a starker fg. For example, if bg is light grey and fg is
   ;; dark grey, base0 should be white and base8 should be black.
   (base0      '("#073642" "#073642" "black"       ))
   (base1      '("#03282F" "#03282F" "brightblack" ))
   (base2      '("#00212C" "#00212C" "brightblack" ))
   (base3      '("#13383C" "#13383C" "brightblack" ))
   (base4      '("#56697A" "#56697A" "brightblack" ))
   (base5      '("#405A61" "#405A61" "brightblack" ))
   (base6      '("#96A7A9" "#96A7A9" "brightblack" ))
   (base7      '("#788484" "#788484" "brightblack" ))
   (base8      '("#626C6C" "#626C6C" "white"       ))

   (grey       base4)
   (red        '("#dc322f" "#ff6655" "red"          ))
   (orange     '("#cb4b16" "#dd8844" "brightred"    ))
   (green      '("#859900" "#99bb66" "green"        ))
   (teal       '("#35a69c" "#33aa99" "brightgreen"  ))
   (yellow     '("#b58900" "#ECBE7B" "yellow"       ))
   (blue       '("#268bd2" "#51afef" "brightblue"   ))
   (dark-blue  '("#3F88AD" "#2257A0" "blue"         ))
   (magenta    '("#d33682" "#c678dd" "magenta"      ))
   (violet     '("#6c71c4" "#a9a1e1" "brightmagenta"))
   (cyan       '("#2aa198" "#46D9FF" "brightcyan"   ))
   (dark-cyan  '("#204052" "#5699AF" "cyan"         ))

   ;; face categories -- required for all themes
   (highlight      blue)
   (vertical-bar   (doom-darken base1 0.5))
   (selection      dark-blue)
   (builtin        blue)
   (comments       (if doom-cwills-solarized-dark-brighter-comments blue base5))
   (doc-comments   teal)
   (constants      magenta)
   (functions      blue)
   (keywords       green)
   (methods        cyan)
   (operators      orange)
   (type           yellow)
   (strings        cyan)
   (variables      violet)
   (numbers        magenta)
   (region         base0)
   (error          red)
   (warning        yellow)
   (success        green)
   (vc-modified    yellow)
   (vc-added       green)
   (vc-deleted     red)

   ;; custom categories
   (-modeline-bright doom-cwills-solarized-dark-brighter-modeline)
   (-modeline-pad
    (when doom-cwills-solarized-dark-padded-modeline
      (if (integerp doom-cwills-solarized-dark-padded-modeline) doom-cwills-solarized-dark-padded-modeline 8)))

   (modeline-fg     nil)
   (modeline-fg-alt base5)

   (modeline-bg
    (if -modeline-bright
        base3
      `(,(doom-darken (car bg) 0.1) ,@(cdr base0))))
   (modeline-bg-alt
    (if -modeline-bright
        base3
      `(,(doom-darken (car bg) 0.15) ,@(cdr base0))))
   (modeline-bg-inactive     `(,(car bg-alt) ,@(cdr base1)))
   (modeline-bg-inactive-alt (doom-darken bg 0.1)))


  ;;;; Base theme face overrides
  (((font-lock-comment-face &override)
    :background (if doom-cwills-solarized-dark-brighter-comments (doom-lighten bg 0.05)))
   ((font-lock-keyword-face &override) :weight 'bold)
   ;;((font-lock-constant-face &override) :weight 'bold)
   ((font-lock-constant-face &override) :foreground "#268bd2" :weight 'bold)
   ((line-number &override) :foreground base4)
   ((line-number-current-line &override) :foreground fg)
   (mode-line
    :background modeline-bg :foreground modeline-fg
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,bg)))
   (mode-line-inactive
    :background modeline-bg-inactive :foreground modeline-fg-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,bg)))
   (mode-line-emphasis :foreground (if -modeline-bright base8 highlight))

   ;;;; centaur-tabs
   (centaur-tabs-active-bar-face :background blue)
   (centaur-tabs-modified-marker-selected
    :inherit 'centaur-tabs-selected :foreground blue)
   (centaur-tabs-modified-marker-unselected
    :inherit 'centaur-tabs-unselected :foreground blue)
   ;;;; company
   (company-tooltip-selection     :background dark-cyan)
   ;;;; css-mode <built-in> / scss-mode
   (css-proprietary-property :foreground orange)
   (css-property             :foreground green)
   (css-selector             :foreground blue)
   ;;;; doom-modeline
   (doom-modeline-bar :background blue)
   (doom-modeline-bar-inactive :background base5)
   (doom-modeline-evil-emacs-state  :foreground magenta)
   (doom-modeline-evil-insert-state :foreground blue)
   ;;;; elscreen
   (elscreen-tab-other-screen-face :background "#353a42" :foreground "#1e2022")
   ;;;; helm
   (helm-selection :inherit 'bold
                   :background selection
                   :distant-foreground bg
                   :extend t)
   ;;;; markdown-mode
   (markdown-markup-face :foreground base5)
   (markdown-header-face :inherit 'bold :foreground red)
   (markdown-url-face    :foreground teal :weight 'normal)
   (markdown-reference-face :foreground base6)
   ((markdown-bold-face &override)   :foreground fg)
   ((markdown-italic-face &override) :foreground fg-alt)
   ;;;; rainbow-delimiters <built-in>
   ((rainbow-delimiters-depth-1-face &override) :foreground cyan)
   ((rainbow-delimiters-depth-2-face &override) :foreground yellow)
   ((rainbow-delimiters-depth-3-face &override) :foreground blue)
   ((rainbow-delimiters-depth-4-face &override) :foreground violet)
   ((rainbow-delimiters-depth-5-face &override) :foreground green)
   ((rainbow-delimiters-depth-6-face &override) :foreground yellow)
   ((rainbow-delimiters-depth-7-face &override) :foreground blue)
   ((rainbow-delimiters-depth-8-face &override) :foreground violet)
   ((rainbow-delimiters-depth-9-face &override) :foreground green)
   ;;;; outline <built-in>
   ((outline-1 &override) :foreground blue :height 1.3)
   ((outline-2 &override) :foreground green :height 1.2)
   ((outline-3 &override) :foreground teal :height 1.15)
   ((outline-4 &override) :foreground (doom-darken blue 0.2) :height 1.1)
   ((outline-5 &override) :foreground (doom-darken green 0.2))
   ((outline-6 &override) :foreground (doom-darken teal 0.2))
   ((outline-7 &override) :foreground (doom-darken blue 0.4))
   ((outline-8 &override) :foreground (doom-darken green 0.4))
   ;;;; org <built-in>
   ((org-block &override) :background base0)
   ((org-block-begin-line &override) :foreground comments :background base0)

   ((org-code &override) :foreground "#586e75")
   ((org-meta-line &override) :foreground "#586e75" :slant 'italic)
   ((org-document-info-keyword &override) :inherit 'shadow :height 1.3)
   ((org-document-title &override) :weight 'bold :foreground "#268bd2" :height 1.3)


   ;;;; all extra overrides
  ((font-lock-builtin-face &override) :foreground "#6c71c4")
  ((font-lock-preprocessor-face &override)
   :foreground "#268bd2" :weight 'normal)
  ((custom-face-tag &override)
   :foreground "#6c71c4" :weight 'normal :height 1.2)
  ((custom-group-subtitle &override)
   :foreground "#b58900" :weight 'bold)
  ((custom-group-tag &override)
   :inherit 'variable-pitch :foreground "#268bd2" :height 1.2)
  ((custom-variable-tag &override)
   :inherit 'variable-pitch :foreground "#2aa198" :height 1.2)
  ((highlight-numbers-number &override) :foreground "#6c71c4" :weight 'normal)
  ((header-line &override) :inherit 'default)
  ((magit-diff-removed-highlight &override) :extend t :background "#2E2A2D" :foreground "#dc322f" :weight 'bold)

   ;;;; solaire-mode
   (solaire-mode-line-face
    :inherit 'mode-line
    :background modeline-bg-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-alt)))
   (solaire-mode-line-inactive-face
    :inherit 'mode-line-inactive
    :background modeline-bg-inactive-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive-alt))))

  ;;;; Base theme variable overrides-
  ;; ()
  )

;;; doom-cwills-solarized-dark-theme.el ends here
