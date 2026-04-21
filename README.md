# cmux.el

An Emacs plugin for sending file references to an AI CLI (Claude Code / Gemini CLI) running in another [cmux](https://github.com/manaflow-ai/cmux) surface.

`cmux.el` is an Emacs Lisp port of [tanabee/cmux.vim](https://github.com/tanabee/cmux.vim), with stricter input validation and no shell invocation.

## Features

- Send the current buffer's path as an `@mention` reference (`@src/main.ts `).
- Send the path with the current line (`@src/main.ts:L42 `).
- Send the path with the selected line range (`@src/main.ts:L42-55 `).
- Auto-detect the surface running Claude Code or Gemini CLI.
- Optional follow-up Enter key and target-pane focus after sending.
- Strict surface-ID validation; refuses TRAMP buffers.

## Installation

Requires Emacs 27.1 or newer. No external Emacs Lisp dependencies.

### use-package + MELPA (once published)

```elisp
(use-package cmux
  :ensure t
  :commands (cmux-send-file cmux-send-pos cmux-send-range
             cmux-dwim cmux-detect cmux-set-surface)
  :bind (:map prog-mode-map
         ("C-c m f" . cmux-send-file)
         ("C-c m p" . cmux-send-pos)
         ("C-c m r" . cmux-send-range)
         ("C-c m d" . cmux-detect)
         ("C-c m s" . cmux-set-surface)))
```

### straight.el

```elisp
(straight-use-package
 '(cmux :type git :host github :repo "kexi/cmux.emacs"))
```

### Manual

```bash
git clone https://github.com/kexi/cmux.emacs.git ~/.emacs.d/site-lisp/cmux.emacs
```

```elisp
(add-to-list 'load-path "~/.emacs.d/site-lisp/cmux.emacs")
(require 'cmux)
```

## Usage

Open two cmux tabs: one with Emacs, one with your AI CLI (Claude Code or Gemini CLI). On the first call, `cmux.el` will run `cmux-detect` automatically to discover the AI CLI's surface. You can also set it explicitly with `M-x cmux-set-surface`.

### Commands

| Command | Purpose | Example output |
|---------|---------|----------------|
| `M-x cmux-send-file` | Send current buffer's path | `@src/main.ts ` |
| `M-x cmux-send-pos` | Send path + current line | `@src/main.ts:L42 ` |
| `M-x cmux-send-range` | Send path + selected line range (requires active region) | `@src/main.ts:L42-55 ` |
| `M-x cmux-dwim` | Send range if region is active, otherwise send file | |
| `M-x cmux-detect` | Re-scan surfaces for Claude Code / Gemini CLI | |
| `M-x cmux-set-surface` | Manually set the target surface | |

> **Note on `cmux-detect` false positives:** the built-in patterns are simple
> word-level regexps (e.g. `anthropic`, `Gemini`), so any surface whose screen
> happens to mention those words — a shell scrollback, a man page, an open
> editor buffer — can be matched even when no AI CLI is actually running
> there. If detection picks the wrong surface, override it explicitly with
> `M-x cmux-set-surface` (e.g. `surface:1`).

### Key bindings

`cmux.el` does **not** bind any keys by default. This is intentional: the original Vim plugin's `C-\` clashes with Emacs's built-in `toggle-input-method`, and every convenient Emacs prefix is already taken by something else. Pick bindings that fit your own layout.

A suggested layout under `C-c m`:

```elisp
(global-set-key (kbd "C-c m f") #'cmux-send-file)
(global-set-key (kbd "C-c m p") #'cmux-send-pos)
(global-set-key (kbd "C-c m r") #'cmux-send-range)
(global-set-key (kbd "C-c m d") #'cmux-detect)
(global-set-key (kbd "C-c m s") #'cmux-set-surface)
```

Or use the buffer-local minor mode via its keymap:

```elisp
(define-key cmux-mode-map (kbd "C-c m f") #'cmux-send-file)
(define-key cmux-mode-map (kbd "C-c m r") #'cmux-send-range)
(global-cmux-mode 1)
```

### Evil mode

```elisp
(with-eval-after-load 'evil
  (evil-define-key 'normal 'global (kbd "SPC c f") #'cmux-send-file)
  (evil-define-key 'normal 'global (kbd "SPC c p") #'cmux-send-pos)
  (evil-define-key 'visual 'global (kbd "SPC c r") #'cmux-send-range)
  (evil-define-key 'normal 'global (kbd "SPC c d") #'cmux-detect)
  (evil-define-key 'normal 'global (kbd "SPC c s") #'cmux-set-surface))
```

## Configuration

All options live in the `cmux` customization group (`M-x customize-group RET cmux RET`).

| Option | Default | Description |
|--------|---------|-------------|
| `cmux-executable` | `"cmux"` | Name or absolute path of the cmux CLI. |
| `cmux-surface` | `nil` | Current target surface (e.g. `"surface:1"`). |
| `cmux-cli-name` | `nil` | Name of the detected AI CLI (read-only, filled by `cmux-detect`). |
| `cmux-auto-focus` | `t` | Focus the target pane after sending. |
| `cmux-auto-enter` | `nil` | Send the Enter key after the reference is delivered. |
| `cmux-read-screen-lines` | `30` | How many screen lines to inspect during detection. |
| `cmux-detect-patterns` | see source | Alist of `(CLI-NAME . REGEXP)` used for detection. |
| `cmux-path-style` | `'git-root` | `'git-root`, `'absolute`, or `'relative-home`. |
| `cmux-path-style-fallback` | `'absolute-with-warning` | Fallback when `'git-root` is unavailable: `'absolute-with-warning`, `'error`, or `'relative-home`. |
| `cmux-surface-env-trust` | `t` | Honor the `CMUX_SURFACE_ID` environment variable when valid. |
| `cmux-debug` | `nil` | Write diagnostics to the `*cmux-log*` buffer. |
| `cmux-call-timeout` | `3` | Per-invocation timeout (seconds) for `cmux` calls. |
| `cmux-log-max-lines` | `1000` | Cap on the `*cmux-log*` buffer. |

## Prerequisites

- [cmux](https://github.com/manaflow-ai/cmux) must be installed and on `PATH`, and Emacs must be running inside a cmux surface.
- An AI CLI (Claude Code or Gemini CLI) must be running in another cmux surface.

## Security Notes

- All cmux invocations use `call-process` with list-form arguments and an absolute executable path resolved via `executable-find`. No shell interpolation happens at any stage.
- TRAMP buffers are refused: any buffer whose file or `default-directory` is remote will raise a `user-error`.
- Surface references are validated against a strict regexp (`surface:<digits>`), trimmed, length-limited, and NUL-rejected before being passed to the CLI. This applies to user input, `CMUX_SURFACE_ID`, and the output of `cmux identify` alike.
- Each `cmux` invocation is bounded by `cmux-call-timeout`.
- File paths containing NUL or control characters are rejected.

## Credits

- Original Vim implementation: [tanabee/cmux.vim](https://github.com/tanabee/cmux.vim).
- cmux itself: [manaflow-ai/cmux](https://github.com/manaflow-ai/cmux).

## License

MIT. See [LICENSE](./LICENSE).
