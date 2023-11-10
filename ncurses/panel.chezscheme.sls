;; Chez Scheme bindings for ncurses' panel library.
;; Written by Jerry 2023.
;; SPDX-License-Identifier: Unlicense

(library (ncurses panel)
  (export
    new-panel
    bottom-panel
    top-panel
    show-panel
    update-panels
    hide-panel
    panel-window
    replace-panel
    move-panel
    panel-hidden
    panel-above
    panel-below
    del-panel
    #;ground-panel
    #;ceiling-panel)
  (import
    (chezscheme)
    (ncurses common)
    )

  (define load-lib
    (load-shared-object "libpanelw.so.6"))

  (define-ftype panel (struct))

  (c_funcs
    [new-panel (window*) (* panel)]
    [bottom-panel ((* panel)) errok]
    [top-panel ((* panel)) errok]
    [show-panel ((* panel)) errok]
    [update-panels () void]
    [hide-panel ((* panel)) errok]
    [panel-window ((* panel)) window*]
    [replace-panel ((* panel) window*) errok]
    [move-panel ((* panel) int int) errok]
    ;; TODO this should check for ERR before converting to bool.
    [panel-hidden ((* panel)) boolean]
    ;; TODO check for NULL and convert to #f if appropriate.
    [panel-above ((* panel)) (* panel)]
    [panel-below ((* panel)) (* panel)]
    [del-panel ((* panel)) errok]
    #;[ground-panel]
    #;[ceiling-panel])

  )
