---
title:   `Init.el`: The Second Coming of Emacs
author:  Nathan DeGruchy
date:    2014-10-24
---

# Emacs Configuration
  Yep, I redid my Emacs configuration, because I wasn't happy that the
  configuration was so incredibly difficult to get running on non-*nix
  platforms, like my Windows Workstation.

  Thus, the new Emacs config was born. Basically the same, but flatter
  and with less dependencies.

## Cleanup

   I've gone ahead and split up the `init` to be easier to home in on
   the specific option I want to modify. This is not perfect, as some
   packages will still have options and keybindings loaded inside
   their `use-package` directives. While I could make it completely
   separate, I've opted for this approach for now to make it
   moderately easier for me to manage.
