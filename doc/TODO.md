
chrome?
safari?
firefox: addons
opera: extension

## bugs

- prevent concurrent writes
- don't save if nothing has changed
- multi status text moves on save (firefox)
? remove #anchor from URLs

## new features

- remember new tags
- use selected text as description
- option to sort tags by name or frequency
- option to enable/disable close on save
- hot key to activate popup
- suggest tags

## completed

- close popup after saved
- focus tag input on open
- configuration
  - user token
  - download tag set
  - default private/public
  - default toRead
- chrome support
- firefox support
- bookmark all tabs at once
- delete saved bookmarks
- replace Search monad with WriterT w Seq?
- don't suggest tags already chosen
- emphasize matched letters in autocomplete
- handle missing faviconUrl (opera)
- exclude unsupported url schemes
