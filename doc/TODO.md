
chrome?
safari?
firefox: addons
opera: extension

## bugs

- prevent concurrent writes
- focus tag input on open
- don't save if nothing has changed
- multi status text moves on save (firefox)
? remove #anchor from URLs

## new features

- remember new tags
- close popup after saved
- suggest tags
- use selected text as description

## completed

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
