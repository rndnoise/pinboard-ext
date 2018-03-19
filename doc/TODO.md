
firefox: addons
opera: extension

## bug fixes

- prevent concurrent writes
- focus tag input on open
- remove #anchor from URLs
- don't save if nothing has changed
- multi status text moves on save (firefox)
- handle missing faviconUrl (opera)

## new features

- remember new tags
- close popup after saved
- suggest tags
- use selected text as description

- configuration
  - user token
  - download tag set
  - default private/public
  - default toRead

## completed

- chrome support
- firefox support
- bookmark all tabs at once
- delete saved bookmarks
- replace Search monad with WriterT w Seq?
- don't suggest tags already chosen
- emphasize matched letters in autocomplete
