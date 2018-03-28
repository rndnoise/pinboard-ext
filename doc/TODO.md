
chrome: addons
firefox: addons { extensions, themes, ...}
opera: extension
safari?

## bugs

- when permissions wrong, ajax exception bubbles to top(!!)
- prevent concurrent writes
- don't save if nothing has changed
- multi status text moves on save (firefox)
- recheck when url changes on single tab
- when tag buffer focused, clicking save does not first 'chooseBuffer'

## search

- pinboard.in only matches complete words
- site:
- title:
- tag:
- text:
- body:

## new features

- remember new tags
- cmd-enter or ?-enter in textarea to save
- allow re-click 'save' to retry multi on error
- use selected text as description
- option to sort tags by name or frequency
- option to enable/disable close on save
- hot key to activate popup
  - ^B to open popup window
  - ^B+Shift to bookmark without any prompts
- suggest tags
- right-click text in browser to add
- right-click LINK in browser to add
- omnibox search https://github.com/mono0x/pinboard-search

## yak shaving

- build/deploy scripts
- concatenate and minify css during build
- minify javascript during build
- github primer css
  - tags: label, label outline orange
  - form validation
  - buttons: btn-blue
  - tabnav / tabnav-extra / subnav

## completed

- move 'save' buttons to top of popup window
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
