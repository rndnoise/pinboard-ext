
chrome: addons
firefox: addons { extensions, themes, ...}
opera: extension
safari?

## bugs

- paste closes popup in Chrome?
- when manifest permissions wrong, ajax exception bubbles to top(!!)
- prevent concurrent writes (rapid click Save)
- don't save if nothing has changed
- multi status text moves on save (firefox)
- recheck when url changes on single tab

## search

- omnibox search https://github.com/mono0x/pinboard-search
- pinboard.in only matches complete words
- site:
- title:
- tag:
- text:
- body:

## new features

- bump old bookmark to top of queue
- give bookmarks a rating (separate ratings for interest to read
  vs have read)
- do something about anchors index.html#abc... pinboard treats
  this separate from index.html so user might not be aware of
  existing bookmark
- remember new tags
- use selected text as description
- cmd-enter or ?-enter in textarea to save
- allow re-click 'save' to retry multi on error
- option to sort tags by name or frequency
- option to enable/disable close on save
- hot key to activate popup
  - ^B to open popup window
  - ^B+Shift to bookmark without any prompts
- suggest tags
- right-click text in browser to add
- right-click LINK in browser to add, use link text as title?

## yak shaving

- concatenate and minify css during build
- minify javascript during build
- github primer css
  - tags: label, label outline orange
  - form validation
  - buttons: btn-blue
  - tabnav / tabnav-extra / subnav

## completed

- save by only open popup, press [Enter]
- focus tag input on open
- left/right arrow keys don't work in text entry for tags
- when tag buffer focused, clicking save does not first 'chooseBuffer'
- clicking suggestions chooses buffer, not clicked item -- focus issue??
- move 'save' buttons to top of popup window
- close popup after saved
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
- move tag input to the top
- bug: when tab is closed, popup remains open
