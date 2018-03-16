module Pinboard.UI.Internal.Icons where

import Data.Monoid              ((<>))
import Halogen.HTML             (HTML, IProp)
import Halogen.SVG.Indexed      as I
import Halogen.SVG.Elements     (svg, path)
import Halogen.SVG.Attributes   (viewBox, version, fillRule, strokeWidth, strokeLineJoin, d)

single :: forall p i. Array (IProp I.SVG i) -> HTML p i
single attrs = svg (attrs <> [ viewBox 0 0 76 76, version "1.1" ]) [ path [ strokeWidth 0.2, strokeLineJoin "round", d "M 35,19L 41,19L 41,35L 57,35L 57,41L 41,41L 41,57L 35,57L 35,41L 19,41L 19,35L 35,35L 35,19 Z " ] [] ]

multi :: forall p i. Array (IProp I.SVG i) -> HTML p i
multi attrs = svg (attrs <> [ viewBox 0 0 76 76, version "1.1" ]) [ path [ strokeWidth 0.2, strokeLineJoin "round", d "M 39,46L 46,46L 46,39L 51,39L 51,46L 58,46L 58,51L 51,51L 51,58L 46,58L 46,51L 39,51L 39,46 Z M 31,25L 38,25L 38,18L 43,18L 43,25L 50,25L 50,30L 43,30L 43,37L 38,37L 38,30L 31,30L 31,25 Z M 18,39L 25,39L 25,32L 30,32L 30,39L 37,39L 37,44L 30,44L 30,51L 25,51L 25,44L 18,44L 18,39 Z " ] [] ]

bookmark :: forall p i. Array (IProp I.SVG i) -> HTML p i
bookmark attrs = svg (attrs <> [ viewBox 0 0 10 16, version "1.1" ]) [ path [ fillRule "evenodd", d "M9 0H1C.27 0 0 .27 0 1v15l5-3.09L10 16V1c0-.73-.27-1-1-1zm-.78 4.25L6.36 5.61l.72 2.16c.06.22-.02.28-.2.17L5 6.6 3.12 7.94c-.19.11-.25.05-.2-.17l.72-2.16-1.86-1.36c-.17-.16-.14-.23.09-.23l2.3-.03.7-2.16h.25l.7 2.16 2.3.03c.23 0 .27.08.09.23h.01z" ] [] ]

pin :: forall p i. Array (IProp I.SVG i) -> HTML p i
pin attrs = svg (attrs <> [ viewBox 0 0 16 16, version "1.1" ]) [ path [ fillRule "evenodd", d "M10 1.2V2l.5 1L6 6H2.2c-.44 0-.67.53-.34.86L5 10l-4 5 5-4 3.14 3.14a.5.5 0 0 0 .86-.34V10l3-4.5 1 .5h.8c.44 0 .67-.53.34-.86L10.86.86a.5.5 0 0 0-.86.34z" ] [] ]

tag :: forall p i. Array (IProp I.SVG i) -> HTML p i
tag attrs = svg (attrs <> [ viewBox 0 0 14 16, version "1.1" ]) [ path [ fillRule "evenodd", d "M7.73 1.73C7.26 1.26 6.62 1 5.96 1H3.5C2.13 1 1 2.13 1 3.5v2.47c0 .66.27 1.3.73 1.77l6.06 6.06c.39.39 1.02.39 1.41 0l4.59-4.59a.996.996 0 0 0 0-1.41L7.73 1.73zM2.38 7.09c-.31-.3-.47-.7-.47-1.13V3.5c0-.88.72-1.59 1.59-1.59h2.47c.42 0 .83.16 1.13.47l6.14 6.13-4.73 4.73-6.13-6.15zM3.01 3h2v2H3V3h.01z" ] [] ]

list :: forall p i. Array (IProp I.SVG i) -> HTML p i
list attrs = svg (attrs <> [ viewBox 0 0 12 16, version "1.1" ]) [ path [ fillRule "evenodd", d "M2 13c0 .59 0 1-.59 1H.59C0 14 0 13.59 0 13c0-.59 0-1 .59-1h.81c.59 0 .59.41.59 1H2zm2.59-9h6.81c.59 0 .59-.41.59-1 0-.59 0-1-.59-1H4.59C4 2 4 2.41 4 3c0 .59 0 1 .59 1zM1.41 7H.59C0 7 0 7.41 0 8c0 .59 0 1 .59 1h.81c.59 0 .59-.41.59-1 0-.59 0-1-.59-1h.01zm0-5H.59C0 2 0 2.41 0 3c0 .59 0 1 .59 1h.81c.59 0 .59-.41.59-1 0-.59 0-1-.59-1h.01zm10 5H4.59C4 7 4 7.41 4 8c0 .59 0 1 .59 1h6.81c.59 0 .59-.41.59-1 0-.59 0-1-.59-1h.01zm0 5H4.59C4 12 4 12.41 4 13c0 .59 0 1 .59 1h6.81c.59 0 .59-.41.59-1 0-.59 0-1-.59-1h.01z" ] [] ]

diff :: forall p i. Array (IProp I.SVG i) -> HTML p i
diff attrs = svg (attrs <> [ viewBox 0 0 13 16, version "1.1" ]) [ path [ fillRule "evenodd", d "M6 7h2v1H6v2H5V8H3V7h2V5h1v2zm-3 6h5v-1H3v1zM7.5 2L11 5.5V15c0 .55-.45 1-1 1H1c-.55 0-1-.45-1-1V3c0-.55.45-1 1-1h6.5zM10 6L7 3H1v12h9V6zM8.5 0H3v1h5l4 4v8h1V4.5L8.5 0z" ] [] ]

added :: forall p i. Array (IProp I.SVG i) -> HTML p i
added attrs = svg (attrs <> [ viewBox 0 0 14 16, version "1.1" ]) [ path [ fillRule "evenodd", d "M13 1H1c-.55 0-1 .45-1 1v12c0 .55.45 1 1 1h12c.55 0 1-.45 1-1V2c0-.55-.45-1-1-1zm0 13H1V2h12v12zM6 9H3V7h3V4h2v3h3v2H8v3H6V9z" ] [] ]

upload :: forall p i. Array (IProp I.SVG i) -> HTML p i
upload attrs = svg (attrs <> [ viewBox 0 0 16 16, version "1.1" ]) [ path [ fillRule "evenodd", d "M7 9H5l3-3 3 3H9v5H7V9zm5-4c0-.44-.91-3-4.5-3C5.08 2 3 3.92 3 6 1.02 6 0 7.52 0 9c0 1.53 1 3 3 3h3v-1.3H3c-1.62 0-1.7-1.42-1.7-1.7 0-.17.05-1.7 1.7-1.7h1.3V6c0-1.39 1.56-2.7 3.2-2.7 2.55 0 3.13 1.55 3.2 1.8v1.2H12c.81 0 2.7.22 2.7 2.2 0 2.09-2.25 2.2-2.7 2.2h-2V12h2c2.08 0 4-1.16 4-3.5C16 6.06 14.08 5 12 5z" ] [] ]

check :: forall p i. Array (IProp I.SVG i) -> HTML p i
check attrs = svg (attrs <> [ viewBox 0 0 12 16, version "1.1" ]) [ path [ fillRule "evenodd", d "M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z" ] [] ]

issue :: forall p i. Array (IProp I.SVG i) -> HTML p i
issue attrs = svg (attrs <> [ viewBox 0 0 14 16, version "1.1" ]) [ path [ fillRule "evenodd", d "M7 2.3c3.14 0 5.7 2.56 5.7 5.7s-2.56 5.7-5.7 5.7A5.71 5.71 0 0 1 1.3 8c0-3.14 2.56-5.7 5.7-5.7zM7 1C3.14 1 0 4.14 0 8s3.14 7 7 7 7-3.14 7-7-3.14-7-7-7zm1 3H6v5h2V4zm0 6H6v2h2v-2z" ] [] ]
