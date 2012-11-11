module Framework.Template where

import Text.Blaze.Internal (Html)

class Renderable a where
  render :: a -> Html
