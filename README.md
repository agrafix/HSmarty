# HSmarty

[![Build Status](https://drone.io/github.com/agrafix/HSmarty/status.png)](https://drone.io/github.com/agrafix/HSmarty/latest)

Haskell implementation of a (currently) very small subset of the [PHP-Smarty][1] template language.

## Usage
```haskell
{-# LANGUAGE OverloadedStrings #-}

import Text.HSmarty

import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T

main =
   renderTemplate "test.tpl" $ HM.fromList [ ( "title", mkParam ("SomeTitle" :: T.Text))
                                           , ( "list", mkParam (["a", "b"] :: [T.Text]))
                                           ]
```

## Implemented features

* Basic template parsing (See [Basic Syntax][2])
* Smarty comments
* Basic expressions (eg. `$var`, `$var.mapItem`, `$var[3]`, `$var@property`, `3+4`, ..)
* Branching (eg. `{if ..}`, `{elseif ..}`, `{else}`, ..)
* Looping with properties (eg. `{foreach $el as $k=>$v}`, `{$v@last}`, `{foreachelse}`, `{/foreach}`)
* Including other templates (`{include file='other.tpl'}`)

[1]: http://www.smarty.net/
[2]: http://www.smarty.net/docs/en/language.basic.syntax.tpl
