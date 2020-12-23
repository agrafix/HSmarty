# HSmarty

Haskell implementation of a subset of the [PHP-Smarty][1] template language.

## Usage
```haskell
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Text.HSmarty

import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T

main :: IO ()
main =
   renderTemplate "test.tpl" $
   HM.fromList [ ( "title", mkParam ("SomeTitle" :: T.Text))
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
* Defining functions (`{function name='foo'}...{/function}`)
* Calling functions (`{foo arg1="bar"}`)
* Assigning variables (`{$foo=1+2+3}`)
* Capturing output into variables (`{capture name='blabla'}<b>{$some}</b>{/capture}`)

### Non-Smarty features

* Explicit scoping blocks (`{scope}{$localFoo=123}{$localFoo}{/scope}{* $localFoo not available here! *}`)

## Contributing

All constributions adding features of the "original" PHP-Smarty V3 are welcome. For any other contributions please discuss in an issue first. Note that by sending a PR you agree that all your code can be released under the BSD3 license as part of this project or otherwise.

[1]: http://www.smarty.net/
[2]: http://www.smarty.net/docs/en/language.basic.syntax.tpl
