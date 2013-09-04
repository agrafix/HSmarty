# HSmarty

Haskell implementation of a (currently) very small subset of the [PHP-Smarty][1] template language.

## Usage
`renderTemplate "test.tpl" $ HM.fromList [ ( T.pack "title", mkParam "SomeTitle"), (T.pack "list", mkParam ["a", "b"]) ]`

## Implemented features

* Basic template parsing (See [Basic Syntax][2])
* Smarty comments
* Basic expressions (eg. `$var`, `$var.mapItem`, `$var[3]`, `$var@property`, `3+4`, ..)
* Branching (eg. `{if ..}`, `{elseif ..}`, `{else}`, ..)
* Looping with properties (eg. `{foreach $el as $k=>$v}`, `{$v@last}`, `{foreachelse}`, `{/foreach}`)
* Including other templates (`{include file='other.tpl'}`)

[1]: http://www.smarty.net/
[2]: http://www.smarty.net/docs/en/language.basic.syntax.tpl
