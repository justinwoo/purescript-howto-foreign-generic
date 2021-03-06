# purescript-howto-foreign-generic [![Build Status](https://travis-ci.org/justinwoo/purescript-howto-foreign-generic.svg?branch=master)](https://travis-ci.org/justinwoo/purescript-howto-foreign-generic)

See the blog post about this here: http://qiita.com/kimagure/items/00f97c7fc6cef178fa3c

## Note!!! Do you need to just get some simple JSON parsed? Give [Simple-JSON](https://github.com/justinwoo/purescript-simple-json) a try!

## Important: Changes from Video and Slides

The lesson/slides were made with the previous version of foreign-generics. Here's a somewhat quick rundown of what's changed:

| Old name | New name |
|----------|----------|
| IsForeign | Decode |
| AsForeign | Encode |
| write | encode |
| read | decode |
| readJSON | decodeJSON |
| readGeneric | genericDecode |
| toForeignGeneric | genericEncode |

This repo presents many common usages for [Purescript-Foreign-Generic](https://github.com/paf31/purescript-foreign-generic).

If you're just getting started, you might be interested in [this repo](https://github.com/justinwoo/simple-record-foreign-generic-demo/blob/master/README.md) instead.

[Egghead lesson for basic introduction to Purescript-Foreign-Generic here
![](http://i.imgur.com/uf6SH0H.png)
](https://egghead.io/lessons/automatically-de-serialize-json-with-purescript-foreign-generics)

[See the slides about this repo and purescript-foreign-generics here!
![](http://i.imgur.com/LGgtQKb.png)
](https://speakerdeck.com/justinwoo/serialization-with-purescript)
