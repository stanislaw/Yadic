# Yadic

Yadic is a command-line tool for online translation powered by [Yandex.Dictionary](https://tech.yandex.com/dictionary/).

__July 15, 2015__ Yadic's 0.1 version was written in two days as my exercise in learning Haskell. I know that true Haskell hackers would hate the way I wrote it (imperative, dirty etc) so I hope to eventually improve it a lot. Meanwhile I would highly appreciate having advice from experienced Haskell programmers.

## Usage

```bash
$ ./yadic de-en kamille
camomile (noun)
$ ./yadic en-ru camomile
ромашка (существительное)
ромашковый (прилагательное)
$ ./yadic ru-tr ромашка
papatya (isim)
$ ./yadic tr-de papatya
Kamille (noun)
```

## Installation

Only OS X was tested so far. `ghc` compiler is needed.

Clone it, build it yourself:

```bash
git clone https://github.com/stanislaw/Yadic
cd Yadic
make # Results in 'yadic' binary in dist/build/yadic
```

Place `yadic` binary somewhere to `/usr/local/bin`.

## Configuration

1) API Key is needed. Yandex.Dictionary API methods are accessed using a key so you need to obtain one: [Dictionary API](https://tech.yandex.com/dictionary/).

2) Create `~/.yadic` file with the following contents:

```yaml
lang: en-en # Default pair of languages to translate (from-to)
apikey: dict.1.1.20150711T192659Z... # your Yandex Dictionary API key
```

## Credits

Yadic is entirely based on [Yandex.Dictionary API](https://tech.yandex.com/dictionary/) free service so most of the credit goes to their wonderful machinery. That is great that they made it available for free.

## TODO

- Learn Haskell to remove imperative non-idiomatic code
- Modularize Yadic, write better functions
- Add option to make output very verbose to support all the functionality Yandex.Dictionary [offers](https://tech.yandex.com/dictionary/doc/dg/reference/lookup-docpage/). But first [Haskell: multiple declarations of x](http://stackoverflow.com/questions/24352280/haskell-multiple-declarations-of-x) is somehow to be resolved.
  
- Add support for Yandex.Translation so that `yadic word` would go to Dictionary and `yadic "phrase"` would to to Translate.

## License

MIT + [Terms of Use of API Yandex.Dictionary Service](https://legal.yandex.com/dictionary_api/). See LICENSE for details.

## Copyright

Copyright (c) 2015, Stanislaw Pankevich. See LICENSE for details.

