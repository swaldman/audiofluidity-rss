# audiofluidity-rss

A clear, simple Scala API for generating RSS, for general websites as well as for podcasts.

Support is present for most common RSS extensions, including all Apple Podcast "itunes" elements.

## Quickstart

Coming soon!

## Limitations

Although this library defines an informal AST for RSS, for now it only
generates XML, it does not consume and parse it back. 

Maybe someday if there's interest.

## License

_audiofluidity-rss_ was revised from a library internal to [_audiofluidity_](https://github.com/swaldman/audiofluidity),
a podcast-specific static-site generator.

However, this library is now offered independently, under Apache 2.0 terms. Please see
[LICENSE](LICENSE).

(The main _audiofluidity_ application is a GPLv3 project.)


## Some useful RSS resources

More docs soon, I hope. But for now, I want to
bookmark some useful RSS resources:

- [Really Simple Syndication Best Practices Profile](https://www.rssboard.org/rss-profile)
- [RSS 2.0.1 Specification](https://www.rssboard.org/rss-2-0-1)
- [RSS Mime Type](https://www.rssboard.org/rss-mime-type-application.txt)
- [RSS Validator](https://www.rssboard.org/rss-validator/)
- [Excerpt from O'Reilly book by Ben Hammersley](https://www.oreilly.com/library/view/developing-feeds-with/0596008813/ch04s02.html)
- [DublinCore (`dc`) specification](https://www.dublincore.org/specifications/dublin-core/dcmi-terms/)
- [Podcast RSS resources at audiofluidity main](https://github.com/swaldman/audiofluidity#podcast-rss)
- [Atom specification](https://datatracker.ietf.org/doc/html/rfc4287)

See also the podcast-centric RSS resource list in the main [_audiofluidity_ README.md](https://github.com/swaldman/audiofluidity#developer-resources) 