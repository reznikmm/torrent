# torrent

>  BitTorrent protocol implementation in Ada

## Install

Run
```
make all install PREFIX=/path/to/install
```

### Dependencies
It depends on [Matreshka](https://forge.ada-ru.org/matreshka) and AWS libraries.

## Usage

To start server just run `torrent-run <file.torrent>` executable.

To use as a library, add `with "torrent";` to your project file.

## Maintainer

[@MaximReznik](https://github.com/reznikmm).

## Contribute

Feel free to join!
[Open an issue](https://github.com/reznikmm/torrent/issues/new) or submit PRs.

## License

[MIT](LICENSE) © Maxim Reznik
