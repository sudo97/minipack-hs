# minipack-hs

minipack-hs is a Haskell library and executable for managing and transpiling JavaScript. This project is designed to be simple and work as just a demonstration tool of what is bundling and transpiling.

## Tools and libraries used

to quote from `minipack-hs.cabal`:

```cabal
    build-depends:    base ^>=4.19.1.0, language-javascript ^>=0.7.1.0, containers, filepath, directory, mtl, transformers
```

[language-javascript](https://hackage.haskell.org/package/language-javascript) is a most amazing library for working with JavaScript in Haskell. I was able to easily hack my way through.

## Limitations

as of now, some of the import types aren't supported (Those I made are enough to show the concept). As well as default export ([The reason here is different.](https://github.com/erikd/language-javascript/blob/b933adf816eb4ee9208b1962a739c455534e179a/src/Language/JavaScript/Parser/AST.hs#L117)).

## Usage

To use the minipack-hs executable, run:

```sh
cabal run minipack-hs -- <entry_point> > output.js
```

## Differences to minipack-rs

1. It's in a garbage collected language, which made it much easier to tackle
1. language-javascript has no transpillation possibilities, so I had to hack my own transpiler for imports and exports.

## Project Structure

- `lib/`: Contains the Haskell source files for the library.
- `app/`: Contains the Haskell source files for the executable.
- `CHANGELOG.md`: Revision history of the project.
- `LICENSE`: License information for the project.
- `minipack-hs.cabal`: Cabal configuration file for the project.

## License

This project is licensed under the MIT License. See the [LICENSE](LICENSE) file for details.

## Contributing

Contributions are welcome! Please open an issue or submit a pull request on GitHub.

## Authors

- Illia Mikhnevych

## Acknowledgments

- Haskell community for their support and contributions.