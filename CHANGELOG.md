# Changelog

Notable changes to this project are documented in this file. The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/) and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

Breaking changes:

New features:

Bugfixes:

Other improvements:

## [v4.0.1](https://github.com/purescript/purescript-minibench/releases/tag/v4.0.1) - 2022-09-23

Other improvements:
- Update FFI to use a more direct and portable convention (#25 by @natefaubion)

## [v4.0.0](https://github.com/purescript/purescript-minibench/releases/tag/v4.0.0) - 2022-04-27

Breaking changes:
- Migrate FFI to ES modules (#22 by @JordanMartinez)

New features:

Bugfixes:

Other improvements:
- Drop `math` dependency; update imports (#23 by @JordanMartinez)

## [v3.0.0](https://github.com/purescript/purescript-minibench/releases/tag/v3.0.0) - 2021-02-26

Breaking changes:
- Added support for PureScript 0.14 and dropped support for all previous versions (#14)
  This change includes dropping the deprecated `globals` dependency and switching to the same functions now exported from `numbers`.

New features:

Bugfixes:

Other improvements:
- Migrated CI to GitHub Actions and updated installation instructions to use Spago (#15)
- Added a changelog and pull request template (#18)

## [v2.0.0](https://github.com/purescript/purescript-minibench/releases/tag/v2.0.0) - 2018-05-23

- Updated for PureScript 0.12

## [v1.0.1](https://github.com/purescript/purescript-minibench/releases/tag/v1.0.1) - 2017-11-24

- Now runs garbage collection before benchmarking. Use the `--force-gc` flag in Node to see the effect of this. (@matthewleon)

## [v1.0.0](https://github.com/purescript/purescript-minibench/releases/tag/v1.0.0) - 2017-06-05

- Initial versioned release
