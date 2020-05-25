# Releases

## pandoc-utils 0.7.1 (2020-5-25)

- Remove the deprecation notice of `applyFilters`.

## pandoc-utils 0.7.0 (2020-5-24)

- Rename `seqFilters` to `sequenceFilters` to avoid conflict with `seq`.

## pandoc-utils 0.6.1 (2020-5-24)

- Fix the reference to `applyFilters` in readme.

## pandoc-utils 0.6.0 (2020-5-24)

- Rename `applyFilters` to `seqFilters` to avoid name conflicts with Pandoc.
- `applyFilters` is now deprecated.

## pandoc-utils 0.5.1 (2020-5-24)

- Fix the link to Hakyll in package description.

## pandoc-utils 0.5.0 (2020-5-24)

- Reorganize documentations and API.
- Add new functions `convertFilter` and `convertFilterM` to convert between
  filter functions.
- Enable implicit conversion in `getConcatedFilter` and `getConcatedFilterM`.
- Add a module exporting all the utility functions provided by the package.

## pandoc-utils 0.4.0 (2020-5-24)

- Initial public release.
