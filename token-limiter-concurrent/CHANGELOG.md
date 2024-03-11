# Changelog

## [0.1.0.0] - 2024-03-11

### Changed

* `waitDebit` now returns how long it waited, if it did.
* `tryDebit` no longer blocks if another `waitDebit` is already happening.

### Removed

* `canDebit`

