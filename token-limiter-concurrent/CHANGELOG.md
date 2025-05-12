# Changelog

## [0.2.0.0] - 2025-05-12

### Changed

* Fixed that `waitDebit` would not wait infinitely if tokens will never become available.

## [0.1.0.0] - 2024-03-11

### Changed

* `waitDebit` now returns how long it waited, if it did.
* `tryDebit` no longer blocks if another `waitDebit` is already happening.

### Removed

* `canDebit`

