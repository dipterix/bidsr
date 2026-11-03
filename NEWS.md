# bidsr 0.1.2

* Updates for compatibility with `S7` > 0.2.2.
* Tabular subclasses now construct their parent class first, so a parent's `content_setter` also runs.
* Assigning new `content` to a tabular object now keeps its column descriptors and is much faster on wide tables.

# bidsr 0.1.1

* Fixed `query_bids` with new argument `env` to capture the current parsing context
* Addressed `CRAN` check issue
* Added `rhub` checks

# bidsr 0.1.0

* Initial CRAN submission.
* Implemented "milestone 1" (see `Roadmap.md`)
