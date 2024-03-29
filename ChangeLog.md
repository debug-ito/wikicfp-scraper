# Revision history for wikicfp-scraper

## 0.1.0.13  -- 2022-11-25

* Confirmed test with ghc-9.2.5.

## 0.1.0.12  -- 2021-03-23

* Confirmed test with `attoparsec-0.14.1`.


## 0.1.0.11  -- 2019-04-21

* Confirmed test with `time-1.9`.


## 0.1.0.10  -- 2019-02-19

* Confirmed test with `scalpel-core-0.6.0`.

## 0.1.0.9  -- 2017-07-21

* Confirmed test with `time-1.8`.


## 0.1.0.8  -- 2017-02-06

* No API change.
* Change dependency on `scalpel` to `scalpel-core`, because the latter
  has lighter dependency.
* Drop dependency upper bound for `hspec`. It's stable enough.


## 0.1.0.7  -- 2017-01-24

* Confirmed test with `hspec-2.4.0`


## 0.1.0.6  -- 2016-11-27

* Confirmed test with `time-1.7`.


## 0.1.0.5  -- 2016-10-17

* Confirmed build with `scalpel-0.4.0`.
* Now this module uses CPP to deal with changes introduced in `scalpel-0.4.0`.


## 0.1.0.4  -- 2016-10-09

* Confirmed build with `hspec-2.3.0`.


## 0.1.0.3  -- 2016-06-20

* Bug fix. Now it returns an empty deadlines (`[]`) for "TBD".


## 0.1.0.2  -- 2016-06-03

* Fix packaging. Now it contains test/data/*.html necessary to run
  tests. See https://github.com/fpco/stackage/pull/1548#issuecomment-223310457


## 0.1.0.1  -- 2016-05-29

* Confirmed build with time-1.6.0.1 and base-4.9.0.0


## 0.1.0.0  -- 2016-05-21

* First version. Released on an unsuspecting world.
