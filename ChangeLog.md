# Revision history for st2

## 0.1.0.2 -- 2018-11-02

* Fix build on GHC-8.6, which requires Control.Monad.ST2 to have
  -XPolyKinds enabled in order to compile.

## 0.1.0.1 -- 2018-11-02

* Fix internal bug that could cause semantic differences under optimisations,
  related to implementation of runRegion#.

## 0.1.0.0 -- 2018-07-23

* First version. Released on an unsuspecting world.
