## sbt project compiled with Scala 3

### Usage

This is a normal sbt project. You can compile code with `sbt compile`, run it with `sbt run`, and `sbt console` will start a Scala 3 REPL.

For more information on the sbt-dotty plugin, see the
[dotty-example-project](https://github.com/lampepfl/dotty-example-project/blob/master/README.md).


# Higher-kinded data

What is HKD?

Also, wanted to try out upcoming Scala 3.

Before diving in, acknowledge community. 
Originally saw concept in Oleg's talk and playing around and searching found 

## Sources

- [Oleg Nizhnikov's talk](https://www.youtube.com/watch?v=k8IgRayL4vo)
  - [Source](https://github.com/Odomontois/data-lab)
- [Chris Penner's talk](https://www.youtube.com/watch?v=sIqZEmnFer8)
- [Philipp Martini's blog post](https://blog.philipp-martini.de/blog/magic-mirror-scala3/)
- [Kat's blog post](https://katrix.github.io/scala/hkd/2019/12/18/intro-to-hkd-in-scala.html)
  - [Source](https://github.com/Katrix/perspective)
- Michael Thomas' two repos:
  - [Higher-kinded data](https://github.com/Michaelt293/higher-kinded-data)
  - [Higher-kinded aggregations](https://github.com/Michaelt293/higher-kinded-aggregations)

## Example

Templating service.
Template model. Start with basic.
JSON encoding/decoding
Then move to HKD with Option for PATCH
Now can reuse HKD for validation
Generate SQL? <- if do not use Skunk. Perhaps will come in a follow-up
Use Skunk's Channel to subscribe to events... NAHHH, we can just append to a queue
Stream these revisions to client with websockets.
Could even stream updates from client back to server for certain fields?

Nested validations?

- pretty long journey. hard to wrap my head around. still barely scratching the surface. (representable, distributable, query generation, testing with lists, validation with lenses (do not want to validate every single field so use lenses but also do not want to miss out if new field is added and SHOULD be validated...have a set of unvalidated fields somewhere which is the complement to the set of validated fields which unioned should equal all the fields))
