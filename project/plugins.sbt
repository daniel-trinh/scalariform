resolvers += Classpaths.typesafeReleases

addSbtPlugin("com.github.retronym" % "sbt-onejar" % "0.8")

resolvers += Classpaths.typesafeSnapshots

addSbtPlugin("com.typesafe.sbteclipse" % "sbteclipse-plugin" % "2.4.0")

addSbtPlugin("com.typesafe.sbt" % "sbt-scalariform" % "1.3.0")

addSbtPlugin("com.typesafe.sbt" % "sbt-pgp" % "0.8.2")

addSbtPlugin("com.eed3si9n" % "sbt-buildinfo" % "0.3.1")

retrieveManaged := true