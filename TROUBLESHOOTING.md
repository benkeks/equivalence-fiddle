# Troubleshooting Guide for Scala/Scala.js Migration

## Common Issues and Solutions

### Dependency Resolution Issues

#### Problem: sbt cannot resolve dependencies
If you see errors like:
```
Error downloading com.github.sbt:sbt-web
```

**Solution**: Ensure you have network access to:
- https://repo1.maven.org/maven2/ (Maven Central)
- https://repo.typesafe.com/ (Typesafe repository)

Add a `~/.sbt/repositories` file if you're behind a corporate proxy:
```
[repositories]
local
maven-central
```

### Compilation Errors

#### Problem: "object raw is not a member of package org.scalajs.dom"

This occurs if any old imports remain. Update:
```scala
import org.scalajs.dom.raw.HTMLElement
```
to:
```scala
import org.scalajs.dom.HTMLElement
```

#### Problem: Implicit conversion not found

In Scala.js 1.x, many conversions that required explicit imports are now automatic. Remove imports like:
```scala
import scala.scalajs.js.Any.fromFunction1
import scala.scalajs.js.Any.jsArrayOps
```

Simply importing `scala.scalajs.js` is usually sufficient.

#### Problem: "value jsDependencies is not a member of sbt.Project"

The `jsDependencies` setting has been removed in Scala.js 1.x. JavaScript dependencies should be:
1. Managed via npm and bundled with webpack, OR
2. Included directly in the web project's resources

We've already removed `jsDependencies` from `build.sbt`.

### Scala 2.13 Specific Issues

#### Problem: Collection operations behave differently

Scala 2.13 completely redesigned the collections library. Most code works without changes, but:

1. **Lazy collections**: Use `.view` explicitly for lazy operations
   ```scala
   // Old (2.12)
   list.filter(x => x > 0).map(x => x * 2)
   
   // Optimized (2.13) - same as above, but can use .view for lazy ops
   list.view.filter(x => x > 0).map(x => x * 2).toList
   ```

2. **Breaking collections**: Some operations have changed signatures
   ```scala
   // If you see: "type mismatch; found: Iterator[A], required: B"
   // Add .toSeq, .toList, or similar to convert
   ```

#### Problem: "procedure syntax is deprecated"

Update procedures to use explicit return types:
```scala
// Old (deprecated)
def doSomething() {
  // ...
}

// New (recommended)
def doSomething(): Unit = {
  // ...
}
```

### Testing Issues

#### Problem: Tests fail with "NoClassDefFoundError"

Ensure you're using compatible test library versions:
- scalatest 3.2.19
- scalactic 3.2.19

#### Problem: JavaScript tests fail

If JavaScript-specific tests fail, ensure:
1. The Scala.js linker is using the correct module kind
2. DOM facades are imported correctly
3. Test infrastructure supports Scala.js 1.x

### Build Performance

#### Problem: Compilation is slow

Scala 2.13 and Scala.js 1.x may have different performance characteristics:

1. **Increase memory**: Update `.sbtopts`
   ```
   -J-Xmx16G
   -J-XX:+UseG1GC
   ```

2. **Use fast optimization during development**
   ```bash
   sbt fastOptJS  # instead of fullOptJS
   ```

3. **Enable parallel execution** (already set in the project)

### IDE Issues

#### Problem: IntelliJ IDEA shows red underlines

1. Reload the sbt project: File → Reload All from Disk
2. Invalidate caches: File → Invalidate Caches / Restart
3. Ensure Scala plugin is up to date
4. Check that the IDE is using the correct Scala version (2.13.18)

#### Problem: VS Code with Metals doesn't work

1. Clean build: `sbt clean`
2. Import build: Run "Metals: Import build" command
3. Check Metals logs for errors
4. Ensure Java 17 is configured

## Verification Steps

After making changes, verify everything works:

### 1. Clean Build
```bash
sbt clean
```

### 2. Compile All Projects
```bash
sbt compile
```

### 3. Run Tests
```bash
sbt shared/test
```

### 4. Build Web Client
```bash
sbt webStage
```

### 5. Build JS API
```bash
sbt jsApi/fullOptJS
```

### 6. Manual Testing
Open `web/target/web/stage/index.html` in a browser and verify:
- Page loads without errors
- Code editor works
- Examples can be loaded
- Comparisons can be performed
- Results are displayed correctly

## Getting Help

If you encounter issues not covered here:

1. Check the official migration guides:
   - [Scala 2.13 Migration](https://docs.scala-lang.org/overviews/core/collections-migration-213.html)
   - [Scala.js 1.x Migration](https://www.scala-js.org/doc/project/migration.html)

2. Search for similar issues:
   - [Scala.js GitHub Issues](https://github.com/scala-js/scala-js/issues)
   - [Stack Overflow](https://stackoverflow.com/questions/tagged/scala.js)

3. Review the changes in this PR:
   - See `MIGRATION_NOTES.md` for a complete list of changes
   - Check git history for specific change details

## Rollback Procedure

If critical issues arise, you can rollback:

```bash
# Revert to previous versions in build files
git revert <this-pr-commit>

# Or manually update version numbers back to:
# - Scala: 2.12.13
# - Scala.js: 0.6.33
# - sbt: 1.8.2
```

Note: After rollback, you'll also need to:
1. Rename `scala/` directories back to `scala-2.12/`
2. Revert import changes (add `.raw` back, etc.)
3. Re-add `jsDependencies` configuration
