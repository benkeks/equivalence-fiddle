# Scala/Scala.js Migration Guide

## Overview

This repository has been updated to support migration to the latest stable versions of Scala and Scala.js.

## Changes Made

### Version Updates

| Component | Old Version | New Version |
|-----------|-------------|-------------|
| Scala | 2.12.13 | 2.13.18 |
| Scala.js | 0.6.33 | 1.20.2 |
| sbt | 1.8.2 | 1.10.7 |
| sbt-scalajs | 0.6.33 | 1.20.2 |
| sbt-web-scalajs | 1.1.0-0.6 | 1.2.0 |
| sbt-assembly | 0.14.10 | 2.3.0 |
| scalaz-core | 7.2.29 | 7.3.8 |
| scalatest | 3.2.0 | 3.2.19 |
| scalactic | 3.2.0 | 3.2.19 |

### Source Directory Structure

Renamed source directories from version-specific to cross-version compatible:
- `scala-2.12/` → `scala/`

This allows the code to work with multiple Scala versions without separate source trees.

### Build Configuration Changes

#### build.sbt
- Updated all `scalaVersion` declarations to `2.13.18`
- Updated library dependency versions for compatibility
- Removed `packageJSDependencies / skip` setting (deprecated in Scala.js 1.x)
- Removed `jsDependencies` configuration (deprecated in Scala.js 1.x)
- Updated `unmanagedSourceDirectories` paths from `scala-2.12` to `scala`

#### project/build.properties
- Updated sbt version to 1.10.7

#### project/plugins.sbt
- Updated all plugin versions to latest compatible releases
- Added explicit `sbt-web` plugin (now required as separate dependency)

### Code Changes for Scala.js 1.x Compatibility

#### Import Changes

1. **DOM API Changes**
   ```scala
   // Old (Scala.js 0.6.x)
   import org.scalajs.dom.raw.Event
   import org.scalajs.dom.raw.HTMLElement
   
   // New (Scala.js 1.x)
   import org.scalajs.dom.Event
   import org.scalajs.dom.HTMLElement
   ```
   The `raw` package was removed in Scala.js 1.x.

2. **Removed Deprecated Implicit Conversions**
   ```scala
   // Old (Scala.js 0.6.x) - these imports are no longer needed
   import scala.scalajs.js.Any.fromFunction1
   import scala.scalajs.js.Any.jsArrayOps
   import scala.scalajs.js.Any.wrapArray
   import scala.scalajs.js.UndefOr.any2undefOrA
   import scala.scalajs.js.UndefOr.undefOr2ops
   import scala.scalajs.js.|.from
   
   // New (Scala.js 1.x) - conversions are now implicit in scala.scalajs.js
   import scala.scalajs.js
   ```

3. **Removed jsDependencies**
   - The `jsDependencies` setting has been removed from Scala.js 1.x
   - JavaScript dependencies should now be managed via npm/webpack or included directly in the web project

## Breaking Changes from Scala 2.12 to 2.13

The migration from Scala 2.12 to 2.13 includes several changes:

1. **Collections Library**: Completely redesigned. Most code should work without changes but performance characteristics may differ.

2. **Compiler Flags**: Some flags have changed or been removed. The project uses:
   - `-Xmax-classfile-name 140`
   - `-feature`
   - `-language:implicitConversions`
   - `-language:postfixOps`
   - `-language:existentials`
   - `-deprecation`

3. **Library Compatibility**: All dependencies have been updated to versions that support Scala 2.13.

## Breaking Changes from Scala.js 0.6 to 1.x

1. **Module System**: The default module kind is now ES modules instead of no module
2. **jsDependencies**: Removed in favor of npm-based dependency management
3. **DOM API**: The `raw` package was removed; use `org.scalajs.dom` directly
4. **Implicit Conversions**: Many explicit conversion imports are no longer needed

## Testing the Migration

### Prerequisites
- Java 17 (as specified in CI configuration)
- sbt 1.10.7 or later

### Build Commands

```bash
# Test compilation
sbt shared/test

# Build web client
sbt webStage

# Build JS API
sbt jsApi/fullOptJS
```

### Expected Behavior

All tests should pass and the web client should build successfully. The generated JavaScript should work identically to the previous version from a functional perspective.

## Verification Checklist

- [ ] `sbt shared/test` runs successfully
- [ ] `sbt webStage` completes without errors
- [ ] `sbt jsApi/fullOptJS` generates the API file
- [ ] Web client loads and functions correctly
- [ ] All CCS examples parse and execute correctly
- [ ] Spectroscopy algorithm produces same results as before

## References

- [Scala 2.13 Release Notes](https://github.com/scala/scala/releases/tag/v2.13.18)
- [Scala.js 1.x Migration Guide](https://www.scala-js.org/doc/project/migration.html)
- [sbt 1.x Migration Guide](https://www.scala-sbt.org/1.x/docs/Migrating-from-sbt-013x.html)

## Additional Notes

- The migration maintains backward compatibility with the existing API
- No changes to the algorithm implementation were necessary
- All language features used in the codebase are supported in Scala 2.13
- The project structure remains unchanged
