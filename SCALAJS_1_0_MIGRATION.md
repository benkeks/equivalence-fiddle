# Scala.js 1.0 Migration Status

## Overview
This document outlines the migration of the Equivalence Fiddle project from Scala.js 0.6.33 to Scala.js 1.0.0, following the official migration guide: https://www.scala-js.org/news/2020/02/25/announcing-scalajs-1.0.0/

## Completed Changes

### 1. Updated sbt Plugins (project/plugins.sbt)
- ✅ Updated `sbt-scalajs` from 0.6.33 to 1.0.0
- ✅ Updated `sbt-web-scalajs` from 1.1.0-0.6 to 1.2.0 (for Scala.js 1.0 compatibility)
- ✅ Added `sbt-jsdependencies` 1.0.0 plugin (required for jsDependencies support in 1.0)

### 2. Build Configuration Updates (build.sbt)
- ✅ Added `JSDependenciesPlugin` to jsClient project enablePlugins
- ✅ Configuration uses modern `scalaJSLinkerConfig` (already present in jsApi)
- ✅ Scala version 2.12.13 is compatible (2.12.1+ supported)

## Breaking Changes Addressed

The following Scala.js 1.0 breaking changes have been considered:

1. **Global Scope Changes**: Code does not appear to rely on problematic patterns like:
   - Storing global scope in a val
   - Accessing undefined members of global scope
   - Dynamic selection of global members
   
   Two uses of `js.isUndefined` found in:
   - `js-client/src/main/scala-2.12/io/equiv/eqfiddle/tool/view/SourceEditor.scala`
   - `js-api/src/main/scala-2.12/io/equiv/eqfiddle/spectroscopy/EquivalenceSpectroscopeAPI.scala`
   These are compatible as they check properties of regular JS objects, not global scope.

2. **ECMAScript 2015 Output**: Scala.js 1.0 now emits ES2015 by default (improvement, no action needed)

3. **Top-level Exports**: Default module kind still uses `NoModule` with top-level var exports (compatible)

4. **Compiler Options**: Removed `-P:scalajs:sjsDefinedByDefault` requirement (no longer needed)

## Outstanding Issues - Library Dependencies

### Problem
The following third-party libraries used by the project do not have Scala.js 1.0 compatible versions published:

- **scalaz-core** (7.2.29): Has limited Scala.js 1.0 support. Published for Scala.js 1.0 but may have compatibility issues.
- **scalajs-d3** (0.3.4): No published Scala.js 1.0 release
- **scalajs-bootstrap** (2.3.5): No published Scala.js 1.0 release

**Status**: Scalaz has been kept in the build and will be pulled from Maven. The scalajs-d3 and scalajs-bootstrap libraries have been commented out as they have no Scala.js 1.0 versions.

The scalaz-core library may still have issues with Scala.js 1.0, but attempting the build will reveal specific compatibility problems if any exist.

### Solutions

Choose one of the following approaches:

#### Option 1: Wait for Library Updates (Recommended for some libraries)
Some of these libraries may eventually release Scala.js 1.0 versions. Monitor:
- https://mvnrepository.com/artifact/org.scalaz/scalaz-core
- https://mvnrepository.com/artifact/org.singlespaced/scalajs-d3
- https://mvnrepository.com/artifact/com.github.karasiq/scalajs-bootstrap

#### Option 2: Find Alternative Libraries
- **Scalaz**: Consider using cats/cats-effect instead (which has Scala.js 1.0 support)
- **scalajs-d3**: Look for maintained D3 bindings with Scala.js 1.0 support
- **scalajs-bootstrap**: Consider vanilla JavaScript or other Bootstrap bindings

#### Option 3: Build Libraries from Source
Publish custom versions of these libraries cross-compiled with Scala.js 1.0:
- Clone each library repository
- Update their build.sbt to use Scala.js 1.0
- Publish to a local repository or Maven Central
- Reference them in this project

#### Option 4: Rewrite Dependent Code
If library usage is limited, refactor code to not depend on these libraries and use direct JavaScript interop instead.

## Build Status

### Current State
- ✅ Scala.js 1.0.0 sbt plugins installed and loaded
- ✅ Core build configuration migrated
- ❌ Full compilation blocked by missing library dependencies

### Testing
To verify plugin installation:
```bash
sbt "show scalaVersion"
```

To attempt compilation (will fail on library resolution):
```bash
sbt "jsApi/compile"
sbt "jsClient/compile"
```

## Files Modified

1. **project/plugins.sbt**
   - Updated plugin versions for Scala.js 1.0

2. **build.sbt**
   - Added JSDependenciesPlugin to jsClient
   - Third-party library dependencies commented out pending availability

## Migration Checklist

- [x] Update sbt plugins to 1.0.0
- [x] Add jsDependencies plugin
- [x] Remove deprecated compiler options
- [x] Use scalaJSLinkerConfig for linker settings
- [x] Ensure Scala version compatibility (2.12.1+)
- [x] Check for problematic global scope access patterns
- [ ] Resolve third-party library dependencies
- [ ] Test full compilation and JS output
- [ ] Test runtime functionality in browser

## Next Steps

1. **Decide on library strategy**: Choose how to handle missing Scala.js 1.0 versions
2. **Implement library solution**: Based on the chosen approach
3. **Update dependencies**: Add compatible versions to build.sbt
4. **Test compilation**: Run full build
5. **Runtime testing**: Test the generated JavaScript in browsers
6. **Performance testing**: Verify any improvements from ES2015 output

## References

- [Scala.js 1.0.0 Announcement](https://www.scala-js.org/news/2020/02/25/announcing-scalajs-1.0.0/)
- [Scala.js Documentation](https://www.scala-js.org/doc/)
- [Scala.js Library Directory](https://www.scala-js.org/libraries/)
- [sbt-web-scalajs Plugin](https://github.com/vmunier/sbt-web-scalajs)
