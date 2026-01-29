# Local CodeMirror Scala.js Fascade

## Overview
This document describes the local implementation of the CodeMirror Scala.js fascade that replaces the external `org.denigma:codemirror-facade` dependency.

## Files Created

### 1. Core Fascade - [js-client/src/main/scala-2.12/org/denigma/codemirror/Editor.scala](js-client/src/main/scala-2.12/org/denigma/codemirror/Editor.scala)

Contains the main Scala.js native traits and objects for CodeMirror interoperability:

**Key Types:**
- `Editor` - Main CodeMirror editor interface
- `Doc` - CodeMirror document object
- `LineInfo` - Information about a specific line
- `LineWidget` - Widget attached to a line
- `LineHandle` - Handle to a line
- `TextMarker` - Text marker in the editor
- `EditorConfiguration` - Configuration options for the editor
- `EditorChange` - Represents a change to the editor content
- `Position` / `PositionLike` - Position in the editor
- `CodeMirror` - Main CodeMirror object with factory methods

**Features:**
- Full native JS interop using `@js.native`
- Supports all major CodeMirror editor operations
- Line gutters and annotations support
- Text markers and widgets
- Event handlers for changes, gutterClick, etc.

### 2. Configuration Builder - [js-client/src/main/scala-2.12/org/denigma/codemirror/extensions/EditorConfig.scala](js-client/src/main/scala-2.12/org/denigma/codemirror/extensions/EditorConfig.scala)

Provides a fluent API for building CodeMirror editor configuration:

**Components:**
- `EditorConfig` - Object providing builder pattern configuration
- `EditorConfigurationBuilder` - Builder class with chainable methods
- `JSOptionBuilder` - Abstract base for building JavaScript option objects
- `OptMap` - Trait representing a JavaScript options map

**Available Methods:**
- `value()` - Set initial content
- `mode()` - Set language mode (e.g., "dces", "javascript")
- `theme()` - Set editor theme
- `lineNumbers()` - Enable/disable line numbers
- `gutters()` - Configure gutter names
- `lineWrapping()` - Enable/disable line wrapping
- `tabSize()` - Set tab size
- `indentUnit()` - Set indent unit
- And many more configuration options

**Usage Example:**
```scala
val cfg = EditorConfig
    .mode("dces")
    .lineNumbers(true)
    .gutters(js.Array("CodeMirror-linenumbers", PROBLEM_GUTTER))
CodeMirror.fromTextArea(editorNode, cfg)
```

### 3. Extensions and Implicits - [js-client/src/main/scala-2.12/org/denigma/codemirror/extensions/package.scala](js-client/src/main/scala-2.12/org/denigma/codemirror/extensions/package.scala)

Provides extension methods and implicit conversions:

**Classes:**
- `ExtendedEditor` - Adds convenience methods to Editor
  - `addOnGutterClick()` - Add gutter click handler
  - `addOnChange()` - Add change handler
  - `addOnChanges()` - Add multi-change handler
  - `lineText()` - Get text of a specific line
  - `linesText()` - Get text of multiple lines

- `ExtendedChange` - Utility methods for editor changes
  - `changedSpan` - Get span of changed lines
  - `newLines` - Map of new lines
  - `mergeSpans()` - Merge change spans

**Implicit Conversions:**
```scala
implicit def editorExtended(editor: Editor): ExtendedEditor
implicit def editorExtended(change: EditorChangeLike): ExtendedChange
```

## Integration with Scala.js 1.0

This fascade is compatible with Scala.js 1.0 and uses modern Scala.js features:

- **Native JS Interop:** Uses `@js.native` for zero-overhead JavaScript interop
- **Functional Programming:** Value classes and implicit conversions for functional style
- **ScalaJSDefined:** Custom Scala.js types like `EditorChangeLike` and `PositionLike`
- **Module Annotations:** Uses `@JSGlobal` for referencing global objects like `CodeMirror`

## Build Configuration

The `build.sbt` has been updated:

**Before:**
```scala
"org.denigma" %%% "codemirror-facade" % "5.22.0-0.8"
```

**After:**
```scala
// Fascade provided locally in js-client/src/main/scala-2.12/org/denigma/codemirror/
```

The `jsDependencies` are still used for runtime JavaScript:
```scala
jsDependencies ++= Seq(
  "org.webjars" % "codemirror" % "5.13" / "codemirror.js",
  // ...
)
```

## Benefits

1. **No External Dependency:** Fascade is part of the codebase
2. **Full Control:** Can be modified or extended as needed
3. **Scala.js 1.0 Compatible:** Uses modern Scala.js patterns
4. **Zero Runtime Overhead:** Native JS interop translates directly to JavaScript calls
5. **Type Safe:** Full Scala type checking for CodeMirror interactions

## Source Reference

This fascade is based on the open-source CodeMirror facade by Anton Kulaga:
- Repository: https://github.com/antonkulaga/codemirror-facade
- License: Check the original repository for license information

## Usage in the Codebase

The fascade is currently used in:
- [SourceEditor.scala](js-client/src/main/scala-2.12/io/equiv/eqfiddle/tool/view/SourceEditor.scala)

All imports from `org.denigma.codemirror` now resolve to the local fascade:
```scala
import org.denigma.codemirror.CodeMirror
import org.denigma.codemirror.Editor
import org.denigma.codemirror.LineWidget
import org.denigma.codemirror.extensions.EditorConfig
```

## Future Improvements

Possible enhancements to the fascade:

1. **Additional Modes:** Add support for more CodeMirror modes
2. **Themes:** Add theme selection methods
3. **Extensions:** Add wrappers for common CodeMirror extensions
4. **Documentation:** Add more detailed comments and examples
5. **Testing:** Add Scala.js unit tests for the fascade
